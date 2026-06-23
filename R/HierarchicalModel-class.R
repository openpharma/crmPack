#' @include Model-class.R
NULL

# HierarchicalModel helpers ----

#' Sanitize a Hierarchical Name for Generated JAGS Code
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Replaces every non-alphanumeric character with an underscore so that user
#' supplied arm and pool names can safely be embedded in generated JAGS
#' variable names.
#'
#' @param name (`string`)\cr the arm or pool name to sanitize.
#'
#' @return A character scalar suitable for generated JAGS identifiers.
#'
#' @keywords internal
h_hierarchical_safe_name <- function(name) {
  assert_string(name)
  gsub("[^A-Za-z0-9_]", "_", name)
}

#' Identify the Internal Type of a Hierarchical Arm Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Maps the currently supported hierarchical arm models to compact internal
#' labels used by the code generators.
#'
#' @param model (`GeneralModel`)\cr arm-specific model object.
#'
#' @return Either `"mono"` or `"combo"`.
#'
#' @keywords internal
h_hierarchical_model_type <- function(model) {
  if (is(model, "TwoDrugsCombo")) {
    return("combo")
  }
  if (h_hierarchical_is_single_model(model)) {
    return("mono")
  }
  stop("Unsupported hierarchical arm model.")
}

#' List Supported Exchangeable Parameter References for an Arm Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Returns the parameter reference syntax that may be used for a model in the
#' `exchangeable_parameters` argument of [HierarchicalModel()].
#'
#' @param model (`GeneralModel`)\cr arm-specific model object.
#'
#' @return Character vector of supported references.
#'
#' @keywords internal
h_hierarchical_supported_refs <- function(model) {
  if (is(model, "TwoDrugsCombo")) {
    return(as.character(unlist(lapply(
      seq_along(model@single_models),
      function(index) {
        paste0(model@single_models[[index]]@sample, "[", index, "]")
      }
    ))))
  }

  if (h_hierarchical_is_single_model(model)) {
    return(model@sample)
  }

  character()
}

#' Is a Compatible Hierarchical Single-Agent Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Uses the same structural compatibility contract as [TwoDrugsCombo()] for
#' single-agent binary outcome models.
#'
#' @param model (`GeneralModel`)\cr model object.
#'
#' @return A flag.
#'
#' @keywords internal
h_hierarchical_is_single_model <- function(model) {
  is(model, "GeneralModel") &&
    !is(model, "TwoDrugsCombo") &&
    setequal(model@datanames, c("nObs", "y", "x")) &&
    length(formalArgs(model@init)) == 0L &&
    setequal(
      setdiff(formalArgs(model@modelspecs), "from_prior"),
      character()
    )
}

#' Parse a Hierarchical Parameter Reference
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Converts a user-facing parameter reference into metadata that can be used by
#' the JAGS code generators.
#'
#' @param model (`GeneralModel`)\cr arm-specific model object.
#' @param arm_name (`string`)\cr the user-facing arm name.
#' @param ref (`string`)\cr parameter reference.
#'
#' @return Named list with entries `kind`, `index`, `latent`, and `sample`.
#'
#' @keywords internal
h_hierarchical_parse_ref <- function(model, arm_name, ref) {
  assert_string(ref)

  safe_arm <- h_hierarchical_safe_name(arm_name)
  type <- h_hierarchical_model_type(model)

  if (type == "mono" && ref %in% model@sample) {
    return(list(
      kind = ref,
      index = match(ref, model@sample),
      latent = h_hierarchical_reference_stochastic_node(
        model = model,
        arm_name = arm_name,
        ref = ref
      ),
      sample = paste0(ref, "_", safe_arm)
    ))
  }

  if (type == "combo") {
    m <- regexec("^(.+)\\[([0-9]+)\\]$", ref)
    capture <- regmatches(ref, m)[[1L]]
    if (length(capture) == 3L) {
      kind <- capture[2L]
      drug_index <- as.integer(capture[3L])
      if (
        drug_index >= 1L &&
          drug_index <= length(model@single_models) &&
          kind %in% model@single_models[[drug_index]]@sample
      ) {
        return(list(
          kind = kind,
          index = match(kind, model@single_models[[drug_index]]@sample),
          arm_index = drug_index,
          latent = h_hierarchical_reference_stochastic_node(
            model = model,
            arm_name = arm_name,
            ref = ref
          ),
          sample = paste0(kind, "_", safe_arm, "[", drug_index, "]")
        ))
      }
    }
  }

  stop(
    "Unsupported hierarchical parameter reference '",
    ref,
    "' for arm '",
    arm_name,
    "'."
  )
}

#' Create the Namespaced Expression for a Parameter Reference
#'
#' @inheritParams h_hierarchical_parse_ref
#'
#' @return A symbol or indexed call for the public sample node.
#'
#' @keywords internal
h_hierarchical_reference_expr <- function(model, arm_name, ref) {
  safe_arm <- h_hierarchical_safe_name(arm_name)

  if (h_hierarchical_is_single_model(model)) {
    return(as.name(paste0(ref, "_", safe_arm)))
  }

  m <- regexec("^(.+)\\[([0-9]+)\\]$", ref)
  capture <- regmatches(ref, m)[[1L]]
  assert_true(length(capture) == 3L)
  h_two_drugs_combo_indexed_call(
    paste0(capture[2L], "_", safe_arm),
    as.integer(capture[3L])
  )
}

#' Normalize an Expression Key
#'
#' @param expr (`language`)\cr expression to identify.
#'
#' @return A character scalar.
#'
#' @keywords internal
h_hierarchical_expr_key <- function(expr) {
  gsub("\\s+", "", deparse1(expr))
}

#' Find the Root Symbol of an Expression
#'
#' @param expr (`language`)\cr expression to inspect.
#'
#' @return Character vector of root symbols.
#'
#' @keywords internal
h_hierarchical_root_symbols <- function(expr) {
  h_two_drugs_combo_lhs_symbols(expr)
}

#' Collect Prior Relations
#'
#' @param expr (`language`)\cr prior model body.
#' @param operator (`string`)\cr relation operator, either `"~"` or `"<-"`.
#'
#' @return A list of relation calls.
#'
#' @keywords internal
h_hierarchical_prior_relations <- function(expr, operator) {
  relations <- list()
  if (is.call(expr)) {
    if (identical(as.character(expr[[1L]]), operator)) {
      relations <- list(expr)
    }
    relations <- c(
      relations,
      unlist(
        lapply(
          as.list(expr)[-1L],
          h_hierarchical_prior_relations,
          operator = operator
        ),
        recursive = FALSE
      )
    )
  }
  relations
}

#' Find Stochastic Subexpressions in an Expression
#'
#' @param expr (`language`)\cr expression to inspect.
#' @param stochastic_roots (`character`)\cr stochastic node roots.
#'
#' @return Named list of stochastic subexpressions keyed by expression text.
#'
#' @keywords internal
h_hierarchical_stochastic_subexpressions <- function(expr, stochastic_roots) {
  out <- list()
  if (is.symbol(expr)) {
    if (as.character(expr) %in% stochastic_roots) {
      out[[h_hierarchical_expr_key(expr)]] <- expr
    }
    return(out)
  }
  if (is.call(expr)) {
    if (
      identical(as.character(expr[[1L]]), "[") &&
        any(h_hierarchical_root_symbols(expr) %in% stochastic_roots)
    ) {
      out[[h_hierarchical_expr_key(expr)]] <- expr
      return(out)
    }
    for (arg in as.list(expr)[-1L]) {
      out <- c(out, h_hierarchical_stochastic_subexpressions(
        arg,
        stochastic_roots
      ))
    }
  }
  out[!duplicated(names(out))]
}

#' Infer the Stochastic Node for a Parameter Reference
#'
#' @inheritParams h_hierarchical_parse_ref
#'
#' @return Character scalar naming the stochastic node to pool.
#'
#' @keywords internal
h_hierarchical_reference_stochastic_node <- function(model, arm_name, ref) {
  prior_model <- h_hierarchical_namespace_model(
    model = model,
    arm_name = arm_name,
    slot_name = "priormodel"
  )
  stochastic <- h_hierarchical_prior_relations(body(prior_model), "~")
  deterministic <- h_hierarchical_prior_relations(body(prior_model), "<-")
  stochastic_roots <- unique(unlist(lapply(
    stochastic,
    function(expr) h_hierarchical_root_symbols(expr[[2L]])
  )))
  deterministic_map <- setNames(
    lapply(deterministic, function(expr) expr[[3L]]),
    vapply(
      deterministic,
      function(expr) h_hierarchical_expr_key(expr[[2L]]),
      character(1L)
    )
  )

  current <- h_hierarchical_reference_expr(model, arm_name, ref)
  for (step in seq_len(50L)) {
    current_roots <- h_hierarchical_root_symbols(current)
    if (length(current_roots) == 1L && current_roots %in% stochastic_roots) {
      return(h_hierarchical_expr_key(current))
    }

    current_key <- h_hierarchical_expr_key(current)
    if (!current_key %in% names(deterministic_map)) {
      break
    }

    rhs <- deterministic_map[[current_key]]
    stochastic_exprs <- h_hierarchical_stochastic_subexpressions(
      rhs,
      stochastic_roots
    )
    if (length(stochastic_exprs) == 1L) {
      return(names(stochastic_exprs))
    }
    if (length(stochastic_exprs) > 1L) {
      stop(
        "Parameter reference '",
        ref,
        "' for arm '",
        arm_name,
        "' depends on multiple stochastic nodes."
      )
    }
    current <- rhs
  }

  stop(
    "Could not infer a stochastic prior node for parameter reference '",
    ref,
    "' in arm '",
    arm_name,
    "'."
  )
}

#' Find Pooled Nodes for an Arm
#'
#' @param model (`GeneralModel`)\cr arm-specific model object.
#' @param arm_name (`string`)\cr hierarchical arm name.
#' @param refs (`character`)\cr hierarchical references.
#' @param pool_names (`character`)\cr pool names matching `refs`.
#'
#' @return Character vector of namespaced nodes with exchangeable priors.
#'
#' @keywords internal
h_hierarchical_pooled_nodes <- function(model, arm_name, refs, pool_names) {
  refs <- refs[pool_names != ""]
  if (length(refs) == 0L) {
    return(character())
  }
  vapply(
    refs,
    function(ref) h_hierarchical_parse_ref(model, arm_name, ref)$latent,
    character(1L)
  )
}

#' Remove Fixed Priors for Pooled Nodes
#'
#' @param model_fun (`function`)\cr namespaced prior model function.
#' @param pooled_nodes (`character`)\cr namespaced nodes with exchangeable priors.
#'
#' @return A function with fixed prior distribution statements removed.
#'
#' @keywords internal
h_hierarchical_remove_pooled_prior_lines <- function(model_fun, pooled_nodes) {
  if (length(pooled_nodes) == 0L) {
    return(model_fun)
  }
  pooled_roots <- unique(vapply(
    pooled_nodes,
    function(node) h_hierarchical_root_symbols(parse(text = node)[[1L]])[[1L]],
    character(1L)
  ))

  body_expr <- body(model_fun)
  if (!is.call(body_expr) || !identical(as.character(body_expr[[1L]]), "{")) {
    return(model_fun)
  }

  keep <- vapply(
    as.list(body_expr)[-1L],
    function(expr) {
      if (
        is.call(expr) &&
          identical(as.character(expr[[1L]]), "~") &&
          (
            h_hierarchical_expr_key(expr[[2L]]) %in% pooled_nodes ||
              any(h_hierarchical_root_symbols(expr[[2L]]) %in% pooled_roots)
          )
      ) {
        return(FALSE)
      }
      TRUE
    },
    logical(1L)
  )
  body(model_fun) <- as.call(c(list(as.name("{")), as.list(body_expr)[-1L][keep]))
  model_fun
}

#' Flatten Hierarchical Pool Definitions into a Lookup Table
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Builds a lookup keyed by `"arm::reference"` so the compiler can quickly tell
#' whether a parameter is pooled or fixed.
#'
#' @param parameter_pools (`list`)\cr exchangeable parameter specification from
#'   [HierarchicalModel()].
#'
#' @return Named list mapping `"arm::reference"` keys to pool names.
#'
#' @keywords internal
h_hierarchical_make_pool_map <- function(parameter_pools) {
  pooled_map <- list()

  for (pool_name in names(parameter_pools)) {
    members <- parameter_pools[[pool_name]]
    for (arm_name in names(members)) {
      this_key <- paste0(arm_name, "::", members[[arm_name]])
      if (!test_null(pooled_map[[this_key]])) {
        browser()
      }
      pooled_map[[this_key]] <- pool_name
    }
  }

  pooled_map
}

#' Find the Pool Name for One or More Parameter References
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Returns the hierarchical pool membership of each parameter reference, or an
#' empty string if the parameter keeps its arm-specific fixed prior.
#'
#' @param arm_name (`string`)\cr arm name.
#' @param refs (`character`)\cr parameter references belonging to that arm.
#' @param pooled_map (`list`)\cr output of
#'   [h_hierarchical_make_pool_map()].
#'
#' @return Character vector of pool names, using `""` for unpooled parameters.
#'
#' @keywords internal
h_hierarchical_pool_names <- function(arm_name, refs, pooled_map) {
  vapply(
    refs,
    function(ref) {
      key <- paste0(arm_name, "::", ref)
      if (is.null(pooled_map[[key]])) "" else pooled_map[[key]]
    },
    character(1L)
  )
}

#' Add an Arm Suffix to a Hierarchical Model Fragment
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Namespaces one arm-specific model fragment using code supplied by the arm
#' model class itself.
#'
#' @param model (`GeneralModel`)\cr arm-specific model object.
#' @param arm_name (`string`)\cr hierarchical arm name.
#' @param slot_name (`string`)\cr model-function slot to namespace.
#'
#' @return A function containing the namespaced JAGS model fragment.
#'
#' @keywords internal
h_hierarchical_namespace_model <- function(model, arm_name, slot_name) {
  safe_arm <- h_hierarchical_safe_name(arm_name)
  suffix <- paste0("_", safe_arm)
  model_fun <- slot(model, slot_name)
  prior_specs <- h_two_drugs_combo_single_model_specs(model, from_prior = TRUE)
  full_specs <- h_two_drugs_combo_single_model_specs(model, from_prior = FALSE)
  prior_inits <- do.call(model@init, list())
  prior_nodes <- h_two_drugs_combo_assigned_nodes(body(model@priormodel))
  data_nodes <- h_two_drugs_combo_assigned_nodes(body(model@datamodel))
  nodes <- setdiff(
    unique(c(
      names(prior_specs),
      names(full_specs),
      names(prior_inits),
      prior_nodes,
      data_nodes,
      model@sample,
      model@datanames
    )),
    character()
  )
  replacements <- setNames(
    lapply(nodes, function(name) as.name(paste0(name, suffix))),
    nodes
  )

  body(model_fun) <- h_two_drugs_combo_replace_symbols(
    body(model_fun),
    replacements
  )
  model_fun
}

#' Suffix Single-Arm Model Specifications
#'
#' @inheritParams h_hierarchical_namespace_model
#' @param from_prior (`flag`)\cr whether to request prior-only specifications.
#'
#' @return A named list of suffixed model specifications.
#'
#' @keywords internal
h_hierarchical_model_specs <- function(model, arm_name, from_prior) {
  specs <- h_two_drugs_combo_single_model_specs(model, from_prior = from_prior)
  h_two_drugs_combo_suffix_names(
    specs,
    paste0("_", h_hierarchical_safe_name(arm_name))
  )
}

#' Drop Fixed-Prior Specs for Fully Pooled Generic Parameters
#'
#' @param specs (`list`)\cr namespaced model specifications.
#' @param model (`GeneralModel`)\cr arm-specific model object.
#' @param arm_name (`string`)\cr hierarchical arm name.
#' @param refs (`character`)\cr supported hierarchical references.
#' @param pool_names (`character`)\cr pool names matching `refs`.
#'
#' @return Filtered `specs`.
#'
#' @keywords internal
h_hierarchical_filter_pooled_specs <- function(
  specs,
  model,
  arm_name,
  refs,
  pool_names
) {
  safe_arm <- h_hierarchical_safe_name(arm_name)
  pooled_refs <- refs[pool_names != ""]

  if (h_hierarchical_is_single_model(model)) {
    if (all(model@sample %in% pooled_refs)) {
      prior_specs <- h_two_drugs_combo_single_model_specs(
        model,
        from_prior = TRUE
      )
      remove_names <- paste0(names(prior_specs), "_", safe_arm)
      specs <- specs[!names(specs) %in% remove_names]
    }
    return(specs)
  }

  if (is(model, "TwoDrugsCombo")) {
    for (index in seq_along(model@single_models)) {
      single_model <- model@single_models[[index]]
      single_refs <- paste0(single_model@sample, "[", index, "]")
      if (all(single_refs %in% pooled_refs)) {
        prior_specs <- h_two_drugs_combo_single_model_specs(
          single_model,
          from_prior = TRUE
        )
        remove_names <- paste0(
          names(prior_specs),
          "_drug",
          index,
          "_",
          safe_arm
        )
        specs <- specs[!names(specs) %in% remove_names]
      }
    }
  }

  specs
}

#' Deparse a Model Body into JAGS Lines
#'
#' @inheritParams h_hierarchical_namespace_model
#'
#' @return Character vector containing the body expressions without braces.
#'
#' @keywords internal
h_hierarchical_model_body_lines <- function(model_fun) {
  body_lines <- deparse(body(model_fun))
  if (length(body_lines) > 1L && identical(body_lines[[1L]], "{")) {
    body_lines[-c(1L, length(body_lines))]
  } else {
    body_lines
  }
}

#' Compile the Hierarchical Data Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Concatenates the arm-specific likelihood code into a single JAGS model
#' function. Each arm receives a unique variable-name prefix derived from its
#' user-facing name.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#'
#' @return A function representing the compiled JAGS data model.
#'
#' @keywords internal
h_hierarchical_compile_datamodel <- function(models_to_arms) {
  Reduce(
    h_jags_join_models,
    lapply(names(models_to_arms), function(arm_name) {
      h_hierarchical_namespace_model(
        model = models_to_arms[[arm_name]],
        arm_name = arm_name,
        slot_name = "datamodel"
      )
    })
  )
}

#' Compile the Hierarchical Prior Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Builds the prior part of a hierarchical JAGS model. Unpooled parameters keep
#' their arm-specific fixed priors, while pooled parameters are linked through
#' exchangeable normal distributions with simple hyperpriors.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#' @param parameter_pools (`list`)\cr exchangeable parameter specification from
#'   [HierarchicalModel()].
#'
#' @return A list with entries `priormodel` and `sample`.
#'
#' @keywords internal
h_hierarchical_compile_priormodel <- function(models_to_arms, parameter_pools) {
  pooled_map <- h_hierarchical_make_pool_map(parameter_pools)
  fixed_lines <- character()
  sample_names <- character()

  for (arm_name in names(models_to_arms)) {
    model <- models_to_arms[[arm_name]]
    safe_arm <- h_hierarchical_safe_name(arm_name)

    model_refs <- h_hierarchical_supported_refs(model)
    pool_names <- h_hierarchical_pool_names(
      arm_name = arm_name,
      refs = model_refs,
      pooled_map = pooled_map
    )

    if (all(pool_names == "")) {
      fixed_lines <- c(
        fixed_lines,
        h_hierarchical_model_body_lines(h_hierarchical_namespace_model(
          model = model,
          arm_name = arm_name,
          slot_name = "priormodel"
        ))
      )
      sample_names <- c(
        sample_names,
        paste0(model@sample, "_", safe_arm)
      )
      next
    }

    pooled_nodes <- h_hierarchical_pooled_nodes(
      model = model,
      arm_name = arm_name,
      refs = model_refs,
      pool_names = pool_names
    )
    fixed_model <- h_hierarchical_namespace_model(
      model = model,
      arm_name = arm_name,
      slot_name = "priormodel"
    )
    fixed_model <- h_hierarchical_remove_pooled_prior_lines(
      model_fun = fixed_model,
      pooled_nodes = pooled_nodes
    )
    fixed_lines <- c(
      fixed_lines,
      h_hierarchical_model_body_lines(fixed_model)
    )
    sample_names <- c(
      sample_names,
      paste0(model@sample, "_", safe_arm)
    )
  }

  hyper_lines <- character()
  hyper_names <- character()
  for (pool_name in names(parameter_pools)) {
    members <- parameter_pools[[pool_name]]
    first_arm <- names(members)[1L]
    first_ref <- members[[1L]]
    first_info <- h_hierarchical_parse_ref(
      models_to_arms[[first_arm]],
      first_arm,
      first_ref
    )
    safe_pool <- h_hierarchical_safe_name(pool_name)
    mu_name <- paste0("mu_", safe_pool)
    tau_name <- paste0("tau_", safe_pool)

    # Each pooled arm-level latent parameter gets an exchangeable normal prior
    # centered on the pool-specific mean and SD.
    hyper_lines <- c(
      hyper_lines,
      vapply(
        seq_along(members),
        function(i) {
          arm_name <- names(members)[i]
          arm_ref <- members[[i]]
          arm_info <- h_hierarchical_parse_ref(
            models_to_arms[[arm_name]],
            arm_name,
            arm_ref
          )
          paste0(
            arm_info$latent,
            " ~ dnorm(",
            mu_name,
            ", pow(",
            tau_name,
            ", -2))"
          )
        },
        character(1L)
      )
    )

    # The first model parameter gets the default location-prior scale, while
    # later parameters get the default gradient-prior scale.
    if (identical(first_info$index, 1L)) {
      hyper_lines <- c(
        hyper_lines,
        paste0(mu_name, " ~ dnorm(logit(0.25), pow(2.5, -2))"),
        paste0(tau_name, " ~ dlnorm(log(0.5), pow(kappa_hier, -2))")
      )
    } else {
      hyper_lines <- c(
        hyper_lines,
        paste0(mu_name, " ~ dnorm(0, pow(0.7, -2))"),
        paste0(tau_name, " ~ dlnorm(log(0.25), pow(kappa_hier, -2))")
      )
    }

    hyper_names <- c(hyper_names, mu_name, tau_name)
  }

  list(
    priormodel = eval(parse(
      text = paste(
        c("function() {", fixed_lines, hyper_lines, "}"),
        collapse = "\n"
      )
    )),
    sample = unique(c(sample_names, hyper_names))
  )
}

#' Compile the Hierarchical Model-Specification Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Creates the `modelspecs` function used by [mcmc()] to prepare the JAGS data
#' needed by the dynamically compiled hierarchical model.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#' @param parameter_pools (`list`)\cr exchangeable parameter specification from
#'   [HierarchicalModel()].
#'
#' @return A function returning the JAGS data list.
#'
#' @keywords internal
h_hierarchical_compile_modelspecs <- function(models_to_arms, parameter_pools) {
  pooled_map <- h_hierarchical_make_pool_map(parameter_pools)

  function(arms, from_prior) {
    assert_list(arms, any.missing = FALSE)
    assert_flag(from_prior)

    specs <- list()
    if (length(parameter_pools) > 0L) {
      # Matches the prototype's log-normal hyper-SD parametrization.
      specs$kappa_hier <- log(2) / 1.96
    }

    for (arm_name in names(models_to_arms)) {
      model <- models_to_arms[[arm_name]]
      safe_arm <- h_hierarchical_safe_name(arm_name)
      model_refs <- h_hierarchical_supported_refs(model)
      pool_names <- h_hierarchical_pool_names(
        arm_name = arm_name,
        refs = model_refs,
        pooled_map = pooled_map
      )

      if (all(pool_names == "")) {
        specs <- c(
          specs,
          h_hierarchical_model_specs(
            model = model,
            arm_name = arm_name,
            from_prior = from_prior
          )
        )
        next
      }

      fixed_specs <- h_hierarchical_model_specs(
        model = model,
        arm_name = arm_name,
        from_prior = from_prior
      )
      fixed_specs <- h_hierarchical_filter_pooled_specs(
        specs = fixed_specs,
        model = model,
        arm_name = arm_name,
        refs = model_refs,
        pool_names = pool_names
      )
      specs <- c(
        specs,
        fixed_specs
      )
    }

    specs
  }
}

#' Compile the Hierarchical Initial-Value Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Wraps the per-arm `init` functions and renames their outputs so they match
#' the dynamically generated hierarchical JAGS variable names.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#' @param parameter_pools (`list`)\cr exchangeable parameter specification from
#'   [HierarchicalModel()].
#'
#' @return A function returning a named list of JAGS initial values.
#'
#' @keywords internal
h_hierarchical_compile_init <- function(models_to_arms, parameter_pools) {
  function(arms) {
    assert_list(arms, any.missing = FALSE)

    init <- list()
    for (arm_name in names(models_to_arms)) {
      model <- models_to_arms[[arm_name]]
      safe_arm <- h_hierarchical_safe_name(arm_name)
      arm_inits <- do.call(model@init, list())

      # Hierarchical compilation prefixes arm-local initial values by arm.
      for (init_name in names(arm_inits)) {
        init[[paste0(init_name, "_", safe_arm)]] <- arm_inits[[init_name]]
      }
    }

    for (pool_name in names(parameter_pools)) {
      safe_pool <- h_hierarchical_safe_name(pool_name)
      init[[paste0("mu_", safe_pool)]] <- 0
      init[[paste0("tau_", safe_pool)]] <- 0.5
    }

    init
  }
}

# HierarchicalModel ----

## class ----

#' `HierarchicalModel`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`HierarchicalModel`] is a class for the prototype hierarchical model that
#' links compatible single-agent binary outcome arms and/or [`TwoDrugsCombo`]
#' arms.
#'
#' @details The class currently stores the structural pieces from the design
#'   prototype as a named list of arm-specific models and a named list of
#'   exchangeable parameter pools used to dynamically compile a joint JAGS model.
#'
#' @slot models_to_arms (`list`)\cr named list of arm-specific models. Each
#'   entry must be either a compatible single-agent binary outcome
#'   [`GeneralModel`] object using `nObs`, `y`, and `x` inputs, or a
#'   [`TwoDrugsCombo`] object.
#' @slot parameter_pools (`list`)\cr named list describing which parameters are
#'   exchangeable across arms. Each list entry contains arm names as names and
#'   parameter references as values.
#'
#' @seealso [`HierarchicalData`], [`TwoDrugsCombo`].
#'
#' @aliases HierarchicalModel
#' @export
#'
.HierarchicalModel <- setClass(
  Class = "HierarchicalModel",
  contains = "GeneralModel",
  slots = c(
    models_to_arms = "list",
    parameter_pools = "list"
  ),
  validity = v_hierarchical_model
)

## constructor ----

#' @rdname HierarchicalModel-class
#'
#' @param ... named model objects describing the trial arms.
#' @param exchangeable_parameters a named list describing
#'   which parameters are exchangeable across arms. This will be
#'   used to define the hierarchical structure of the model. Each
#'   list entry contains the arms as names and the parameters to be shared
#'   as a string.
#'
#' @export
#' @example examples/Model-class-HierarchicalModel.R
HierarchicalModel <- function(
  ...,
  exchangeable_parameters = list()
) {
  args <- list(...)
  assert_list(args, any.missing = FALSE, min.len = 2L)
  assert_true(!is.null(names(args)))
  assert_character(names(args), unique = TRUE, any.missing = FALSE)
  assert_true(all(vapply(
    args,
    function(model) {
      h_hierarchical_is_single_model(model) || is(model, "TwoDrugsCombo")
    },
    logical(1L)
  )))
  assert_list(exchangeable_parameters, any.missing = FALSE, null.ok = TRUE)
  assert_character(sapply(names(args), h_hierarchical_safe_name), unique = TRUE)
  if (length(exchangeable_parameters) > 0L) {
    assert_character(
      sapply(names(exchangeable_parameters), h_hierarchical_safe_name),
      unique = TRUE
    )
  }

  compiled_datamodel <- h_hierarchical_compile_datamodel(args)
  compiled_prior <- h_hierarchical_compile_priormodel(
    models_to_arms = args,
    parameter_pools = exchangeable_parameters
  )

  .HierarchicalModel(
    models_to_arms = args,
    parameter_pools = exchangeable_parameters,
    datamodel = compiled_datamodel,
    priormodel = compiled_prior$priormodel,
    modelspecs = h_hierarchical_compile_modelspecs(
      models_to_arms = args,
      parameter_pools = exchangeable_parameters
    ),
    init = h_hierarchical_compile_init(
      models_to_arms = args,
      parameter_pools = exchangeable_parameters
    ),
    datanames = "arms",
    datanames_prior = "arms",
    sample = compiled_prior$sample
  )
}

## default constructor ----

#' @rdname HierarchicalModel-class
#' @note Typically, end users will not use the `.DefaultHierarchicalModel()`
#'   function directly.
#' @export
.DefaultHierarchicalModel <- function() {
  mono_model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 10
  )
  combo_model <- .DefaultTwoDrugsCombo()
  first_ref <- mono_model@sample[[1L]]
  second_ref <- mono_model@sample[[2L]]

  HierarchicalModel(
    mono = mono_model,
    combo = combo_model,
    exchangeable_parameters = list(
      mono_intercept = list(
        mono = first_ref,
        combo = paste0(first_ref, "[1]")
      ),
      mono_slope = list(
        mono = second_ref,
        combo = paste0(second_ref, "[1]")
      )
    )
  )
}
