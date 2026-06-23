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
  if (is(model, "LogisticLogNormal")) {
    return("mono")
  }
  if (is(model, "TwoDrugsCombo")) {
    return("combo")
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
  if (is(model, "LogisticLogNormal")) {
    return(c("alpha0", "alpha1"))
  }

  if (is(model, "TwoDrugsCombo")) {
    return(c("alpha0[1]", "alpha1[1]", "alpha0[2]", "alpha1[2]"))
  }

  character()
}

#' Parse a Hierarchical Parameter Reference
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Converts a user-facing parameter reference such as `"alpha0"` or
#' `"alpha1[2]"` into metadata that can be used by the JAGS code generators.
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

  if (type == "mono") {
    if (identical(ref, "alpha0")) {
      return(list(
        kind = "alpha0",
        index = 1L,
        latent = paste0("theta_", safe_arm, "[1]"),
        sample = paste0("alpha0_", safe_arm)
      ))
    }

    if (identical(ref, "alpha1")) {
      return(list(
        kind = "alpha1",
        index = 2L,
        latent = paste0("theta_", safe_arm, "[2]"),
        sample = paste0("alpha1_", safe_arm)
      ))
    }
  }

  if (type == "combo") {
    m <- regexec("^alpha([01])\\[([12])\\]$", ref)
    capture <- regmatches(ref, m)[[1L]]
    if (length(capture) == 3L) {
      kind <- paste0("alpha", capture[2L])
      index <- as.integer(capture[3L])
      return(list(
        kind = kind,
        index = index,
        latent = paste0(
          "theta_",
          safe_arm,
          "[",
          if (kind == "alpha0") 1L else 2L,
          ", ",
          index,
          "]"
        ),
        sample = paste0(kind, "_", safe_arm, "[", index, "]")
      ))
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
  lines <- unlist(lapply(names(models_to_arms), function(arm_name) {
    model <- models_to_arms[[arm_name]]
    safe_arm <- h_hierarchical_safe_name(arm_name)

    if (is(model, "LogisticLogNormal")) {
      # Mono arms contribute the standard single-agent logistic likelihood.
      return(c(
        paste0("for (i in 1:nObs_", safe_arm, ") {"),
        paste0(
          "  logit(p_",
          safe_arm,
          "[i]) <- alpha0_",
          safe_arm,
          " + alpha1_",
          safe_arm,
          " * log(x_",
          safe_arm,
          "[i] / ref_dose_",
          safe_arm,
          ")"
        ),
        paste0("  y_", safe_arm, "[i] ~ dbern(p_", safe_arm, "[i])"),
        "}"
      ))
    }

    # Combination arms reuse the existing two-drug probability construction.
    c(
      paste0("for (i in 1:nObs_", safe_arm, ") {"),
      "  for (j in 1:2) {",
      paste0(
        "    logit(p_single_",
        safe_arm,
        "[i, j]) <- alpha0_",
        safe_arm,
        "[j] + alpha1_",
        safe_arm,
        "[j] * log(x_",
        safe_arm,
        "[i, j] / ref_dose_",
        safe_arm,
        "[j])"
      ),
      "  }",
      paste0(
        "  p0_",
        safe_arm,
        "[i] <- p_single_",
        safe_arm,
        "[i, 1] + ",
        "p_single_",
        safe_arm,
        "[i, 2] - ",
        "p_single_",
        safe_arm,
        "[i, 1] * p_single_",
        safe_arm,
        "[i, 2]"
      ),
      paste0(
        "  logit(p_",
        safe_arm,
        "[i]) <- log(p0_",
        safe_arm,
        "[i] / ",
        "(1 - p0_",
        safe_arm,
        "[i])) + eta_",
        safe_arm,
        " * (x_",
        safe_arm,
        "[i, 1] / ref_dose_",
        safe_arm,
        "[1])",
        " * (x_",
        safe_arm,
        "[i, 2] / ref_dose_",
        safe_arm,
        "[2])"
      ),
      paste0("  y_", safe_arm, "[i] ~ dbern(p_", safe_arm, "[i])"),
      "}"
    )
  }))

  eval(parse(text = paste(c("function() {", lines, "}"), collapse = "\n")))
}

#' Detect the Parallel Mono/Combo Hierarchical Layout
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Identifies the current MAC-prior prototype where one two-drug combination
#' arm is linked to two monotherapy arms, one per combo drug.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#' @param parameter_pools (`list`)\cr exchangeable parameter specification.
#'
#' @return A named list describing the layout, or `NULL` if the input does not
#'   match the complete parallel mono/combo structure.
#'
#' @keywords internal
h_hierarchical_parallel_combo_structure <- function(
  models_to_arms,
  parameter_pools
) {
  if (length(parameter_pools) != 4L) {
    return(NULL)
  }

  combo_arms <- names(models_to_arms)[vapply(
    models_to_arms,
    function(model) is(model, "TwoDrugsCombo"),
    logical(1L)
  )]
  if (length(combo_arms) != 1L) {
    return(NULL)
  }

  combo_arm <- combo_arms[[1L]]
  mono_arms <- names(models_to_arms)[vapply(
    models_to_arms,
    function(model) is(model, "LogisticLogNormal"),
    logical(1L)
  )]
  combo_model <- models_to_arms[[combo_arm]]

  find_pool <- function(combo_ref, mono_ref) {
    matches <- list()
    for (pool_name in names(parameter_pools)) {
      pool <- parameter_pools[[pool_name]]
      if (!is.list(pool) || length(pool) != 2L || is.null(names(pool))) {
        next
      }
      if (!identical(pool[[combo_arm]], combo_ref)) {
        next
      }

      mono_hits <- mono_arms[vapply(
        mono_arms,
        function(mono_arm) identical(pool[[mono_arm]], mono_ref),
        logical(1L)
      )]
      if (length(mono_hits) == 1L) {
        matches[[length(matches) + 1L]] <- list(
          pool_name = pool_name,
          mono_arm = mono_hits[[1L]]
        )
      }
    }

    if (length(matches) == 1L) {
      matches[[1L]]
    } else {
      NULL
    }
  }

  blocks <- vector("list", 2L)
  used_pools <- character()
  used_mono_arms <- character()

  for (j in 1:2) {
    intercept_pool <- find_pool(paste0("alpha0[", j, "]"), "alpha0")
    slope_pool <- find_pool(paste0("alpha1[", j, "]"), "alpha1")
    if (
      is.null(intercept_pool) ||
        is.null(slope_pool) ||
        !identical(intercept_pool$mono_arm, slope_pool$mono_arm)
    ) {
      return(NULL)
    }

    block_name <- h_hierarchical_safe_name(combo_model@drug_names[[j]])
    blocks[[j]] <- list(
      index = j,
      mono_arm = intercept_pool$mono_arm,
      block_name = block_name,
      intercept_pool = intercept_pool$pool_name,
      slope_pool = slope_pool$pool_name
    )
    used_pools <- c(used_pools, intercept_pool$pool_name, slope_pool$pool_name)
    used_mono_arms <- c(used_mono_arms, intercept_pool$mono_arm)
  }

  if (
    anyDuplicated(used_pools) ||
      anyDuplicated(used_mono_arms) ||
      !setequal(names(parameter_pools), used_pools)
  ) {
    return(NULL)
  }

  list(combo_arm = combo_arm, blocks = blocks)
}

#' Compile the Parallel Mono/Combo MAC Prior Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Builds the block-diagonal MAC prior used for the complete parallel
#' monotherapy and combination-arm prototype.
#'
#' @param models_to_arms (`list`)\cr named arm-specific models.
#' @param structure (`list`)\cr output from
#'   [h_hierarchical_parallel_combo_structure()].
#'
#' @return A list with entries `priormodel` and `sample`.
#'
#' @keywords internal
h_hierarchical_compile_parallel_combo_priormodel <- function(
  models_to_arms,
  structure
) {
  combo_arm <- structure$combo_arm
  combo_safe <- h_hierarchical_safe_name(combo_arm)
  combo_model <- models_to_arms[[combo_arm]]
  prior_lines <- character()
  hyper_lines <- character()
  assign_lines <- character()
  sample_names <- character()
  hyper_names <- character()

  for (block in structure$blocks) {
    j <- block$index
    safe_mono <- h_hierarchical_safe_name(block$mono_arm)
    safe_block <- block$block_name
    mu_vec <- paste0("mu_", safe_block)
    prec_mat <- paste0("prec_", safe_block)
    rho_name <- paste0("rho_", safe_block)
    intercept_name <- paste0(
      "mu_",
      h_hierarchical_safe_name(block$intercept_pool)
    )
    slope_name <- paste0("mu_", h_hierarchical_safe_name(block$slope_pool))
    tau_intercept_name <- paste0(
      "tau_",
      h_hierarchical_safe_name(block$intercept_pool)
    )
    tau_slope_name <- paste0("tau_", h_hierarchical_safe_name(block$slope_pool))
    intercept_tau_median <- if (j == 1L) "0.5" else "0.75"
    slope_mu_prior <- if (j == 1L) {
      paste0(slope_name, " ~ dnorm(0, pow(0.7, -2))")
    } else {
      paste0(slope_name, " ~ dnorm(0, 1)")
    }

    prior_lines <- c(
      prior_lines,
      paste0(
        "theta_",
        safe_mono,
        "[1:2] ~ dmnorm(",
        mu_vec,
        "[1:2], ",
        prec_mat,
        "[1:2, 1:2])"
      ),
      paste0(
        "theta_",
        combo_safe,
        "[1:2, ",
        j,
        "] ~ dmnorm(",
        mu_vec,
        "[1:2], ",
        prec_mat,
        "[1:2, 1:2])"
      )
    )

    hyper_lines <- c(
      hyper_lines,
      paste0(intercept_name, " ~ dnorm(logit(0.25), pow(2.5, -2))"),
      slope_mu_prior,
      paste0(
        tau_intercept_name,
        " ~ dlnorm(log(",
        intercept_tau_median,
        "), pow(kappa_hier, -2))"
      ),
      paste0(tau_slope_name, " ~ dlnorm(log(0.25), pow(kappa_hier, -2))"),
      paste0(rho_name, " ~ dunif(-1, 1)"),
      paste0(mu_vec, "[1] <- ", intercept_name),
      paste0(mu_vec, "[2] <- ", slope_name),
      paste0(
        prec_mat,
        "[1, 1] <- 1 / (pow(",
        tau_intercept_name,
        ", 2) * (1 - pow(",
        rho_name,
        ", 2)))"
      ),
      paste0(
        prec_mat,
        "[2, 2] <- 1 / (pow(",
        tau_slope_name,
        ", 2) * (1 - pow(",
        rho_name,
        ", 2)))"
      ),
      paste0(
        prec_mat,
        "[1, 2] <- -",
        rho_name,
        " / (",
        tau_intercept_name,
        " * ",
        tau_slope_name,
        " * (1 - pow(",
        rho_name,
        ", 2)))"
      ),
      paste0(prec_mat, "[2, 1] <- ", prec_mat, "[1, 2]")
    )

    hyper_names <- c(
      hyper_names,
      intercept_name,
      slope_name,
      tau_intercept_name,
      tau_slope_name,
      rho_name
    )
  }

  eta_mu_name <- paste0("mu_eta_", combo_safe)
  eta_tau_name <- paste0("tau_eta_", combo_safe)
  if (combo_model@log_normal_eta) {
    prior_lines <- c(
      prior_lines,
      paste0(
        "log_eta_",
        combo_safe,
        " ~ dnorm(",
        eta_mu_name,
        ", pow(",
        eta_tau_name,
        ", -2))"
      )
    )
    assign_lines <- c(
      assign_lines,
      paste0("eta_", combo_safe, " <- exp(log_eta_", combo_safe, ")")
    )
  } else {
    prior_lines <- c(
      prior_lines,
      paste0(
        "eta_",
        combo_safe,
        " ~ dnorm(",
        eta_mu_name,
        ", pow(",
        eta_tau_name,
        ", -2))"
      )
    )
  }
  hyper_lines <- c(
    hyper_lines,
    paste0(eta_mu_name, " ~ dnorm(0, 1)"),
    paste0(eta_tau_name, " ~ dlnorm(log(0.125), pow(kappa_hier, -2))")
  )
  hyper_names <- c(hyper_names, eta_mu_name, eta_tau_name)

  for (arm_name in names(models_to_arms)) {
    model <- models_to_arms[[arm_name]]
    safe_arm <- h_hierarchical_safe_name(arm_name)
    if (is(model, "LogisticLogNormal")) {
      assign_lines <- c(
        assign_lines,
        paste0("alpha0_", safe_arm, " <- theta_", safe_arm, "[1]"),
        paste0("alpha1_", safe_arm, " <- exp(theta_", safe_arm, "[2])")
      )
      sample_names <- c(
        sample_names,
        paste0("alpha0_", safe_arm),
        paste0("alpha1_", safe_arm)
      )
    } else {
      assign_lines <- c(
        assign_lines,
        paste0("alpha0_", safe_arm, "[1] <- theta_", safe_arm, "[1, 1]"),
        paste0("alpha1_", safe_arm, "[1] <- exp(theta_", safe_arm, "[2, 1])"),
        paste0("alpha0_", safe_arm, "[2] <- theta_", safe_arm, "[1, 2]"),
        paste0("alpha1_", safe_arm, "[2] <- exp(theta_", safe_arm, "[2, 2])")
      )
      sample_names <- c(
        sample_names,
        paste0("alpha0_", safe_arm),
        paste0("alpha1_", safe_arm),
        paste0("eta_", safe_arm)
      )
    }
  }

  list(
    priormodel = eval(parse(
      text = paste(
        c("function() {", prior_lines, hyper_lines, assign_lines, "}"),
        collapse = "\n"
      )
    )),
    sample = unique(c(sample_names, hyper_names))
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
  parallel_combo_structure <- h_hierarchical_parallel_combo_structure(
    models_to_arms = models_to_arms,
    parameter_pools = parameter_pools
  )
  if (!is.null(parallel_combo_structure)) {
    return(h_hierarchical_compile_parallel_combo_priormodel(
      models_to_arms = models_to_arms,
      structure = parallel_combo_structure
    ))
  }

  pooled_map <- h_hierarchical_make_pool_map(parameter_pools)
  fixed_lines <- character()
  assign_lines <- character()
  sample_names <- character()

  for (arm_name in names(models_to_arms)) {
    model <- models_to_arms[[arm_name]]
    safe_arm <- h_hierarchical_safe_name(arm_name)

    if (is(model, "LogisticLogNormal")) {
      pool_names <- h_hierarchical_pool_names(
        arm_name = arm_name,
        refs = c("alpha0", "alpha1"),
        pooled_map = pooled_map
      )

      if (all(pool_names == "")) {
        # If neither mono parameter is pooled we can keep the original
        # bivariate normal prior unchanged.
        fixed_lines <- c(
          fixed_lines,
          paste0(
            "theta_",
            safe_arm,
            "[1:2] ~ dmnorm(mean_",
            safe_arm,
            "[1:2], prec_",
            safe_arm,
            "[1:2, 1:2])"
          )
        )
      } else {
        for (idx in seq_along(pool_names)) {
          if (pool_names[idx] == "") {
            fixed_lines <- c(
              fixed_lines,
              paste0(
                "theta_",
                safe_arm,
                "[",
                idx,
                "] ~ dnorm(",
                "marginal_mean_",
                safe_arm,
                "[",
                idx,
                "], ",
                "marginal_prec_",
                safe_arm,
                "[",
                idx,
                "])"
              )
            )
          }
        }
      }

      # The generated alpha-parameters always use the arm-prefixed latent theta.
      assign_lines <- c(
        assign_lines,
        paste0("alpha0_", safe_arm, " <- theta_", safe_arm, "[1]"),
        paste0("alpha1_", safe_arm, " <- exp(theta_", safe_arm, "[2])")
      )
      sample_names <- c(
        sample_names,
        paste0("alpha0_", safe_arm),
        paste0("alpha1_", safe_arm)
      )
    } else {
      for (j in 1:2) {
        pool_names <- h_hierarchical_pool_names(
          arm_name = arm_name,
          refs = c(paste0("alpha0[", j, "]"), paste0("alpha1[", j, "]")),
          pooled_map = pooled_map
        )

        if (all(pool_names == "")) {
          fixed_lines <- c(
            fixed_lines,
            paste0(
              "theta_",
              safe_arm,
              "[1:2, ",
              j,
              "] ~ dmnorm(",
              "prior_mean_",
              safe_arm,
              "[1:2, ",
              j,
              "], ",
              "prior_prec_",
              safe_arm,
              "[1:2, 1:2, ",
              j,
              "])"
            )
          )
        } else {
          for (idx in 1:2) {
            if (pool_names[idx] == "") {
              fixed_lines <- c(
                fixed_lines,
                paste0(
                  "theta_",
                  safe_arm,
                  "[",
                  idx,
                  ", ",
                  j,
                  "] ~ dnorm(",
                  "marginal_mean_",
                  safe_arm,
                  "[",
                  idx,
                  ", ",
                  j,
                  "], ",
                  "marginal_prec_",
                  safe_arm,
                  "[",
                  idx,
                  ", ",
                  j,
                  "])"
                )
              )
            }
          }
        }
      }

      # Eta always remains arm-specific in the current prototype.
      if (model@log_normal_eta) {
        fixed_lines <- c(
          fixed_lines,
          paste0(
            "log_eta_",
            safe_arm,
            " ~ dnorm(gamma_",
            safe_arm,
            ", tau_",
            safe_arm,
            ")"
          ),
          paste0("eta_", safe_arm, " <- exp(log_eta_", safe_arm, ")")
        )
      } else {
        fixed_lines <- c(
          fixed_lines,
          paste0(
            "eta_",
            safe_arm,
            " ~ dnorm(gamma_",
            safe_arm,
            ", tau_",
            safe_arm,
            ")"
          )
        )
      }

      assign_lines <- c(
        assign_lines,
        paste0("alpha0_", safe_arm, "[1] <- theta_", safe_arm, "[1, 1]"),
        paste0("alpha1_", safe_arm, "[1] <- exp(theta_", safe_arm, "[2, 1])"),
        paste0("alpha0_", safe_arm, "[2] <- theta_", safe_arm, "[1, 2]"),
        paste0("alpha1_", safe_arm, "[2] <- exp(theta_", safe_arm, "[2, 2])")
      )
      sample_names <- c(
        sample_names,
        paste0("alpha0_", safe_arm),
        paste0("alpha1_", safe_arm),
        paste0("eta_", safe_arm)
      )
    }
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

    # We keep separate default hyperpriors for intercept-like vs slope-like
    # parameters to mirror the prototype assumptions.
    if (first_info$kind == "alpha0") {
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
        c("function() {", fixed_lines, hyper_lines, assign_lines, "}"),
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
  parallel_combo_structure <- h_hierarchical_parallel_combo_structure(
    models_to_arms = models_to_arms,
    parameter_pools = parameter_pools
  )
  parallel_combo_arm <- if (is.null(parallel_combo_structure)) {
    NA_character_
  } else {
    parallel_combo_structure$combo_arm
  }

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

      if (is(model, "LogisticLogNormal")) {
        pool_names <- h_hierarchical_pool_names(
          arm_name = arm_name,
          refs = c("alpha0", "alpha1"),
          pooled_map = pooled_map
        )

        if (all(pool_names == "")) {
          specs[[paste0("mean_", safe_arm)]] <- model@params@mean
          specs[[paste0("prec_", safe_arm)]] <- model@params@prec
        } else if (any(pool_names == "")) {
          # When only one mono parameter is pooled, the unpooled parameter keeps
          # a simple marginal normal prior.
          specs[[paste0("marginal_mean_", safe_arm)]] <- model@params@mean
          specs[[paste0("marginal_prec_", safe_arm)]] <- 1 /
            diag(model@params@cov)
        }

        if (!from_prior) {
          specs[[paste0("ref_dose_", safe_arm)]] <- as.numeric(model@ref_dose)
        }
      } else {
        prior_mean <- do.call(
          cbind,
          lapply(model@single_models, function(single_model) {
            single_model@params@mean
          })
        )
        prior_prec <- array(
          data = do.call(
            c,
            lapply(model@single_models, function(single_model) {
              single_model@params@prec
            })
          ),
          dim = c(2, 2, 2)
        )
        marginal_mean <- do.call(
          cbind,
          lapply(model@single_models, function(single_model) {
            single_model@params@mean
          })
        )
        marginal_prec <- do.call(
          cbind,
          lapply(
            model@single_models,
            function(single_model) 1 / diag(single_model@params@cov)
          )
        )
        needs_joint <- logical(2L)
        needs_marginal <- logical(2L)

        for (j in 1:2) {
          pool_names <- h_hierarchical_pool_names(
            arm_name = arm_name,
            refs = c(paste0("alpha0[", j, "]"), paste0("alpha1[", j, "]")),
            pooled_map = pooled_map
          )
          needs_joint[j] <- all(pool_names == "")
          needs_marginal[j] <- any(pool_names == "") && !all(pool_names == "")
        }

        if (any(needs_joint)) {
          specs[[paste0("prior_mean_", safe_arm)]] <- prior_mean
          specs[[paste0("prior_prec_", safe_arm)]] <- prior_prec
        }
        if (any(needs_marginal)) {
          specs[[paste0("marginal_mean_", safe_arm)]] <- marginal_mean
          specs[[paste0("marginal_prec_", safe_arm)]] <- marginal_prec
        }
        if (!identical(arm_name, parallel_combo_arm)) {
          specs[[paste0("gamma_", safe_arm)]] <- model@gamma
          specs[[paste0("tau_", safe_arm)]] <- model@tau
        }
        if (!from_prior) {
          specs[[paste0("ref_dose_", safe_arm)]] <- unname(model@ref_dose)
        }
      }
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
  parallel_combo_structure <- h_hierarchical_parallel_combo_structure(
    models_to_arms = models_to_arms,
    parameter_pools = parameter_pools
  )

  function(arms) {
    assert_list(arms, any.missing = FALSE)

    init <- list()
    for (arm_name in names(models_to_arms)) {
      model <- models_to_arms[[arm_name]]
      safe_arm <- h_hierarchical_safe_name(arm_name)
      arm_inits <- do.call(model@init, list())

      # Hierarchical compilation prefixes latent variable names by arm.
      if ("theta" %in% names(arm_inits)) {
        init[[paste0("theta_", safe_arm)]] <- arm_inits$theta
      }
      if ("eta" %in% names(arm_inits)) {
        init[[paste0("eta_", safe_arm)]] <- arm_inits$eta
      }
      if ("log_eta" %in% names(arm_inits)) {
        init[[paste0("log_eta_", safe_arm)]] <- arm_inits$log_eta
      }
    }

    for (pool_name in names(parameter_pools)) {
      safe_pool <- h_hierarchical_safe_name(pool_name)
      init[[paste0("mu_", safe_pool)]] <- 0
      init[[paste0("tau_", safe_pool)]] <- 0.5
    }

    if (!is.null(parallel_combo_structure)) {
      combo_safe <- h_hierarchical_safe_name(parallel_combo_structure$combo_arm)
      for (block in parallel_combo_structure$blocks) {
        init[[paste0("rho_", block$block_name)]] <- 0
      }
      init[[paste0("mu_eta_", combo_safe)]] <- 0
      init[[paste0("tau_eta_", combo_safe)]] <- 0.125
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
#' links a single-agent [`LogisticLogNormal`] arm with a
#' [`TwoDrugsCombo`] arm.
#'
#' @details The class currently stores the structural pieces from the design
#'   prototype as a named list of arm-specific models and a named list of
#'   exchangeable parameter pools used to dynamically compile a joint JAGS model.
#'
#' @slot models_to_arms (`list`)\cr named list of arm-specific models. Each
#'   entry must currently be either a [`LogisticLogNormal`] or a
#'   [`TwoDrugsCombo`] object.
#' @slot parameter_pools (`list`)\cr named list describing which parameters are
#'   exchangeable across arms. Each list entry contains arm names as names and
#'   parameter references as values, e.g. `"alpha0"` or `"alpha0[1]"`.
#'
#' @seealso [`HierarchicalData`], [`LogisticLogNormal`],
#'   [`TwoDrugsCombo`].
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
      is(model, "LogisticLogNormal") || is(model, "TwoDrugsCombo")
    },
    logical(1L)
  )))
  assert_list(exchangeable_parameters, any.missing = FALSE, null.ok = TRUE)
  assert_character(sapply(names(args), h_hierarchical_safe_name), unique = TRUE)
  assert_character(
    sapply(names(exchangeable_parameters), h_hierarchical_safe_name),
    unique = TRUE
  )

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
  HierarchicalModel(
    mono = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    combo = .DefaultTwoDrugsCombo(),
    exchangeable_parameters = list(
      mono_intercept = list(
        mono = "alpha0",
        combo = "alpha0[1]"
      ),
      mono_slope = list(
        mono = "alpha1",
        combo = "alpha1[1]"
      )
    )
  )
}
