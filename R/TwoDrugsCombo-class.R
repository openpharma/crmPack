#' @include Model-class.R
NULL

# TwoDrugsCombo ----

## class ----

#' `TwoDrugsCombo`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`TwoDrugsCombo`] is the class for a two-drug combination regression model with
#' fixed priors for the two single-agent dose-toxicity models and an additional
#' interaction parameter.
#'
#' @details Let \eqn{p(x_1, x_2)} be the probability of DLT at the dose
#' combination \eqn{(x_1, x_2)}. The model combines two single-agent models
#' with an interaction term:
#' \deqn{\textrm{odds}(p(x_1, x_2)) = \textrm{odds}(p_0(x_1, x_2)) *
#'   \exp\left(\eta * I(x_1, x_2)\right),}
#' where \eqn{p_0(x_1, x_2) = 1 - (1 - p_1(x_1))(1 - p_2(x_2))} and each
#' single-agent probability follows a model \eqn{p_j(x_j)}. The normalized dose
#' \eqn{\tilde{x}_j} is extracted from the single-agent model's dose covariate,
#' e.g. \eqn{x_j / x_j^{*}}, \eqn{x_j - x_j^{*}}, or \eqn{x_j}.
#' The interaction parameter \eqn{\eta} has either a normal prior or, if
#' `log_normal_eta = TRUE`, a log-normal prior.
#'
#' @slot single_models (`list`)
#'   named list of length 2 containing single-agent [`GeneralModel`] objects,
#'   one per drug. Each model must use `nObs`, `y`, and `x` as data inputs and
#'   contain a Bernoulli likelihood for `y` in its `datamodel`.
#' @slot ref_dose (`numeric`)
#'   optional reference doses extracted from `single_models`, if provided.
#' @slot drug_names (`character`)
#'   the names of the two drugs.
#' @slot gamma (`numeric`)
#'   prior mean parameter for the interaction term.
#' @slot tau (`numeric`)
#'   prior precision parameter for the interaction term.
#' @slot log_normal_eta (`flag`)
#'   should the interaction term use a log-normal prior?
#'
#' @seealso [`LogisticLogNormal`], [`DataCombo`].
#'
#' @aliases TwoDrugsCombo
#' @export
#'
.TwoDrugsCombo <- setClass(
  Class = "TwoDrugsCombo",
  contains = "GeneralModel",
  slots = c(
    single_models = "list",
    ref_dose = "numeric",
    drug_names = "character",
    gamma = "numeric",
    tau = "numeric",
    log_normal_eta = "logical"
  ),
  prototype = prototype(
    single_models = list(
      drug1 = .DefaultLogisticLogNormal(),
      drug2 = .DefaultLogisticLogNormal()
    ),
    ref_dose = c(drug1 = 50, drug2 = 50),
    drug_names = c("drug1", "drug2"),
    gamma = 0,
    tau = 1,
    log_normal_eta = FALSE
  ),
  validity = v_model_two_drugs_combo
)

#' Replace Symbols in an R/JAGS Expression
#'
#' @description
#' Recursively substitutes symbols in an expression using a named list of
#' replacement expressions. Used to namespace single-agent model code before
#' joining it into a [`TwoDrugsCombo`] model.
#'
#' @param expr (`language`)\cr expression to transform.
#' @param replacements (`list`)\cr named list mapping symbol names to replacement
#'   symbols or calls.
#'
#' @return Transformed expression.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_replace_symbols <- function(expr, replacements) {
  if (is.symbol(expr)) {
    name <- as.character(expr)
    if (name %in% names(replacements)) {
      return(replacements[[name]])
    }
    return(expr)
  }
  if (is.call(expr)) {
    return(as.call(lapply(
      as.list(expr),
      h_two_drugs_combo_replace_symbols,
      replacements
    )))
  }
  expr
}

#' Create an Indexed JAGS Call
#'
#' @description
#' Creates calls like `theta[1]` or `p_single[i, 2]`.
#'
#' @param symbol (`string`)\cr name of the indexed object.
#' @param ... index expressions.
#'
#' @return A call to `[`.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_indexed_call <- function(symbol, ...) {
  as.call(c(list(as.name("["), as.name(symbol)), list(...)))
}

#' Find Symbols Assigned on the Left-Hand Side
#'
#' @description
#' Extracts root symbols from left-hand side expressions. For example,
#' `logit(p[i])` and `theta[1:2]` both return the assigned node name.
#'
#' @param expr (`language`)\cr left-hand side expression.
#'
#' @return Character vector of assigned node names.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_lhs_symbols <- function(expr) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr) && identical(as.character(expr[[1L]]), "[")) {
    return(h_two_drugs_combo_lhs_symbols(expr[[2L]]))
  }
  if (is.call(expr)) {
    return(unique(unlist(lapply(as.list(expr)[-1L], h_two_drugs_combo_lhs_symbols))))
  }
  character()
}

#' Collect Assigned JAGS Nodes
#'
#' @description
#' Recursively finds node names assigned by `<-` or `~` in a model body.
#'
#' @param expr (`language`)\cr model body or sub-expression.
#'
#' @return Character vector of node names.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_assigned_nodes <- function(expr) {
  lhs <- character()
  if (is.call(expr)) {
    operator <- as.character(expr[[1L]])
    if (operator %in% c("<-", "~")) {
      lhs <- h_two_drugs_combo_lhs_symbols(expr[[2L]])
    }
    lhs <- c(lhs, unlist(lapply(as.list(expr)[-1L], h_two_drugs_combo_assigned_nodes)))
  }
  unique(lhs)
}

#' Evaluate Single-Agent Model Specifications
#'
#' @description
#' Calls a compatible single-agent model's `modelspecs` function. The combo
#' constructor currently supports single-agent `modelspecs` with no data
#' arguments and an optional `from_prior` argument.
#'
#' @param model (`GeneralModel`)\cr single-agent model.
#' @param from_prior (`flag`)\cr whether to request prior-only specifications.
#'
#' @return A named list of model specifications.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_single_model_specs <- function(model, from_prior) {
  spec_args <- formalArgs(model@modelspecs)
  assert_subset(setdiff(spec_args, "from_prior"), character())
  args <- if ("from_prior" %in% spec_args) {
    list(from_prior = from_prior)
  } else {
    list()
  }
  do.call(model@modelspecs, args)
}

#' Extract Optional Reference Dose
#'
#' @description
#' Reads `ref_dose` from full model specifications when available. Models
#' without `ref_dose` are valid and return `NA_real_`.
#'
#' @inheritParams h_two_drugs_combo_single_model_specs
#'
#' @return Numeric scalar reference dose or `NA_real_`.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_single_model_ref_dose <- function(model) {
  specs <- h_two_drugs_combo_single_model_specs(model, from_prior = FALSE)
  if ("ref_dose" %in% names(specs) && isTRUE(test_number(specs$ref_dose))) {
    specs$ref_dose
  } else {
    NA_real_
  }
}

#' Suffix Named List Elements
#'
#' @description
#' Adds a suffix to every name in a named list. Used for model specifications and
#' initial values after drug-specific namespacing.
#'
#' @param x (`list`)\cr named list.
#' @param suffix (`string`)\cr suffix to append.
#'
#' @return `x` with suffixed names.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_suffix_names <- function(x, suffix) {
  names(x) <- vapply(
    names(x),
    function(name) {
      paste0(name, suffix)
    },
    character(1L)
  )
  x
}

#' Does an Expression Contain a Symbol?
#'
#' @param expr (`language`)\cr expression to inspect.
#' @param symbol (`string`)\cr symbol name to search for.
#'
#' @return A logical scalar.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_contains_symbol <- function(expr, symbol) {
  if (is.symbol(expr)) {
    return(identical(as.character(expr), symbol))
  }
  if (is.call(expr)) {
    return(any(vapply(
      as.list(expr)[-1L],
      h_two_drugs_combo_contains_symbol,
      logical(1L),
      symbol = symbol
    )))
  }
  FALSE
}

#' Is an Expression the Dose Term?
#'
#' @description
#' Detects raw dose expressions, either `x` or indexed `x[...]`.
#'
#' @inheritParams h_two_drugs_combo_contains_symbol
#'
#' @return A logical scalar.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_is_x_term <- function(expr) {
  is.symbol(expr) &&
    identical(as.character(expr), "x") ||
    is.call(expr) &&
      identical(as.character(expr[[1L]]), "[") &&
      is.symbol(expr[[2L]]) &&
      identical(as.character(expr[[2L]]), "x")
}

#' Infer Normalized Dose from an Expression
#'
#' @description
#' Infers the single-agent dose covariate used in the linear predictor. It strips
#' wrappers such as `log(...)`, follows the single `x`-containing term through
#' sums and products, and preserves normalizations such as `x / ref_dose` and
#' `x - ref_dose`.
#'
#' @inheritParams h_two_drugs_combo_contains_symbol
#'
#' @return A dose-normalization expression, or `NULL` when no dose appears.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_normalized_dose_from_expr <- function(expr) {
  if (!h_two_drugs_combo_contains_symbol(expr, "x")) {
    return(NULL)
  }
  if (h_two_drugs_combo_is_x_term(expr)) {
    return(expr)
  }
  if (is.call(expr) && identical(as.character(expr[[1L]]), "log")) {
    return(h_two_drugs_combo_normalized_dose_from_expr(expr[[2L]]))
  }
  if (is.call(expr) && identical(as.character(expr[[1L]]), "(")) {
    return(h_two_drugs_combo_normalized_dose_from_expr(expr[[2L]]))
  }
  if (is.call(expr) && as.character(expr[[1L]]) %in% c("+", "*")) {
    x_args <- Filter(
      function(arg) h_two_drugs_combo_contains_symbol(arg, "x"),
      as.list(expr)[-1L]
    )
    if (length(x_args) == 1L) {
      return(h_two_drugs_combo_normalized_dose_from_expr(x_args[[1L]]))
    }
  }
  if (is.call(expr) && as.character(expr[[1L]]) %in% c("/", "-")) {
    if (h_two_drugs_combo_contains_symbol(expr[[2L]], "x")) {
      return(expr)
    }
  }
  expr
}

#' Collect Right-Hand Side Expressions
#'
#' @description
#' Recursively collects the right-hand side of `<-` and `~` expressions in a
#' model body.
#'
#' @inheritParams h_two_drugs_combo_contains_symbol
#'
#' @return A list of expressions.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_rhs_expressions <- function(expr) {
  rhs <- list()
  if (is.call(expr)) {
    operator <- as.character(expr[[1L]])
    if (operator %in% c("<-", "~")) {
      rhs <- list(expr[[3L]])
    }
    rhs <- c(
      rhs,
      unlist(
        lapply(
          as.list(expr)[-1L],
          h_two_drugs_combo_rhs_expressions
        ),
        recursive = FALSE
      )
    )
  }
  rhs
}

#' Infer and Namespace Normalized Dose
#'
#' @description
#' Finds the first dose-normalization expression in a model body and applies the
#' drug-specific symbol replacements.
#'
#' @param expr (`language`)\cr single-agent data model body.
#' @param replacements (`list`)\cr symbol replacements used for namespacing.
#'
#' @return Namespaced normalized-dose expression.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_normalized_dose_expr <- function(expr, replacements) {
  normalized <- Filter(
    Negate(is.null),
    lapply(h_two_drugs_combo_rhs_expressions(expr), h_two_drugs_combo_normalized_dose_from_expr)
  )
  assert_true(length(normalized) > 0L)
  h_two_drugs_combo_replace_symbols(normalized[[1L]], replacements)
}

#' Replace a Bernoulli Likelihood
#'
#' @description
#' Converts a single-agent likelihood `y[i] ~ dbern(p[i])` into an assignment to
#' the drug-specific combo probability, e.g. `p_single[i, 1] <- p_drug1[i]`.
#'
#' @param expr (`language`)\cr expression to inspect.
#' @param replacements (`list`)\cr symbol replacements used for namespacing.
#' @param index (`count`)\cr drug index in the combo model.
#'
#' @return Replacement expression, or `NULL` if `expr` is not a Bernoulli
#'   likelihood.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_likelihood_replacement <- function(expr, replacements, index) {
  is_likelihood <- is.call(expr) &&
    identical(as.character(expr[[1L]]), "~") &&
    is.call(expr[[3L]]) &&
    identical(as.character(expr[[3L]][[1L]]), "dbern")

  if (!is_likelihood) {
    return(NULL)
  }

  # Preserve the observation index from the original likelihood where possible,
  # so models using a non-standard loop index still map into p_single correctly.
  lhs <- expr[[2L]]
  dose_index <- if (is.call(lhs) && identical(as.character(lhs[[1L]]), "[")) {
    h_two_drugs_combo_replace_symbols(lhs[[3L]], replacements)
  } else {
    as.name("i")
  }
  prob <- h_two_drugs_combo_replace_symbols(expr[[3L]][[2L]], replacements)

  as.call(list(
    as.name("<-"),
    h_two_drugs_combo_indexed_call("p_single", dose_index, index),
    prob
  ))
}

#' Replace Bernoulli Likelihoods in a Data Model
#'
#' @description
#' Recursively namespaces a single-agent data model and replaces its Bernoulli
#' toxicity likelihood with a `p_single` assignment.
#'
#' @inheritParams h_two_drugs_combo_likelihood_replacement
#'
#' @return A list with `expr`, the transformed expression, and `found`, a flag
#'   indicating whether a Bernoulli likelihood was replaced.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_replace_bernoulli_likelihood <- function(expr, replacements, index) {
  found <- FALSE

  replace_likelihood <- function(expr) {
    # The likelihood replacement has to happen before generic symbol
    # replacement, otherwise `y[i] ~ dbern(p[i])` would be namespaced instead of
    # being converted into the single-agent probability contribution.
    replacement <- h_two_drugs_combo_likelihood_replacement(expr, replacements, index)
    if (!is.null(replacement)) {
      found <<- TRUE
      return(replacement)
    }
    if (is.symbol(expr)) {
      return(h_two_drugs_combo_replace_symbols(expr, replacements))
    }
    if (is.call(expr)) {
      return(as.call(lapply(as.list(expr), replace_likelihood)))
    }
    expr
  }

  list(expr = replace_likelihood(expr), found = found)
}

#' Create Dose Column Mapping Model
#'
#' @description
#' Creates a tiny JAGS model that maps the two-column combo dose matrix to a
#' single-agent dose vector, e.g. `x_drug1[i] <- x[i, 1]`.
#'
#' @param index (`count`)\cr drug index in the combo dose matrix.
#'
#' @return Function containing a JAGS model fragment.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_x_mapping_model <- function(index) {
  model <- function() {}
  body(model) <- substitute(
    {
      for (i in 1:nObs) {
        x_agent[i] <- x[i, drug_index]
      }
    },
    list(
      x_agent = as.name(paste0("x_drug", index)),
      drug_index = index
    )
  )
  model
}

#' Create Sample Alias Model
#'
#' @description
#' Creates deterministic JAGS aliases from namespaced single-agent sample nodes
#' back to user-facing sample names. If both agents expose a sample name, the
#' alias is vector-valued; if only one agent exposes it, the alias is scalar.
#'
#' @param samples (`character`)\cr union of single-agent sample names.
#' @param single_models (`list`)\cr single-agent models.
#'
#' @return Function containing a JAGS model fragment.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_sample_alias_model <- function(samples, single_models) {
  model <- function() {}
  expressions <- list(as.name("{"))
  for (sample_name in samples) {
    # Use the compact index in the public alias (`alpha0[1]`) but map it to the
    # original drug index in the namespaced node (`alpha0_drug2`) when a sample
    # exists for only a subset of single-agent models.
    sample_model_indices <- which(vapply(
      single_models,
      function(model) sample_name %in% model@sample,
      logical(1L)
    ))
    for (index in seq_along(sample_model_indices)) {
      model_index <- sample_model_indices[[index]]
      expressions <- c(
        expressions,
        list(as.call(list(
          as.name("<-"),
          h_two_drugs_combo_indexed_call(sample_name, index),
          as.name(paste0(sample_name, "_drug", model_index))
        )))
      )
    }
  }
  body(model) <- as.call(expressions)
  model
}

#' Create Interaction Covariate Model
#'
#' @description
#' Multiplies the normalized dose expressions from the two single-agent models
#' to obtain the combo interaction covariate.
#'
#' @param normalized_dose (`list`)\cr normalized-dose expressions, one per drug.
#'
#' @return Function containing a JAGS model fragment.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_interaction_model <- function(normalized_dose) {
  model <- function() {}
  interaction <- as.call(c(list(as.name("*")), normalized_dose))
  body(model) <- substitute(
    {
      for (i in 1:nObs) {
        combo_interaction[i] <- interaction_value
      }
    },
    list(interaction_value = interaction)
  )
  model
}

#' Build Namespaced Single-Agent Model Parts
#'
#' @description
#' Takes one compatible single-agent model and returns drug-specific JAGS
#' fragments, model specifications, initial values, and normalized dose
#' expression for use inside a [`TwoDrugsCombo`] model.
#'
#' @param model (`GeneralModel`)\cr single-agent model.
#' @param index (`count`)\cr drug index.
#'
#' @return A named list with `priormodel`, `datamodel`, `normalized_dose`,
#'   `prior_specs`, `full_specs`, and `inits`.
#' @keywords internal
#' @noRd
#'
h_two_drugs_combo_single_model_part <- function(model, index) {
  prior_specs <- h_two_drugs_combo_single_model_specs(model, from_prior = TRUE)
  full_specs <- h_two_drugs_combo_single_model_specs(model, from_prior = FALSE)
  prior_inits <- do.call(model@init, list())
  prior_nodes <- h_two_drugs_combo_assigned_nodes(body(model@priormodel))
  data_nodes <- h_two_drugs_combo_assigned_nodes(body(model@datamodel))
  suffix <- paste0("_drug", index)
  prefixed_nodes <- setdiff(
    unique(c(
      names(full_specs),
      names(prior_inits),
      prior_nodes,
      data_nodes,
      model@sample
    )),
    c("nObs", "x", "y")
  )
  # Every node or data specification local to the single-agent model receives a
  # drug suffix. The common data names `nObs`, `x`, and `y` are kept special
  # because the combo model supplies them.
  replacements <- c(
    setNames(
      lapply(prefixed_nodes, function(name) as.name(paste0(name, suffix))),
      prefixed_nodes
    ),
    list(x = as.name(paste0("x_drug", index)))
  )
  normalized_dose <- h_two_drugs_combo_normalized_dose_expr(
    body(model@datamodel),
    replacements
  )

  prior_model <- model@priormodel
  body(prior_model) <- h_two_drugs_combo_replace_symbols(body(prior_model), replacements)

  data_model <- model@datamodel
  data_body <- h_two_drugs_combo_replace_bernoulli_likelihood(
    body(data_model),
    replacements = replacements,
    index = index
  )
  assert_true(data_body$found)
  body(data_model) <- data_body$expr
  # Prepend the dose-column mapping so the namespaced single-agent data model can
  # continue to refer to a vector dose input.
  data_model <- h_jags_join_models(h_two_drugs_combo_x_mapping_model(index), data_model)

  list(
    priormodel = prior_model,
    datamodel = data_model,
    normalized_dose = normalized_dose,
    prior_specs = h_two_drugs_combo_suffix_names(prior_specs, suffix),
    full_specs = h_two_drugs_combo_suffix_names(full_specs, suffix),
    inits = h_two_drugs_combo_suffix_names(prior_inits, suffix)
  )
}

## constructor ----

#' @rdname TwoDrugsCombo-class
#'
#' @param single_models (`list`)
#'   named list of length 2 with compatible single-agent [`GeneralModel`]
#'   objects, one per drug.
#' @param gamma (`number`)
#'   prior mean parameter for the interaction term.
#' @param tau (`number`)
#'   prior precision parameter for the interaction term.
#' @param log_normal_eta (`flag`)
#'   should the interaction term use a log-normal prior?
#'
#' @export
#' @example examples/Model-class-TwoDrugsCombo.R
TwoDrugsCombo <- function(
  single_models,
  gamma = 0,
  tau = 1,
  log_normal_eta = FALSE
) {
  assert_list(single_models, len = 2L)
  if (is.null(names(single_models))) {
    names(single_models) <- paste0("drug", seq_along(single_models))
  }
  assert_character(
    names(single_models),
    len = 2L,
    unique = TRUE,
    any.missing = FALSE
  )
  assert_true(all(sapply(single_models, test_class, "GeneralModel")))
  assert_true(all(vapply(
    single_models,
    function(model) {
      setequal(model@datanames, c("nObs", "y", "x")) &&
        length(formalArgs(model@init)) == 0L &&
        setequal(
          setdiff(formalArgs(model@modelspecs), "from_prior"),
          character()
        )
    },
    logical(1L)
  )))
  assert_number(gamma, finite = TRUE)
  assert_number(tau, lower = .Machine$double.xmin, finite = TRUE)
  assert_flag(log_normal_eta)

  ref_dose <- as.numeric(vapply(
    single_models,
    h_two_drugs_combo_single_model_ref_dose,
    numeric(1L)
  ))
  names(ref_dose) <- names(single_models)

  single_model_parts <- lapply(seq_along(single_models), function(index) {
    h_two_drugs_combo_single_model_part(single_models[[index]], index = index)
  })
  single_datamodel <- Reduce(
    h_jags_join_models,
    lapply(single_model_parts, "[[", "datamodel")
  )
  single_priormodel <- Reduce(
    h_jags_join_models,
    lapply(single_model_parts, "[[", "priormodel")
  )
  all_samples <- unique(unlist(lapply(single_models, slot, "sample")))
  single_priormodel <- h_jags_join_models(
    single_priormodel,
    h_two_drugs_combo_sample_alias_model(all_samples, single_models)
  )
  single_datamodel <- h_jags_join_models(
    single_datamodel,
    h_two_drugs_combo_interaction_model(lapply(
      single_model_parts,
      "[[",
      "normalized_dose"
    ))
  )

  .TwoDrugsCombo(
    single_models = single_models,
    ref_dose = ref_dose,
    drug_names = names(single_models),
    gamma = gamma,
    tau = tau,
    log_normal_eta = log_normal_eta,
    datamodel = h_jags_join_models(
      single_datamodel,
      function() {
        for (i in 1:nObs) {
          p0[i] <- p_single[i, 1] +
            p_single[i, 2] -
            p_single[i, 1] * p_single[i, 2]
          logit(p[i]) <- log(p0[i] / (1 - p0[i])) +
            eta * combo_interaction[i]
          y[i] ~ dbern(p[i])
        }
      }
    ),
    priormodel = h_jags_join_models(
      single_priormodel,
      if (log_normal_eta) {
        function() {
          log_eta ~ dnorm(gamma, tau)
          eta <- exp(log_eta)
        }
      } else {
        function() {
          eta ~ dnorm(gamma, tau)
        }
      }
    ),
    modelspecs = function(from_prior) {
      specs_name <- if (from_prior) "prior_specs" else "full_specs"
      ms <- c(
        unlist(lapply(single_model_parts, "[[", specs_name), recursive = FALSE),
        list(gamma = gamma, tau = tau)
      )
      ms
    },
    init = if (log_normal_eta) {
      function() {
        c(
          unlist(lapply(single_model_parts, "[[", "inits"), recursive = FALSE),
          list(log_eta = gamma)
        )
      }
    } else {
      function() {
        c(
          unlist(lapply(single_model_parts, "[[", "inits"), recursive = FALSE),
          list(eta = gamma)
        )
      }
    },
    datanames = c("nObs", "y", "x"),
    sample = c(all_samples, "eta")
  )
}

## default constructor ----

#' @rdname TwoDrugsCombo-class
#' @note Typically, end users will not use the `.DefaultTwoDrugsCombo()` function.
#' @export
.DefaultTwoDrugsCombo <- function() {
  TwoDrugsCombo(
    single_models = list(
      drug1 = LogisticLogNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 10
      ),
      drug2 = LogisticLogNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 20
      )
    ),
    gamma = 0,
    tau = 1,
    log_normal_eta = FALSE
  )
}
