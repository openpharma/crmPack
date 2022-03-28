#' Internal Helper Functions for Validation of [`AllModels`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`AllModels`] or inherited classes and therefore not exported.
#'
#' @name v_model_objects
#' @param object (`AllModels`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_model_objects validates that the names of the
#'   arguments in `init` function are included in `datanames` slot.
v_general_model <- function(object) {
  v <- Validate()
  v$check(
    h_check_fun_formals(object@init, allowed = object@datanames),
    "Arguments of the init function must be data names"
  )
  v$result()
}

#' @describeIn v_model_objects validates that the names of the
#'   arguments in `dose` and `prob` functions contains `prob` and `dose`
#'   respectively, as well as they match `sample` slot of the `object`.
v_model <- function(object) {
  v <- Validate()
  v$check(
    h_check_fun_formals(
      object@dose,
      mandatory = "prob",
      allowed = object@sample
    ),
    "Arguments of dose function are incorrect"
  )
  v$check(
    h_check_fun_formals(
      object@prob,
      mandatory = "dose",
      allowed = object@sample
    ),
    "Arguments of prob function are incorrect"
  )
  v$result()
}

#' @describeIn v_model_objects validates that the `ref_dose` is a positive scalar.
v_model_log_normal <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  v$result()
}

#' @describeIn v_model_objects validates that the logistic Kadane model
#'   parameters are valid.
v_model_logistic_kadane <- function(object) {
  v <- Validate()
  v$check(
    is.probability(object@theta, bounds = FALSE),
    "theta must be a probability scalar > 0 and < 1"
  )
  is_xmin_number <- test_number(object@xmin)
  v$check(is_xmin_number, "xmin must be scalar")

  is_xmax_number <- test_number(object@xmax)
  v$check(is_xmax_number, "xmax must be scalar")

  if (is_xmin_number && is_xmax_number) {
    v$check(object@xmin < object@xmax, "xmin must be strictly smaller than xmax")
  }
  v$result()
}

#' @describeIn v_model_objects validates that the logistic Kadane model
#'   parameters with a beta and gamma prior are valid.
v_model_logistic_kadane_beta_gamma <- function(object) {
  v <- Validate()
  v$check(
    is.scalar(object@alpha) & is.numeric(object@alpha) && object@alpha > 0,
    "Beta distribution shape parameter alpha must be a positive scalar"
  )
  v$check(
    is.scalar(object@beta) & is.numeric(object@beta) && object@beta > 0,
    "Beta distribution shape parameter beta must be a positive scalar"
  )
  v$check(
    is.scalar(object@shape) & is.numeric(object@shape) && object@shape > 0,
    "Gamma distribution shape parameter must be a positive scalar"
  )
  v$check(
    is.scalar(object@rate) & is.numeric(object@rate) && object@rate > 0,
    "Gamma distribution rate parameter must be a positive scalar"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `weightpar` and `ref_dose` are valid.
v_model_logistic_normal_mix <- function(object) {
  v <- Validate()

  v$check(
    h_test_named_numeric(object@weightpar, permutation.of = c("a", "b")),
    "weightpar must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
  v$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `component` is a list with
#'   valid `ModelParamsNormal` objects as well as `weights` and `ref_dose` are
#'   correct.
v_model_logistic_normal_fixed_mix <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@components, test_class, "ModelParamsNormal")),
    "components must be a list with ModelParamsNormal S4 class objects"
  )
  comp_valid_result <- sapply(object@components, validObject, test = TRUE)
  comp_valid <- sapply(comp_valid_result, isTRUE)
  v$check(
    all(comp_valid),
    paste(
      "components must be a list with valid ModelParamsNormal S4 class objects",
      paste(unlist(comp_valid_result[!comp_valid]), collapse = ", "),
      collapse = ", ",
      sep = ", "
    )
  )
  v$check(
    length(object@components) == length(object@weights),
    "components must have same length as weights"
  )
  v$check(
    test_numeric(object@weights, lower = 0 + .Machine$double.xmin, finite = TRUE, any.missing = FALSE),
    "weights must be positive"
  )
  v$check(
    sum(object@weights) == 1,
    "weights must sum to 1"
  )
  v$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  v$check(
    test_flag(object@log_normal),
    "log_normal must be TRUE or FALSE"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `ref_dose` is valid and
#'   `share_weight` represents probability.
v_model_logistic_log_normal_mix <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  v$check(
    is.probability(object@share_weight),
    "share_weight does not specify a probability"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpoint`] class slots are valid.
v_model_dual_endpoint <- function(object) {
  rmin <- .Machine$double.xmin
  v <- Validate()

  v$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0 + rmin),
    "ref_dose must be a positive scalar"
  )
  v$check(
    test_flag(object@use_log_dose),
    "use_log_dose must be TRUE or FALSE"
  )
  uf_sigma2W <- object@use_fixed["sigma2W"]
  v$check(
    test_flag(uf_sigma2W),
    "use_fixed must be a named logical vector that contains name 'sigma2W'"
  )
  uf_rho <- object@use_fixed["rho"]
  v$check(
    test_flag(uf_rho),
    "use_fixed must be a named logical vector that contains name 'rho'"
  )

  if (isTRUE(uf_sigma2W)) {
    v$check(
      test_number(object@sigma2W, lower = 0 + rmin, finite = TRUE),
      "sigma2W must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2W is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2W, permutation.of = c("a", "b")),
      "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }

  if (isTRUE(uf_rho)) {
    v$check(
      test_number(object@rho, lower = -1 + rmin, upper = 1 - rmin), # rmin is ignored here!
      "rho must be a number in (-1, 1)"
    )
  } else {
    # object@rho is a vector with parameters for Beta(a, b).
    v$check(
      h_test_named_numeric(object@rho, permutation.of = c("a", "b")),
      "rho must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }

  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointRW`] class slots are valid.
v_model_dual_endpoint_rw <- function(object) {
  v <- Validate()

  uf_sigma2W <- object@use_fixed["sigma2betaW"]
  v$check(
    test_flag(uf_sigma2W),
    "use_fixed must be a named logical vector that contains name 'sigma2betaW'"
  )
  if (isTRUE(uf_sigma2W)) {
    v$check(
      test_number(object@sigma2betaW, lower = 0 + .Machine$double.xmin, finite = TRUE),
      "sigma2betaW must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2betaW is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2betaW, permutation.of = c("a", "b")),
      "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }
  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointBeta`] class slots are valid.
v_model_dual_endpoint_beta <- function(object) {
  v <- Validate()

  for (s in c("E0", "Emax", "delta1", "mode")) {
    rmin <- .Machine$double.xmin
    uf <- object@use_fixed[s]

    v$check(
      test_flag(uf),
      paste0("use_fixed must be a named logical vector that contains name '", s, "'")
    )
    if (isTRUE(uf)) {
      if (s %in% c("delta1", "mode")) {
        v$check(
          test_number(slot(object, s), lower = 0 + rmin, finite = TRUE),
          paste(s, "must be a positive and finite numerical scalar")
        )
      }
    } else {
      # s is a vector with parameters for Uniform(s[1], s[2]) prior.
      v$check(
        test_numeric(
          slot(object, s),
          lower = 0,
          finite = TRUE,
          any.missing = FALSE,
          len = 2,
          unique = TRUE,
          sorted = TRUE
        ),
        paste(s, "must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values")
      )
    }
  }

  v$check(
    test_number(object@ref_dose_beta, na.ok = TRUE, lower = 0 + rmin),
    "ref_dose_beta must be a positive scalar"
  )

  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointEmax`] class slots are valid.
v_model_dual_endpoint_emax <- function(object) {
  v <- Validate()

  for (s in c("E0", "Emax", "ED50")) {
    rmin <- .Machine$double.xmin
    uf <- object@use_fixed[s]

    v$check(
      test_flag(uf),
      paste0("use_fixed must be a named logical vector that contains name '", s, "'")
    )
    if (isTRUE(uf)) {
      v$check(
        test_number(slot(object, s), lower = 0 + rmin, finite = TRUE),
        paste(s, "must be a positive and finite numerical scalar")
      )
    } else {
      # s is a vector with parameters for Uniform(s[1], s[2]) prior.
      v$check(
        test_numeric(
          slot(object, s),
          lower = 0,
          finite = TRUE,
          any.missing = FALSE,
          len = 2,
          unique = TRUE,
          sorted = TRUE
        ),
        paste(s, "must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values")
      )
    }
  }

  v$check(
    test_number(object@ref_dose_emax, na.ok = TRUE, lower = 0 + rmin),
    "ref_dose_emax must be a positive scalar"
  )

  v$result()
}
