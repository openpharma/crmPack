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
    test_numeric(
      object@weightpar,
      lower = 0 + .Machine$double.xmin,
      finite = TRUE,
      any.missing = FALSE,
      len = 2,
      names = "named"
    ),
    "weightpar must be a numerical vector of length two with values greater than 0"
  )
  v$check(
    test_set_equal(names(object@weightpar), c("a", "b")),
    "weightpar should be a named vector of length two with names 'a' and 'b'"
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

#' @describeIn v_model_objects validates that `share_weight` represents probability.
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
