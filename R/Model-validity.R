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
  o <- Validate()
  o$check(
    h_check_fun_formals(object@init, allowed = object@datanames),
    "Arguments of the init function must be data names"
  )
  o$result()
}

#' @describeIn v_model_objects validates that the names of the
#'   arguments in `dose` and `prob` functions contains `prob` and `dose`
#'   respectively, as well as they match `sample` slot of the `object`.
v_model <- function(object) {
  o <- Validate()
  o$check(
    h_check_fun_formals(
      object@dose,
      mandatory = "prob",
      allowed = object@sample
    ),
    "Arguments of dose function are incorrect"
  )
  o$check(
    h_check_fun_formals(
      object@prob,
      mandatory = "dose",
      allowed = object@sample
    ),
    "Arguments of prob function are incorrect"
  )
  o$result()
}

#' @describeIn v_model_objects validates that the normal model parameters
#'   are valid as well as `ref_dose` is a positive scalar.
v_model_log_normal <- function(object) {
  o <- Validate()
  o$check(
    test_numeric(x = object@mean, len = 2L, any.missing = FALSE),
    "mean must have length 2 and no missing values are allowed"
  )
  is_cov_2x2 <- test_matrix(
    object@cov,
    mode = "numeric", nrows = 2, ncols = 2, any.missing = FALSE
  )
  is_prec_2x2 <- test_matrix(
    object@prec,
    mode = "numeric", nrows = 2, ncols = 2, any.missing = FALSE
  )
  o$check(is_cov_2x2, "cov must be 2x2 matrix without any missing values")
  o$check(is_prec_2x2, "prec must be 2x2 matrix without any missing values")
  if (is_cov_2x2) {
    o$check(
      h_is_positive_definite(object@cov),
      "cov must be positive-definite matrix"
    )
    if (is_prec_2x2) {
      o$check(
        all.equal(object@cov %*% object@prec, diag(1, 2), check.attributes = FALSE) == TRUE,
        "prec must be inverse of cov"
      )
    }
  }
  o$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  o$result()
}

#' @describeIn v_model_objects validates that the logistic Kadane model
#'   parameters are valid.
v_model_logistic_kadane <- function(object) {
  o <- Validate()
  o$check(
    is.probability(object@theta, bounds = FALSE),
    "theta must be a probability scalar > 0 and < 1"
  )
  is_xmin_number <- test_number(object@xmin)
  o$check(is_xmin_number, "xmin must be scalar")

  is_xmax_number <- test_number(object@xmax)
  o$check(is_xmax_number, "xmax must be scalar")

  if (is_xmin_number && is_xmax_number) {
    o$check(object@xmin < object@xmax, "xmin must be strictly smaller than xmax")
  }
  o$result()
}

#' @describeIn v_model_objects validates that the logistic normal mixture
#'   model parameters as well as `ref_dose` are valid.
v_model_logistic_normal_mixture <- function(object) {
  o <- Validate()

  for (i in c("comp1", "comp2")) {
    comp <- slot(object, i)
    o$check(
      test_numeric(x = comp$mean, len = 2L, any.missing = FALSE),
      "mean must have length 2 and no missing values are allowed"
    )
    is_cov_2x2 <- test_matrix(
      comp$cov,
      mode = "numeric", nrows = 2, ncols = 2, any.missing = FALSE
    )
    is_prec_2x2 <- test_matrix(
      comp$prec,
      mode = "numeric", nrows = 2, ncols = 2, any.missing = FALSE
    )
    o$check(is_cov_2x2, "cov must be 2x2 matrix without any missing values")
    o$check(is_prec_2x2, "prec must be 2x2 matrix without any missing values")
    if (is_cov_2x2) {
      o$check(
        h_is_positive_definite(comp$cov),
        "cov must be positive-definite matrix"
      )
    }
    if (is_prec_2x2) {
      o$check(
        all.equal(comp$cov %*% comp$prec, diag(1, 2), check.attributes = FALSE) == TRUE,
        "prec must be inverse of cov"
      )
    }
  }
  o$check(
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
  o$check(
    test_set_equal(names(object@weightpar), c("a", "b")),
    "weightpar should be a named vector of length two with names 'a' and 'b'"
  )
  o$check(
    test_number(object@ref_dose, na.ok = TRUE, lower = 0),
    "ref_dose must be a non-negative scalar"
  )
  o$result()
}
