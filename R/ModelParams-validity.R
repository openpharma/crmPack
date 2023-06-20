#' Internal Helper Functions for Validation of Model Parameters Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an object
#' with model parameters or inherited classes and therefore not exported.
#'
#' @name v_model_params
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_model_params a helper function that validates bivariate normal
#'   parameters.
#' @param object (`ModelParamsNormal`)\cr bivariate normal parameters object
#'   to validate.
v_model_params_normal <- function(object) {
  v <- Validate()

  v$check(
    test_numeric(x = object@mean, min.len = 2L, any.missing = FALSE),
    "mean must have length of at least 2 and no missing values are allowed"
  )
  is_cov_valid <- h_is_positive_definite(object@cov, size = nrow(object@cov))
  v$check(
    is_cov_valid,
    "cov must be a square positive-definite matrix without any missing values"
  )
  is_prec_valid <- h_is_positive_definite(object@prec, size = nrow(object@cov))
  v$check(
    is_prec_valid,
    "prec must be a square positive-definite matrix without any missing values"
  )
  if (is_cov_valid && is_prec_valid) {
    v$check(
      all.equal(object@cov %*% object@prec, diag(1, nrow(object@cov)), check.attributes = FALSE) == TRUE,
      "prec must be inverse of cov"
    )
  }
  v$result()
}
