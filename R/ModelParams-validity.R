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
  v$check(is_cov_2x2, "cov must be 2x2 matrix without any missing values")
  v$check(is_prec_2x2, "prec must be 2x2 matrix without any missing values")
  if (is_cov_2x2) {
    v$check(
      h_is_positive_definite(object@cov),
      "cov must be positive-definite matrix"
    )
    if (is_prec_2x2) {
      v$check(
        all.equal(object@cov %*% object@prec, diag(1, 2), check.attributes = FALSE) == TRUE,
        "prec must be inverse of cov"
      )
    }
  }
  v$result()
}
