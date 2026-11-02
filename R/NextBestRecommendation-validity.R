#' Internal Helper Functions for Validation of [`NextBestRecommendation`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`NextBestRecommendation`] or inherited classes and therefore not exported.
#'
#' @name v_nextbestrecommendation
#' @param object (`NextBestRecommendation`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_nextbestrecommendation validates that the [`NextBestRecommendation`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot.
v_nextbestrecommendation <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@value,
      # len = 1,
      lower = 1,
      # any.missing = FALSE
    ),
    "value must be a positive real or `NA`."
  )
  v$result()
}

#' @name v_nextbestrecommendationprobs
#' @param object (`NextBestRecommendationProbs`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_nextbestrecommendation validates that the [`NextBestRecommendationProbs`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a named list of probabilities in the `probs` slot.
v_nextbestrecommendationprobs <- function(object) {
  v <- Validate()
  cols <- names(object@probs)
  v$check(
    test_true("Dose" %in% cols),
    "probs must contain a column named Dose."
  )

  # Not appropriate for NextBestNCRMLoss
  # unset(cols["Dose"])
  # for (c in cols) {
  #   v$check(
  #     test_probabilities(object@probs[c]),
  #     "every column of probs apart from dose must contain non-missing probabilities."
  #   )
  # }

  v$check(
    test_atomic_vector(
      object@probs["Dose"],
      unique = TRUE,
      any.missing = FALSE
    ),
    "probs must be named and names must be unique."
  )
  v$check(
    test_list(object@probs),
    "probs must be a list."
  )
  v$result()
}

#' @name v_nextbestrecommendationthreeplusthree
#' @param object (`NextBestRecommendationThreePlusThree`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_nextbestrecommendation validates that the [`NextBestRecommendationThreePlusThree`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a flag in the `stopHere` slot.
v_nextbestrecommendationthreeplusthree <- function(object) {
  v <- Validate()
  v$check(
    test_flag(object@stopHere),
    "stopHere must be a flag."
  )
  v$result()
}
