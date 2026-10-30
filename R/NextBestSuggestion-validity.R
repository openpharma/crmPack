#' Internal Helper Functions for Validation of [`NextBestSuggestion`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`NextBestSuggestion`] or inherited classes and therefore not exported.
#'
#' @name v_nextbestsuggestion
#' @param object (`NextBestSuggestion`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_nextbestsuggestion validates that the [`NextBestSuggestion`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot.
v_nextbestsuggestion <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@value,
      len = 1,
      lower = 1,
      any.missing = FALSE
    ),
    "value must be a positive real or `NA`."
  )
  v$result()
}

#' @name v_nextbestsuggestionprobs
#' @param object (`NextBestSuggestionProbs`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_nextbestsuggestion validates that the [`NextBestSuggestionProbs`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a named list of probabilities in the `probs` slot.
v_nextbestsuggestionprobs <- function(object) {
  v <- Validate()
  cols <- names(object@probs)
  v$check(
    test_true("Dose" %in% cols),
    "probs must contain a column named Dose."
  )
  unset(cols["Dose"])
  for (c in cols) {
    v$check(
      test_probabilities(object@probs[c]),
      "every column of probs apart from dose must contain non-missing probabilities."
    )
  }
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
