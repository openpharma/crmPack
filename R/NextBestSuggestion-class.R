#' @include helpers.R
#' @include NextBestSuggestion-validity.R
#' @include CrmPackClass-class.R
NULL

# NextBestSuggestion-class ----

#' `NextBestSuggestion`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestSuggestion`] is a class for defining the suggested dose for the next
#' cohort to be treated.
#'
#' @slot value (`real`)\cr The suggested dose.
#'
#' @aliases NextBestSuggestion
#' @export
#'
.NextBestSuggestion <- setClass(
  Class = "NextBestSuggestion",
  slots = c(
    value = "numeric"
  ),
  prototype = prototype(
    list(value = NA_real_)
  ),
  contains = "CrmPackClass",
  validity = v_nextbestsuggestion
)

## constructor ----

#' @rdname NextBestSuggestion-class
#' @param value (`number`)\crthe suggested dose for the next cohort
#' @export
NextBestSuggestion <- function(value) {
  .NextBestSuggestion(value = value)
}

## default constructor ----

#' @rdname NextBestSuggestion-class
#' @note Typically, end users will not use the `.DefaultNextBestSuggestion()` function.
#' @export
.DefaultNextBestSuggestion <- function() {
  .NextBestSuggestion(value = 1)
}

# NextBestSuggestionProbs ----

## class ----

#' `NextBestSuggestionProbs`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`NextBestSuggestionProbs`] inherits from [`NextBestSuggestion`] and adds a
#' in which probabilities associated with the suggestion can be stored.
#'
#' @slot probs (`list`)\cr the probabilities associated with the suggestion.
#'
#' @aliases NextBestSuggestionProbs
#' @export
#'
.NextBestSuggestionProbs <- setClass(
  Class = "NextBestSuggestionProbs",
  contains = "NextBestSuggestion",
  slots = c(
    probs = "list"
  ),
  prototype = prototype(
    backfilled = logical(),
    probs = list()
  ),
  # validity = v_nextbestsuggestionprobs
)

## constructor ----

#' @rdname NextBestSuggestionProbs-class
#'
#' @inheritParams NextBestSuggestion
#' @param probs (`list`)\cr ta named list of probabilities.
#' @param ... not used.
#'
#' @export
#' @example examples/NextBestSuggestionProbs-class.R
#'
NextBestSuggestionProbs <- function(
  value = numeric(),
  probs = list(),
  ...
) {
  assert_numeric(value)
  assert_probabilities(unlist(probs))
  assert_named(probs, type = "unique")

  .NextBestSuggestionProbs(value = value, probs = probs)
}

## default constructor ----

#' @rdname NextBestSuggestionProbs-class
#' @note Typically, end users will not use the `.DefaultNextBestSuggestionProbs()` function.
#' @export
.DefaultNextBestSuggestionProbs <- function() {
  NextBestSuggestionProbs(
    value = 1,
    probs = list(target = 0.5, overdose = 0.25)
  )
}
