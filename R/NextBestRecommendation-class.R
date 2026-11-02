#' @include helpers.R
#' @include NextBestRecommendation-validity.R
#' @include CrmPackClass-class.R
NULL

# NextBestRecommendation-class ----

#' `NextBestRecommendation`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestRecommendation`] is a class for defining the suggested dose for the next
#' cohort to be treated.
#'
#' @slot value (`real`)\cr The suggested dose.
#'
#' @aliases NextBestRecommendation
#' @export
#'
.NextBestRecommendation <- setClass(
  Class = "NextBestRecommendation",
  slots = c(
    value = "numeric"
  ),
  prototype = prototype(
    list(value = NA_real_)
  ),
  contains = "CrmPackClass",
  validity = v_nextbestrecommendation
)

## constructor ----

#' @rdname NextBestRecommendation-class
#' @param value (`number`)\crthe suggested dose for the next cohort
#' @export
NextBestRecommendation <- function(value) {
  .NextBestRecommendation(value = value)
}

## default constructor ----

#' @rdname NextBestRecommendation-class
#' @note Typically, end users will not use the `.DefaultNextBestRecommendation()` function.
#' @export
.DefaultNextBestRecommendation <- function() {
  .NextBestRecommendation(value = 1)
}

# NextBestRecommendationProbs ----

## class ----

#' `NextBestRecommendationProbs`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestRecommendationProbs`] inherits from [`NextBestRecommendation`] and adds a
#' slot in which probabilities associated with the suggestion can be stored.
#'
#' @slot probs (`list`)\cr the probabilities associated with the suggestion.
#'
#' @aliases NextBestRecommendationProbs
#' @export
#'
.NextBestRecommendationProbs <- setClass(
  Class = "NextBestRecommendationProbs",
  contains = "NextBestRecommendation",
  slots = c(
    probs = "tbl_df"
  ),
  prototype = prototype(
    backfilled = logical(),
    probs = tibble::tibble()
  ),
  validity = v_nextbestrecommendationprobs
)

## constructor ----

#' @rdname NextBestRecommendationProbs-class
#'
#' @inheritParams NextBestRecommendation
#' @param probs (`list`)\cr ta named list of probabilities.
#' @param ... not used.
#'
#' @export
#' @example examples/NextBestRecommendationProbs-class.R
#'
NextBestRecommendationProbs <- function(
  value = numeric(),
  probs = tibble::tibble(),
  ...
) {
  assert_numeric(value)

  .NextBestRecommendationProbs(value = value, probs = probs)
}

## default constructor ----

#' @rdname NextBestRecommendationProbs-class
#' @note Typically, end users will not use the `.DefaultNextBestRecommendationProbs()` function.
#' @export
.DefaultNextBestRecommendationProbs <- function() {
  NextBestRecommendationProbs(
    value = 1,
    probs = tibble::tibble(Dose = 1, target = 0.5, overdose = 0.25)
  )
}


# NextBestRecommendationThreePlusThree ----

## class ----

#' `NextBestRecommendationThreePlusThree`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NextBestRecommendationThreePlusThree`] inherits from [`NextBestRecommendation`]
#' and adds a slot in which the StopGo decision can be recorded.
#'
#' @slot stopHere (`flag`)\cr the StopGo flag.
#'
#' @aliases NextBestRecommendationThreePlusThree
#' @export
#'
.NextBestRecommendationThreePlusThree <- setClass(
  Class = "NextBestRecommendationThreePlusThree",
  contains = "NextBestRecommendation",
  slots = c(
    stopHere = "logical"
  ),
  prototype = prototype(
    stopHere = logical()
  ),
  validity = v_nextbestrecommendationthreeplusthree
)

## constructor ----

#' @rdname NextBestRecommendationThreePlusThree-class
#'
#' @inheritParams NextBestRecommendation
#' @param stopHere (`flag`)\cr the StopGo flag.
#' @param ... not used.
#'
#' @export
#' @example examples/NextBestRecommendationThreePlusThree-class.R
#'
NextBestRecommendationThreePlusThree <- function(
    value = numeric(),
    stopHere = FALSE,
    ...
) {
  assert_numeric(value)

  .NextBestRecommendationThreePlusThree(value = value, stopHere = stopHere)
}

## default constructor ----

#' @rdname NextBestRecommendationThreePlusThree-class
#' @note Typically, end users will not use the `.DefaultNextBestRecommendationThreePlusThree()` function.
#' @export
.DefaultNextBestRecommendationProbs <- function() {
  NextBestRecommendationProbs(
    value = 1,
    stopHere = FALSE
  )
}
