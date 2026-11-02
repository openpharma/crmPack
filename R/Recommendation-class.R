#' @include helpers.R
#' @include Recommendation-validity.R
#' @include CrmPackClass-class.R
NULL

# Recommendation-class ----

#' `Recommendation`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`Recommendation`] is a class for defining the suggested dose for the next
#' cohort to be treated.
#'
#' @slot value (`real`)\cr The suggested dose.
#'
#' @aliases Recommendation
#' @export
#'
.Recommendation <- setClass(
  Class = "Recommendation",
  slots = c(
    value = "numeric"
  ),
  prototype = prototype(
    list(value = NA_real_)
  ),
  contains = "CrmPackClass",
  validity = v_recommendation
)

## constructor ----

#' @rdname Recommendation-class
#' @param value (`number`)\crthe suggested dose for the next cohort
#' @export
Recommendation <- function(value) {
  .Recommendation(value = value)
}

## default constructor ----

#' @rdname Recommendation-class
#' @note Typically, end users will not use the `.DefaultRecommendation()` function.
#' @export
.DefaultRecommendation <- function() {
  .Recommendation(value = 1)
}

# RecommendationProbs ----

## class ----

#' `RecommendationProbs`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationProbs`] inherits from [`Recommendation`] and adds a
#' slot in which probabilities associated with the suggestion can be stored.
#'
#' @slot probs (`list`)\cr the probabilities associated with the suggestion.
#'
#' @aliases RecommendationProbs
#' @export
#'
.RecommendationProbs <- setClass(
  Class = "RecommendationProbs",
  contains = "Recommendation",
  slots = c(
    probs = "tbl_df"
  ),
  prototype = prototype(
    backfilled = logical(),
    probs = tibble::tibble()
  ),
  validity = v_recommendationprobs
)

## constructor ----

#' @rdname RecommendationProbs-class
#'
#' @inheritParams Recommendation
#' @param probs (`tbl_df`)\cr a tibble containing columns for dose and probabilties
#' of toxicity.  The toxicity grade name is given by the column name.
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationProbs-class.R
#'
RecommendationProbs <- function(
  value = numeric(),
  probs = tibble::tibble(),
  ...
) {
  assert_numeric(value)

  .RecommendationProbs(value = value, probs = probs)
}

## default constructor ----

#' @rdname RecommendationProbs-class
#' @note Typically, end users will not use the `.DefaultRecommendationProbs()` function.
#' @export
.DefaultRecommendationProbs <- function() {
  RecommendationProbs(
    value = 1,
    probs = tibble::tibble(Dose = 1, target = 0.5, overdose = 0.25)
  )
}


# RecommendationThreePlusThree ----

## class ----

#' `RecommendationThreePlusThree`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationThreePlusThree`] inherits from [`Recommendation`]
#' and adds a slot in which the StopGo decision can be recorded.
#'
#' @slot stopHere (`flag`)\cr the StopGo flag.
#'
#' @aliases RecommendationThreePlusThree
#' @export
#'
.RecommendationThreePlusThree <- setClass(
  Class = "RecommendationThreePlusThree",
  contains = "Recommendation",
  slots = c(
    stopHere = "logical"
  ),
  prototype = prototype(
    stopHere = logical()
  ),
  validity = v_recommendationthreeplusthree
)

## constructor ----

#' @rdname RecommendationThreePlusThree-class
#'
#' @inheritParams Recommendation
#' @param stopHere (`flag`)\cr the StopGo flag.
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationThreePlusThree-class.R
#'
RecommendationThreePlusThree <- function(
    value = numeric(),
    stopHere = FALSE,
    ...
) {
  assert_numeric(value)

  .RecommendationThreePlusThree(value = value, stopHere = stopHere)
}

## default constructor ----

#' @rdname RecommendationThreePlusThree-class
#' @note Typically, end users will not use the `.DefaultRecommendationThreePlusThree()` function.
#' @export
.DefaultRecommendationProbs <- function() {
  RecommendationProbs(
    value = 1,
    stopHere = FALSE
  )
}

# RecommendationProbs ----

## class ----

#' `RecommendationProbs`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationProbs`] inherits from [`Recommendation`] and adds a
#' slot in which probabilities associated with the suggestion can be stored.
#'
#' @slot probs (`list`)\cr the probabilities associated with the suggestion.
#'
#' @aliases RecommendationProbs
#' @export
#'
.RecommendationProbs <- setClass(
  Class = "RecommendationProbs",
  contains = "Recommendation",
  slots = c(
    probs = "tbl_df"
  ),
  prototype = prototype(
    backfilled = logical(),
    probs = tibble::tibble()
  ),
  validity = v_recommendationprobs
)

## constructor ----

#' @rdname RecommendationProbs-class
#'
#' @inheritParams Recommendation
#' @param probs (`tbl_df`)\cr a tibble containing columns for dose and probabilties
#' of toxicity.  The toxicity grade name is given by the column name.
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationProbs-class.R
#'
RecommendationProbs <- function(
    value = numeric(),
    probs = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationProbs(value = value, probs = probs)
}

## default constructor ----

#' @rdname RecommendationProbs-class
#' @note Typically, end users will not use the `.DefaultRecommendationProbs()` function.
#' @export
.DefaultRecommendationProbs <- function() {
  RecommendationProbs(
    value = 1,
    probs = tibble::tibble(Dose = 1, target = 0.5, overdose = 0.25)
  )
}

# RecommendationDetails ----

## class ----

#' `RecommendationDetails`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationDetails`] inherits from [`Recommendation`] and adds a
#' slot in which arbitrary additional details can be stored in a tibble.  This
#' class is a virtual parent class.  Subclasses define class-specific validation
#' rules beyond "`details` must be a tibble".
#'
#' @slot details (`tbl_df`)\cr class-specific additional information.
#'
#' @aliases RecommendationDetails
#' @export
#'
.RecommendationDetails <- setClass(
  Class = "RecommendationDetails",
  contains = c("Recommendation", "VIRTUAL"),
  slots = c(
    details = "tbl_df"
  ),
  prototype = prototype(
    backfilled = logical(),
    details = tibble::tibble()
  ),
  validity = v_recommendationdetails
)

# RecommendationTD ----

## class ----

#' `RecommendationTD`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationTD`] inherits from [`RecommendationDetails`].
#'
#' @aliases RecommendationTD
#' @export
#'
.RecommendationTD <- setClass(
  Class = "RecommendationTD",
  contains = "RecommendationDetails",
  validity = v_recommendationtd
)

## constructor ----

#' @rdname RecommendationTD-class
#'
#' @inheritParams RecommendationDetails
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationProbs-class.R
#'
RecommendationTD <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationTD(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationTD-class
#' @note Typically, end users will not use the `.DefaultRecommendationTD()` function.
#' @export
.DefaultRecommendationTD <- function() {
  RecommendationTD(
    value = c(75.0, 75.0),
    details = tibble::tibble(
      Time  = c("During trial", "End of Trial"),
      TargetDose = c(99.8, 81.5),
      LowerCI = c(NA, 24.3),
      UpperCI = c(NA, 273.0),
      CIRatio = c(NA, 11.2)
    )
  )
}

# RecommendationMaxGain ----

## class ----

#' `RecommendationMaxGain`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationMaxGain`] inherits from [`RecommendationDetails`].
#'
#' @aliases RecommendationMaxGain
#' @export
#'
.RecommendationMaxGain <- setClass(
  Class = "RecommendationMaxGain",
  contains = "RecommendationDetails",
  validity = v_recommendationmaxgain
)

## constructor ----

#' @rdname RecommendationMaxGain-class
#'
#' @inheritParams RecommendationDetails
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationMaxGain-class.R
#'
RecommendationMaxGain <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationMaxGain(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationMaxGain-class
#' @note Typically, end users will not use the `.DefaultRecommendationMaxGain()` function.
#' @export
.DefaultRecommendationMaxGain <- function() {
  RecommendationMaxGain(
    value = c(75.0, 75.0),
    details = tibble::tibble(
      Time  = c("During trial", "End of Trial"),
      TargetDose = c(99.8, 81.5),
      LowerCI = c(NA, 24.3),
      UpperCI = c(NA, 273.0),
      CIRatio = c(NA, 11.2)
    )
  )
}
