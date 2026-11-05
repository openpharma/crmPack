#' @include helpers.R
#' @include Recommendation-validity.R
#' @include CrmPackClass-class.R
NULL

# Recommendation-class ----

#' `Recommendation`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`Recommendation`] is a class for defining the recommended dose for the next
#' cohort to be treated.
#'
#' @slot value (`real`)\cr The recommended dose.
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
#' @param value (`number`)\cr The recommended dose for the next cohort
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
.DefaultRecommendationThreePlusThree <- function() {
  RecommendationThreePlusThree(
    value = 1,
    stopHere = FALSE
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

## constructor ----

#' @rdname RecommendationTD-class
#'
#' @inheritParams RecommendationDetails
#' @param ... not used.
#'
#' A mock function that does nothing
#'
#' @export

RecommendationDetails <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  Recommendation(value = value, ...)
}

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
    value = c(25, NA, 125),
    details = tibble::tibble(
      EstimateType = c("During trial", "End of trial", "Max gain"),
      TargetDose = c(30.5, 19.3, 36.2),
      LowerCI = c(NA, 19.3, 19.3),
      UpperCI = c(NA, 36.2, 219),
      CIRatio = c(NA, 1, 6.05)
    )
  )
}

# RecommendationAllocation ----

## class ----

#' `RecommendationAllocation`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationAllocation`] inherits from [`RecommendationDetails`].
#'
#' @aliases RecommendationAllocation
#' @export
#'
.RecommendationAllocation <- setClass(
  Class = "RecommendationAllocation",
  contains = "RecommendationDetails",
  validity = v_recommendationallocation
)

## constructor ----

#' @rdname RecommendationAllocation-class
#'
#' @inheritParams Recommendation
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationAllocation-class.R
#'
RecommendationAllocation <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationAllocation(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationAllocation-class
#' @note Typically, end users will not use the `.DefaultRecommendationAllocation()` function.
#' @export
.DefaultRecommendationAllocation <- function() {
  RecommendationAllocation(
    value = 3,
    details = tibble::tibble(
      Dose = c(        1.5,   2.5,   3.5,   4.5,   6.0,   7.0),
      Allocation = c(0.075, 0.373, 0.281, 0.081, 0.038, 0.152)
    )
  )
}

# RecommendationNCRMLoss ----

## class ----

#' `RecommendationNCRMLoss`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationNCRMLoss`] inherits from [`RecommendationDetails`].
#'
#' @aliases RecommendationNCRMLoss
#' @export
#'
.RecommendationNCRMLoss <- setClass(
  Class = "RecommendationNCRMLoss",
  contains = "RecommendationDetails",
  validity = v_recommendationncrmloss
)

## constructor ----

#' @rdname RecommendationNCRMLoss-class
#'
#' @inheritParams RecommendationDetails
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationNCRMLoss-class.R
#'
RecommendationNCRMLoss <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationNCRMLoss(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationNCRMLoss-class
#' @note Typically, end users will not use the `.DefaultRecommendationNCRMLoss()` function.
#' @export
.DefaultRecommendationNCRMLoss <- function() {
  RecommendationNCRMLoss(
    value = 3,
    details = tibble::tibble(
      Dose =          c(    0.1,    0.5,    1.5,      3,      6,    10,    20,    30,    40,    50,    60,    70,    80),
      Underdosing =   c(  1.000,  0.995,  0.973,  0.942,  0.888, 0.730, 0.505, 0.240, 0.108, 0.049, 0.012, 0.008, 0.005),
      Target =        c(  0.000,  0.005,  0.026,  0.057,  0.087, 0.205, 0.290, 0.409, 0.384, 0.291, 0.179, 0.136, 0.105),
      Excesive =      c(  0.000,  0.000,  0.001,  0.001,  0.025, 0.064, 0.181, 0.280, 0.384, 0.502, 0.573, 0.548, 0.507),
      Unacceptable  = c(  0.000,  0.000,  0.000,  0.000,  0.000, 0.001, 0.024, 0.071, 0.124, 0.158, 0.236, 0.308, 0.383),
      Mean          = c(0.00756, 0.0187, 0.0373, 0.0598, 0.0982, 0.143, 0.238, 0.317, 0.383, 0.439, 0.486, 0.525, 0.559),
      StdDev        = c( 0.0182, 0.0327, 0.0505, 0.0671, 0.0892, 0.109, 0.139, 0.154, 0.161, 0.164, 0.166, 0.167, 0.168),
      PosteriorLoss = c( 1.0000,  0.995,  0.974,  0.943,  0.913, 0.796, 0.734, 0.662, 0.740, 0.867, 1.060, 1.170, 1.280)
    )
  )
}

# RecommendationProbs ----

## class ----

#' `RecommendationProbs`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationProbs`] inherits from [`RecommendationDetails`]
#'
#' @slot details (`tibble`)\cr the probabilities associated with the suggestion.
#'
#' @aliases RecommendationProbs
#' @export
#'
.RecommendationProbs <- setClass(
  Class = "RecommendationProbs",
  contains = "RecommendationDetails",
  prototype = prototype(
    backfilled = logical(),
    details = tibble::tibble()
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
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationProbs(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationProbs-class
#' @note Typically, end users will not use the `.DefaultRecommendationProbs()` function.
#' @export
.DefaultRecommendationProbs <- function() {
  RecommendationProbs(
    value = 1,
    details = tibble::tibble(Dose = 1, Target = 0.5, Overdose = 0.25)
  )
}

## class ----

#' `RecommendationProbsDataCombo`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecommendationProbsDataCombo`] inherits from [`RecommendationProbs`].
#' It differs from [`RecommendationProbs`] because of the need to validate
#' differently.
#'
#'
#' @aliases RecommendationProbsDualEndpoint
#' @export
#'
.RecommendationProbsDualEndpoint <- setClass(
  Class = "RecommendationProbsDualEndpoint",
  contains = "RecommendationDetails",
  prototype = prototype(
    backfilled = logical(),
    detail = tibble::tibble()
  ),
  validity = v_recommendationprobsdualendpoint
)

## constructor ----

#' @rdname RecommendationProbsDualEndpoint-class
#'
#' @inheritParams Recommendation
#' @param details (`tbl_df`)\cr a tibble containing columns for dose and probabilties
#' of toxicity and target efficacy.
#' @param ... not used.
#'
#' @export
#' @example examples/RecommendationProbs-class.R
#'
RecommendationProbsDualEndpoint <- function(
    value = numeric(),
    details = tibble::tibble(),
    ...
) {
  assert_numeric(value)

  .RecommendationProbsDualEndpoint(value = value, details = details)
}

## default constructor ----

#' @rdname RecommendationProbsDualEndpoint-class
#' @note Typically, end users will not use the `.DefaultRecommendationProbsDualEndpoint()` function.
#' @export
.DefaultRecommendationProbsDualEndpoint <- function() {
  RecommendationProbsDualEndpoint(
    value = 1,
    details = tibble::tibble(Dose = 1, Target = 0.5, Overdose = 0.75)
  )
}
