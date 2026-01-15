#' @include Backfill-class.R
#' @include helpers.R
NULL

# openCohort ----

## generic ----

#' Open / recruit backfill patients into a cohort?
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param opening (`Opening`)\cr opening rule to be applied.
#' @param cohort (`int`)\cr backfill cohort index.
#' @param data (`Data`)\cr current trial data.
#' @param dose (`numeric`)\cr the recommended next best dose.
#' @param ... further arguments (not used).
#'
#' @return `TRUE` if this backfill cohort can be opened / recruited into,
#' `FALSE` otherwise.
setGeneric(
  name = "openCohort",
  def = function(opening, cohort, data, dose, ...) {
    # For any opening rule, if the dose of the cohort to be opened
    # is higher than the recommended dose, do not open it.
    cohort_dose <- h_get_dose_for_cohort(data, cohort)
    if (cohort_dose > dose) {
      return(FALSE)
    }
    standardGeneric("openCohort")
  },
  valueClass = "logical"
)

## OpeningMinDose ----

#' @describeIn openCohort method for `OpeningMinDose` class.
#'
#' @aliases openCohort-OpeningMinDose
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningMinDose.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningMinDose"),
  definition = function(opening, cohort, data, dose, ...) {
    cohort_dose <- h_get_dose_for_cohort(data, cohort)
    (!is.na(cohort_dose) &&
      cohort_dose >= opening@min_dose)
  }
)

## OpeningMinCohorts ----

setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningMinCohorts"),
  definition = function(opening, cohort, data, dose, ...) {
    n_cohorts <- max(data@cohort)
    n_cohorts >= opening@min_cohorts
  }
)

## OpeningNone ----

setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningNone"),
  definition = function(opening, cohort, data, dose, ...) {
    FALSE
  }
)

## Logical operators ----

.OpeningList <- setClass(
  Class = "OpeningList",
  contains = "Opening",
  slots = list(open_list = "list")
)

.OpeningAll <- setClass(
  Class = "OpeningAll",
  contains = "OpeningList"
)

.OpeningAny <- setClass(
  Class = "OpeningAny",
  contains = "OpeningList"
)

setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningList"),
  definition = function(opening, cohort, data, dose, summary_fun, ...) {
    list_results <- vapply(
      opening@open_list,
      FUN = function(op) openCohort(op, cohort, data, dose, ...),
      FUN.VALUE = logical(1)
    )
    summary_fun(list_results)
  }
)

setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningAll"),
  definition = function(opening, cohort, data, dose, ...) {
    callNextMethod(opening, cohort, data, dose, summary_fun = all, ...)
  }
)

setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningAny"),
  definition = function(opening, cohort, data, dose, ...) {
    callNextMethod(opening, cohort, data, dose, summary_fun = any, ...)
  }
)

setMethod(
  f = "&",
  signature = c(e1 = "Opening", e2 = "Opening"),
  definition = function(e1, e2) {
    .OpeningAll(open_list = list(e1, e2))
  }
)

setMethod(
  f = "|",
  signature = c(e1 = "Opening", e2 = "Opening"),
  definition = function(e1, e2) {
    .OpeningAny(open_list = list(e1, e2))
  }
)

# maxRecruits ----

## generic ----

setGeneric(
  name = "maxRecruits",
  def = function(object, active_cohort_size, ...) {
    standardGeneric("maxRecruits")
  },
  valueClass = "integer"
)

## RecruitmentUnlimited ----

setMethod(
  f = "maxRecruits",
  signature = c(object = "RecruitmentUnlimited"),
  definition = function(object, active_cohort_size, ...) {
    1e6L # Practically unlimited.
  }
)

## RecruitmentRatio ----

setMethod(
  f = "maxRecruits",
  signature = c(object = "RecruitmentRatio"),
  definition = function(object, active_cohort_size, ...) {
    ratio <- object@ratio
    max_recruits <- as.integer(ceiling(ratio * active_cohort_size))
    max_recruits
  }
)
