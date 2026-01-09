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
    standardGeneric("openCohort")
  },
  valueClass = "logical"
)

## OpeningMinDose ----

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
