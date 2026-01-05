#' @include Backfill-class.R
#' @include helpers.R
NULL

# openCohort ----

## generic ----

# Returns a list with elements:
# - open: flag, whether to open a new cohort
# - dose: at which dose
# - cohort: which cohort index
setGeneric(
  name = "openCohort",
  def = function(object, data, dose, ...) {
    standardGeneric("openCohort")
  },
  valueClass = "list"
)

## OpeningMinDose ----

setMethod(
  f = "openCohort",
  signature = c(object = "OpeningMinDose"),
  definition = function(object, data, dose, ...) {
    previous_cohort <- h_previous_cohort(data)
    if (
      !is.na(previous_cohort$dose) &&
        previous_cohort$dose >= object@min_dose
    ) {
      list(
        open = TRUE,
        dose = previous_cohort$dose,
        cohort = previous_cohort$cohort
      )
    } else {
      list(open = FALSE, dose = NA_real_, cohort = NA_integer_)
    }
  }
)

## OpeningNone ----

setMethod(
  f = "openCohort",
  signature = c(object = "OpeningNone"),
  definition = function(object, data, dose, ...) {
    list(open = FALSE, dose = NA_real_, cohort = NA_integer_)
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
