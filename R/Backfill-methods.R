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
    # is higher than or equal to the recommended dose, do not open it.
    cohort_dose <- h_get_dose_for_cohort(data, cohort)
    if (is.na(cohort_dose) || cohort_dose >= dose) {
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

#' @describeIn openCohort method for `OpeningMinCohorts` class.
#'
#' @aliases openCohort-OpeningMinCohorts
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningMinCohorts.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningMinCohorts"),
  definition = function(opening, cohort, data, dose, ...) {
    n_cohorts <- max(data@cohort)
    n_cohorts >= opening@min_cohorts
  }
)

## OpeningNone ----

#' @describeIn openCohort method for `OpeningNone` class.
#'
#' @aliases openCohort-OpeningNone
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningNone.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningNone"),
  definition = function(opening, cohort, data, dose, ...) {
    FALSE
  }
)

## OpeningMinResponses ----

#' @describeIn openCohort method for `OpeningMinResponses` class.
#'
#' @aliases openCohort-OpeningMinResponses
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningMinResponses.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningMinResponses"),
  definition = function(opening, cohort, data, dose, ...) {
    cohort_dose <- h_get_dose_for_cohort(data, cohort)

    # Count responses based on include_lower_doses flag
    if (opening@include_lower_doses) {
      # Count responses at cohort_dose and all lower doses
      response_count <- sum(data@response[data@x <= cohort_dose], na.rm = TRUE)
    } else {
      # Count responses only at cohort_dose
      response_count <- sum(data@response[data@x == cohort_dose], na.rm = TRUE)
    }

    response_count >= opening@min_responses
  }
)

## OpeningList ----

#' @describeIn openCohort method for `OpeningList` class.
#'
#' @aliases openCohort-OpeningList
#'
#' @param summary_fun (`function`)\cr to apply to the list of
#'   results (e.g., `all` or `any`).
#'   Only used for `OpeningList` and its subclasses.
#'
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

## OpeningAll ----

#' @describeIn openCohort method for `OpeningAll` class.
#'   Returns `TRUE` if ALL opening criteria are satisfied.
#'
#' @aliases openCohort-OpeningAll
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningAll.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningAll"),
  definition = function(opening, cohort, data, dose, ...) {
    callNextMethod(opening, cohort, data, dose, summary_fun = all, ...)
  }
)

## OpeningAny ----

#' @describeIn openCohort method for `OpeningAny` class.
#'   Returns `TRUE` if ANY opening criteria are satisfied.
#'
#' @aliases openCohort-OpeningAny
#'
#' @export
#' @example examples/Backfill-method-openCohort-OpeningAny.R
setMethod(
  f = "openCohort",
  signature = c(opening = "OpeningAny"),
  definition = function(opening, cohort, data, dose, ...) {
    callNextMethod(opening, cohort, data, dose, summary_fun = any, ...)
  }
)

## & operator ----

#' Logical AND Operator for Opening Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Combines two [`Opening`] objects with AND logic using the `&` operator.
#' This creates an [`OpeningAll`] object.
#'
#' @param e1 (`Opening`)
#'   the first opening object.
#' @param e2 (`Opening`)
#'   the second opening object.
#'
#' @return An [`OpeningAll`] object combining `e1` and `e2`.
#'
#' @seealso [`OpeningAll`] for more details.
#'
#' @export
#' @aliases &,Opening,Opening-method
setMethod(
  f = "&",
  signature = c(e1 = "Opening", e2 = "Opening"),
  definition = function(e1, e2) {
    .OpeningAll(open_list = list(e1, e2))
  }
)

## | operator ----

#' Logical OR Operator for Opening Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Combines two [`Opening`] objects with OR logic using the `|` operator.
#' This creates an [`OpeningAny`] object.
#'
#' @param e1 (`Opening`)
#'   the first opening object.
#' @param e2 (`Opening`)
#'   the second opening object.
#'
#' @return An [`OpeningAny`] object combining `e1` and `e2`.
#'
#' @seealso [`OpeningAny`] for more details.
#'
#' @export
#' @aliases |,Opening,Opening-method
setMethod(
  f = "|",
  signature = c(e1 = "Opening", e2 = "Opening"),
  definition = function(e1, e2) {
    .OpeningAny(open_list = list(e1, e2))
  }
)

# maxRecruits ----

## generic ----

#' Calculate Maximum Number of Backfill Patients
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the maximum number of backfill patients that can be recruited
#' based on the recruitment rule and the active cohort size.
#'
#' @param object (`Recruitment`)\cr the recruitment rule.
#' @param active_cohort_size (`integer`)\cr the current size of the active
#'   dose escalation cohort.
#' @param ... further arguments (not used).
#'
#' @return The maximum number of backfill patients as an `integer`.
#'
#' @seealso [`Recruitment`], [`RecruitmentUnlimited`], [`RecruitmentRatio`].
#'
setGeneric(
  name = "maxRecruits",
  def = function(object, active_cohort_size, ...) {
    standardGeneric("maxRecruits")
  },
  valueClass = "integer"
)

## RecruitmentUnlimited ----

#' @describeIn maxRecruits method for `RecruitmentUnlimited` class.
#'   Returns a very large number (practically unlimited).
#'
#' @aliases maxRecruits-RecruitmentUnlimited
#'
#' @export
#' @example examples/Backfill-method-maxRecruits-RecruitmentUnlimited.R
setMethod(
  f = "maxRecruits",
  signature = c(object = "RecruitmentUnlimited"),
  definition = function(object, active_cohort_size, ...) {
    1e6L # Practically unlimited.
  }
)

## RecruitmentRatio ----

#' @describeIn maxRecruits method for `RecruitmentRatio` class.
#'   Returns `ceiling(ratio * active_cohort_size)`.
#'
#' @aliases maxRecruits-RecruitmentRatio
#'
#' @export
#' @example examples/Backfill-method-maxRecruits-RecruitmentRatio.R
setMethod(
  f = "maxRecruits",
  signature = c(object = "RecruitmentRatio"),
  definition = function(object, active_cohort_size, ...) {
    ratio <- object@ratio
    max_recruits <- as.integer(ceiling(ratio * active_cohort_size))
    max_recruits
  }
)
