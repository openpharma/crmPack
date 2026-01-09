#' @include helpers.R
#' @include Rules-class.R
#' @include CrmPackClass-class.R
NULL

# Opening ----

## class ----

.Opening <- setClass(
  Class = "Opening",
  contains = "CrmPackClass"
)

## default constructor ----

# TODO add default constructor

# OpeningMinDose ----

## class ----

.OpeningMinDose <- setClass(
  Class = "OpeningMinDose",
  slots = c(min_dose = "numeric"),
  prototype = list(min_dose = 0),
  contains = "Opening"
  # TODO add validity
)

## constructor ----

OpeningMinDose <- function(min_dose = 0) {
  .OpeningMinDose(min_dose = min_dose)
}

## default constructor ----

# TODO add default constructor

# OpeningMinCohorts ----

## class ----

# This concerns the overall number of cohorts treated so far in the trial.
# Therefore it is one way to implement a "delayed backfill cohort opening" rule.
.OpeningMinCohorts <- setClass(
  Class = "OpeningMinCohorts",
  slots = c(min_cohorts = "integer"),
  prototype = list(min_cohorts = 2L),
  contains = "Opening"
  # TODO add validity
)

## constructor ----

OpeningMinCohorts <- function(min_cohorts = 2L) {
  assert_integerish(min_cohorts, len = 1, lower = 1)
  min_cohorts <- as.integer(min_cohorts)
  .OpeningMinCohorts(min_cohorts = min_cohorts)
}

## default constructor ----

# TODO add default constructor

# OpeningNone ----

## class ----

.OpeningNone <- setClass(
  Class = "OpeningNone",
  contains = "Opening"
)

## constructor ----

OpeningNone <- function() {
  .OpeningNone()
}

# Recruitment ----

## class ----

.Recruitment <- setClass(
  Class = "Recruitment",
  contains = "CrmPackClass"
)

## default constructor ----

# TODO add default constructor

# RecruitmentUnlimited ----

## class ----

.RecruitmentUnlimited <- setClass(
  Class = "RecruitmentUnlimited",
  contains = "Recruitment"
)

## constructor ----

RecruitmentUnlimited <- function() {
  .RecruitmentUnlimited()
}

# Backfill ----

## class ----

#' `Backfill` class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Class representing a backfilling rule in a clinical trial design.
#'
#' @slot cohort_size (`CohortSize`)\cr the size of cohorts to be backfilled.
#' @slot opening (`Opening`)\cr the opening criteria for backfilling.
#' @slot recruitment (`Recruitment`)\cr recruitment criteria for backfilling.
#' @slot total_size (`integer`)\cr the total number of patients to be backfilled.
#' @slot priority (`character`)\cr the priority rule for backfilling,
#'   one of "highest", "lowest", or "random".
#'
#' @aliases Backfill
#' @export
.Backfill <- setClass(
  Class = "Backfill",
  slots = list(
    cohort_size = "CohortSize",
    opening = "Opening",
    recruitment = "Recruitment",
    total_size = "integer",
    priority = "character"
  ),
  prototype = prototype(
    cohort_size = CohortSizeConst(size = 3),
    opening = .OpeningMinDose(),
    recruitment = .RecruitmentUnlimited(),
    total_size = 1e6L,
    priority = "highest"
  ),
  contains = "CrmPackClass"
  # TODO add validity
)

## constructor ----

#' @rdname Backfill-class
#'
#' @param cohort_size (`CohortSize`)\cr see the slot definition.
#' @param opening (`Opening`)\cr see the slot definition.
#' @param recruitment (`Recruitment`)\cr see the slot definition.
#' @param total_size (`numeric`)\cr see the slot definition.
#' @param priority (`character`)\cr see the slot definition.
#' @return An object of class `Backfill`.
#'
#' @export
Backfill <- function(
  cohort_size = CohortSizeConst(size = 3),
  opening = OpeningMinDose(),
  recruitment = RecruitmentUnlimited(),
  total_size = 1e6L,
  priority = c("highest", "lowest", "random")
) {
  assert_integerish(total_size, len = 1, lower = 1)
  total_size <- as.integer(total_size)
  priority <- match.arg(priority)

  .Backfill(
    cohort_size = cohort_size,
    opening = opening,
    recruitment = recruitment,
    total_size = total_size,
    priority = priority
  )
}

## default constructor ----

# TODO add default constructor
