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

# RecruitmentFast ----

## class ----

.RecruitmentFast <- setClass(
  Class = "RecruitmentFast",
  contains = "Recruitment"
)

## constructor ----

RecruitmentFast <- function() {
  .RecruitmentFast()
}

# Backfill ----

## class ----

.Backfill <- setClass(
  Class = "Backfill",
  slots = list(
    cohort_size = "CohortSize",
    opening = "Opening",
    recruitment = "Recruitment"
  ),
  prototype = prototype(
    cohort_size = CohortSizeConst(size = 3),
    opening = .OpeningMinDose(),
    recruitment = .RecruitmentFast()
  ),
  contains = "CrmPackClass"
  # TODO add validity
)

## constructor ----

Backfill <- function(
  cohort_size = CohortSizeConst(size = 3),
  opening = OpeningMinDose(),
  recruitment = RecruitmentFast()
) {
  .Backfill(
    cohort_size = cohort_size,
    opening = opening,
    recruitment = recruitment
  )
}

## default constructor ----

# TODO add default constructor
