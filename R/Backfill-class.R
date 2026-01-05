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

# TODO add constructor

## default constructor ----

# TODO add default constructor

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

# TODO add constructor

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
    cohort_size = CohortSize(size = 3),
    opening = .OpeningMinDose(),
    recruitment = .RecruitmentFast()
  ),
  contains = "CrmPackClass"
  # TODO add validity
)

## constructor ----

# TODO add constructor

## default constructor ----

# TODO add default constructor
