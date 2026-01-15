#' @include helpers.R
#' @include Rules-class.R
#' @include CrmPackClass-class.R
#' @include Backfill-validity.R
NULL

# Opening ----

## class ----

#' `Opening`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`Opening`] is a virtual class for opening criteria, from which all
#' other specific opening criteria classes inherit.
#' The subclasses are used for backfill cohort designs.
#'
#' @seealso [`OpeningMinDose`], [`OpeningMinCohorts`], [`OpeningNone`].
#'
#' @aliases Opening
#' @export
#'
.Opening <- setClass(
  Class = "Opening",
  contains = "CrmPackClass"
)

## default constructor ----

#' @rdname Opening-class
#' @note Typically, end users will not use the `.DefaultOpening()` function.
#' @export
.DefaultOpening <- function() {
  stop(
    paste(
      "Class Opening should not be instantiated directly.",
      "Please use one of its subclasses instead."
    )
  )
}

# OpeningMinDose ----

## class ----

#' `OpeningMinDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningMinDose`] opens a backfill cohort when the
#' cohort's dose is above or equal to the minimum dose specified.
#' Note that the next recommended dose is not taken into account.
#'
#' @slot min_dose (`number`)\cr the minimum dose at which backfill
#'   cohorts can be opened.
#'
#' @seealso [`Opening`] and the other subclasses listed in there.
#'
#' @aliases OpeningMinDose
#' @export
.OpeningMinDose <- setClass(
  Class = "OpeningMinDose",
  slots = c(min_dose = "numeric"),
  prototype = list(min_dose = 0),
  contains = "Opening",
  validity = v_opening_min_dose
)

## constructor ----

#' @rdname OpeningMinDose-class
#'
#' @param min_dose (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Backfill-class-OpeningMinDose.R
OpeningMinDose <- function(min_dose = 0) {
  .OpeningMinDose(min_dose = min_dose)
}

## default constructor ----

#' @rdname OpeningMinDose-class
#' @note Typically, end users will not use the `.DefaultOpeningMinDose()` function.
#' @export
.DefaultOpeningMinDose <- function() {
  OpeningMinDose()
}

# OpeningMinCohorts ----

## class ----

#' `OpeningMinCohorts`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningMinCohorts`] opens backfill cohorts when the overall number of
#' cohorts treated so far in the trial reaches or exceeds a minimum threshold.
#' This can be used to implement a "delayed backfill cohort opening" rule.
#'
#' @slot min_cohorts (`integer`)\cr the minimum number of cohorts that must
#'   have been treated before backfilling can be opened.
#'
#' @seealso [`Opening`] and the other subclasses listed in there.
#'
#' @aliases OpeningMinCohorts
#' @export
#'
.OpeningMinCohorts <- setClass(
  Class = "OpeningMinCohorts",
  slots = c(min_cohorts = "integer"),
  prototype = list(min_cohorts = 2L),
  contains = "Opening",
  validity = v_opening_min_cohorts
)

## constructor ----

#' @rdname OpeningMinCohorts-class
#'
#' @param min_cohorts (`integer`)\cr see slot definition.
#'
#' @export
#'
OpeningMinCohorts <- function(min_cohorts = 2L) {
  assert_integerish(min_cohorts, len = 1, lower = 1)
  min_cohorts <- as.integer(min_cohorts)
  .OpeningMinCohorts(min_cohorts = min_cohorts)
}

## default constructor ----

#' @rdname OpeningMinCohorts-class
#' @note Typically, end users will not use the `.DefaultOpeningMinCohorts()` function.
#' @export
.DefaultOpeningMinCohorts <- function() {
  OpeningMinCohorts()
}

# OpeningNone ----

## class ----

#' `OpeningNone`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningNone`] never opens any backfill cohorts. This can be used when
#' no backfill cohorts should be available in a trial design.
#'
#' @aliases OpeningNone
#' @export
#'
.OpeningNone <- setClass(
  Class = "OpeningNone",
  contains = "Opening"
)

## constructor ----

#' @rdname OpeningNone-class
#'
#' @export
#'
OpeningNone <- function() {
  .OpeningNone()
}

## default constructor ----

#' @rdname OpeningNone-class
#' @note Typically, end users will not use the `.DefaultOpeningNone()` function.
#' @export
.DefaultOpeningNone <- function() {
  OpeningNone()
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

# RecruitmentRatio ----

## class ----

.RecruitmentRatio <- setClass(
  Class = "RecruitmentRatio",
  contains = "Recruitment",
  slots = list(ratio = "numeric")
)

## constructor ----

RecruitmentRatio <- function(ratio = 1) {
  assert_numeric(ratio, len = 1, lower = 0)
  .RecruitmentRatio(ratio = ratio)
}

## default constructor ----

# TODO add default constructor

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
