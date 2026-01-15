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
#' @seealso [`OpeningMinDose`], [`OpeningMinCohorts`], [`OpeningNone`],
#'   [`OpeningMinResponses`], [`OpeningAll`], [`OpeningAny`].
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

# OpeningMinResponses ----

## class ----

#' `OpeningMinResponses`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningMinResponses`] opens backfill cohorts when a minimum number of
#' responses has been observed in the trial. The responses can be counted
#' at the cohort's dose level only, or also at lower dose levels if
#' `include_lower_doses` is set to `TRUE`.
#'
#' @slot min_responses (`count`)\cr the minimum number of responses
#'   required before backfill cohorts can be opened (at least 1).
#' @slot include_lower_doses (`logical`)\cr if `TRUE`, responses at all
#'   doses less than or equal to the cohort's dose are counted. If `FALSE`,
#'   only responses at the cohort's dose are counted.
#'
#' @seealso [`Opening`] and the other subclasses listed in there.
#'
#' @aliases OpeningMinResponses
#' @export
#'
.OpeningMinResponses <- setClass(
  Class = "OpeningMinResponses",
  slots = c(
    min_responses = "integer",
    include_lower_doses = "logical"
  ),
  prototype = list(
    min_responses = 1L,
    include_lower_doses = FALSE
  ),
  contains = "Opening",
  validity = v_opening_min_responses
)

## constructor ----

#' @rdname OpeningMinResponses-class
#'
#' @param min_responses (`count`)\cr see slot definition.
#' @param include_lower_doses (`logical`)\cr see slot definition.
#'
#' @export
#' @example examples/Backfill-class-OpeningMinResponses.R
OpeningMinResponses <- function(
  min_responses = 1L,
  include_lower_doses = FALSE
) {
  assert_count(min_responses, positive = TRUE)
  assert_flag(include_lower_doses)
  min_responses <- as.integer(min_responses)
  .OpeningMinResponses(
    min_responses = min_responses,
    include_lower_doses = include_lower_doses
  )
}

## default constructor ----

#' @rdname OpeningMinResponses-class
#' @note Typically, end users will not use the `.DefaultOpeningMinResponses()` function.
#' @export
.DefaultOpeningMinResponses <- function() {
  OpeningMinResponses()
}


# OpeningList and logical operators ----

## OpeningList ----

#' `OpeningList`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningList`] is a virtual class for combining multiple [`Opening`] objects
#' using logical operators. It is used as a base class for [`OpeningAll`] and
#' [`OpeningAny`].
#'
#' @slot open_list (`list`)
#'   a list of [`Opening`] objects to be combined.
#'
#' @seealso [`Opening`], [`OpeningAll`], [`OpeningAny`].
#'
#' @aliases OpeningList
#' @export
#'
.OpeningList <- setClass(
  Class = "OpeningList",
  contains = "Opening",
  slots = list(open_list = "list")
)

## constructor ----

#' @rdname OpeningList-class
#'
#' @param ... (`Opening`)\cr opening objects to combine.
#' @export
#' @example examples/Backfill-class-OpeningList.R
OpeningList <- function(...) {
  args <- list(...)
  assert_list(args, min.len = 1)
  for (arg in args) {
    assert_class(arg, "Opening")
  }
  .OpeningList(open_list = args)
}

## OpeningAll ----

#' `OpeningAll`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningAll`] combines multiple [`Opening`] objects using AND logic.
#' A backfill cohort is opened only if ALL opening criteria in the list
#' are satisfied. This can also be created using the `&` operator.
#'
#' @slot open_list (`list`)
#'   a list of [`Opening`] objects to be combined with AND logic.
#'
#' @seealso [`Opening`], [`OpeningAny`], [`OpeningList`].
#'
#' @aliases OpeningAll
#' @export
#'
.OpeningAll <- setClass(
  Class = "OpeningAll",
  contains = "OpeningList"
)

## constructor ----

#' @rdname OpeningAll-class
#'
#' @param ... (`Opening`)
#'   opening objects to combine with AND logic.
#'
#' @export
#' @example examples/Backfill-class-OpeningAll.R
OpeningAll <- function(...) {
  start <- OpeningList(...)
  .OpeningAll(start)
}

## default constructor ----

#' @rdname OpeningAll-class
#' @note Typically, end users will not use the `.DefaultOpeningAll()` function.
#' @export
.DefaultOpeningAll <- function() {
  OpeningAll(OpeningMinDose(), OpeningMinDose())
}

## OpeningAny ----

#' `OpeningAny`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OpeningAny`] combines multiple [`Opening`] objects using OR logic.
#' A backfill cohort is opened if ANY opening criteria in the list
#' are satisfied. This can also be created using the `|` operator.
#'
#' @slot open_list (`list`)\cr a list of [`Opening`] objects to be
#'   combined with OR logic.
#'
#' @seealso [`Opening`], [`OpeningAll`], [`OpeningList`].
#'
#' @aliases OpeningAny
#' @export
#'
.OpeningAny <- setClass(
  Class = "OpeningAny",
  contains = "OpeningList"
)

## constructor ----

#' @rdname OpeningAny-class
#'
#' @param ... (`Opening`)
#'   opening objects to combine with OR logic.
#'
#' @export
#' @example examples/Backfill-class-OpeningAny.R
OpeningAny <- function(...) {
  start <- OpeningList(...)
  .OpeningAny(start)
}

## default constructor ----

#' @rdname OpeningAny-class
#' @note Typically, end users will not use the `.DefaultOpeningAny()` function.
#' @export
.DefaultOpeningAny <- function() {
  OpeningAny(OpeningMinDose(), OpeningMinDose())
}

# Recruitment ----

## class ----

#' `Recruitment`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`Recruitment`] is a virtual class for recruitment criteria, from which all
#' other specific recruitment criteria classes inherit.
#' The subclasses are used to specify the maximum number of backfill patients
#' that can be recruited relative to the main trial cohort size.
#'
#' @seealso [`RecruitmentUnlimited`], [`RecruitmentRatio`].
#'
#' @aliases Recruitment
#' @export
#'
.Recruitment <- setClass(
  Class = "Recruitment",
  contains = "CrmPackClass"
)

## default constructor ----

#' @rdname Recruitment-class
#' @note Typically, end users will not use the `.DefaultRecruitment()` function.
#' @export
.DefaultRecruitment <- function() {
  stop(
    paste(
      "Class Recruitment should not be instantiated directly.",
      "Please use one of its subclasses instead."
    )
  )
}

# RecruitmentUnlimited ----

## class ----

#' `RecruitmentUnlimited`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecruitmentUnlimited`] allows unlimited recruitment of backfill patients.
#' There is no constraint on the number of backfill patients relative to the
#' main trial cohort size.
#'
#' @seealso [`Recruitment`] and the other subclasses listed in there.
#'
#' @aliases RecruitmentUnlimited
#' @export
#'
.RecruitmentUnlimited <- setClass(
  Class = "RecruitmentUnlimited",
  contains = "Recruitment"
)

## constructor ----

#' @rdname RecruitmentUnlimited-class
#'
#' @export
#' @example examples/Backfill-class-RecruitmentUnlimited.R
RecruitmentUnlimited <- function() {
  .RecruitmentUnlimited()
}

## default constructor ----

#' @rdname RecruitmentUnlimited-class
#' @note Typically, end users will not use the `.DefaultRecruitmentUnlimited()` function.
#' @export
.DefaultRecruitmentUnlimited <- function() {
  RecruitmentUnlimited()
}

# RecruitmentRatio ----

## class ----

#' `RecruitmentRatio`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`RecruitmentRatio`] constrains the recruitment of backfill patients
#' based on a ratio to the main trial cohort size. The maximum number of
#' backfill patients is calculated as `ceiling(ratio * active_cohort_size)`.
#'
#' @slot ratio (`number`)
#'   the recruitment ratio, specifying the maximum number of backfill patients
#'   per patient in the main trial cohort (non-negative).
#'
#' @seealso [`Recruitment`] and the other subclasses listed in there.
#'
#' @aliases RecruitmentRatio
#' @export
#'
.RecruitmentRatio <- setClass(
  Class = "RecruitmentRatio",
  contains = "Recruitment",
  slots = list(ratio = "numeric"),
  validity = v_recruitment_ratio
)

## constructor ----

#' @rdname RecruitmentRatio-class
#'
#' @param ratio (`number`)
#'   see slot definition.
#'
#' @export
#' @example examples/Backfill-class-RecruitmentRatio.R
RecruitmentRatio <- function(ratio = 1) {
  assert_numeric(ratio, len = 1, lower = 0)
  .RecruitmentRatio(ratio = ratio)
}

## default constructor ----

#' @rdname RecruitmentRatio-class
#' @note Typically, end users will not use the `.DefaultRecruitmentRatio()` function.
#' @export
.DefaultRecruitmentRatio <- function() {
  RecruitmentRatio()
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
