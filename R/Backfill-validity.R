# Opening ----

#' Internal Helper Functions for Validation of [`Opening`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Opening`] or inherited classes and therefore not exported.
#'
#' @name v_opening
#' @param object (`Opening`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_opening validates that the [`OpeningMinDose`] object
#'   contains valid `min_dose` slot.
v_opening_min_dose <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@min_dose, lower = 0),
    "min_dose needs to be a non-negative numeric scalar"
  )
  v$result()
}
#' @describeIn v_opening validates that the [`OpeningMinCohorts`] object
#'   contains valid `min_cohorts` slot.
v_opening_min_cohorts <- function(object) {
  v <- Validate()
  v$check(
    test_int(object@min_cohorts, lower = 1),
    "min_cohorts needs to be a positive integer scalar"
  )
  v$result()
}

#' @describeIn v_opening validates that the [`OpeningMinResponses`] object
#'   contains valid `min_responses` and `include_lower_doses` slots.
v_opening_min_responses <- function(object) {
  v <- Validate()
  v$check(
    test_count(object@min_responses, positive = TRUE),
    "min_responses needs to be a positive integer scalar"
  )
  v$check(
    test_flag(object@include_lower_doses),
    "include_lower_doses needs to be a logical flag"
  )
  v$result()
}

# Recruitment ----

#' Internal Helper Functions for Validation of [`Recruitment`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Recruitment`] or inherited classes and therefore not exported.
#'
#' @name v_recruitment
#' @param object (`Recruitment`)
#'   object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_recruitment validates that the [`RecruitmentRatio`] object
#'   contains valid `ratio` slot.
v_recruitment_ratio <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@ratio, lower = 0),
    "ratio needs to be a non-negative numeric scalar"
  )
  v$result()
}

# Backfill ----

#' Internal Helper Functions for Validation of [`Backfill`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Backfill`] object and therefore not exported.
#'
#' @name v_backfill
#' @param object (`Backfill`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_backfill validates that the [`Backfill`] object
#'   contains valid slots.
v_backfill <- function(object) {
  v <- Validate()
  v$check(
    test_count(object@max_size, positive = TRUE),
    "max_size needs to be a positive integer scalar"
  )
  v$check(
    test_choice(object@priority, c("highest", "lowest", "random")),
    "priority needs to be one of 'highest', 'lowest', or 'random'"
  )
  v$result()
}
