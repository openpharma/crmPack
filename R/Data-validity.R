#' Internal Helper Functions for Validation of [`GeneralData`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`GeneralData`] or inherited classes and therefore not exported.
#'
#' @name v_data_objects
#' @param object (`GeneralData`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_data_objects validates that the [`GeneralData`]
#'   object contains unique `ID`, non-negative `cohort` indices and
#'   `ID` and `cohort` vectors are of the same length `nObs`.
v_general_data <- function(object) {
  v <- Validate()
  # In if clause so that below test_* won't fail.
  if (!test_int(object@nObs)) {
    return("nObs must be of type integer of length 1")
  }
  v$check(
    test_integer(
      object@ID,
      len = object@nObs,
      any.missing = FALSE,
      unique = TRUE,
      null.ok = TRUE
    ),
    "ID must be of type integer and length nObs and unique"
  )
  v$check(
    test_integer(
      object@cohort,
      lower = 0L,
      len = object@nObs,
      any.missing = FALSE,
      sorted = TRUE
    ),
    "cohort must be of type integer and length nObs and contain non-negative, sorted values"
  )
  v$result()
}

#' @describeIn v_data_objects helper function which verifies whether
#'   the `dose` values are unique in each and every different `cohort`.
#' @param dose (`numeric`)\cr dose values.
#' @param cohort (`integer`)\cr cohort indices parallel to `doses`.
#' @return `TRUE` if `dose` is unique per `cohort`, otherwise `FALSE`.
h_doses_unique_per_cohort <- function(dose, cohort) {
  assert_numeric(dose)
  assert_integer(cohort)

  num_doses_per_cohort <- tapply(
    X = dose,
    INDEX = cohort,
    FUN = function(d) length(unique(d))
  )
  all(num_doses_per_cohort == 1L)
}

#' @describeIn v_data_objects helper function which verifies whether the
#'   two-drug dose combination is unique in each cohort.
#' @param x (`matrix`)
#' @param cohort (`integer`)
#' @return `TRUE` if `x` is unique per `cohort`, otherwise `FALSE`.
h_combo_doses_unique_per_cohort <- function(x, cohort) {
  assert_matrix(x)
  assert_integer(cohort)

  combos_per_cohort <- tapply(
    X = seq_len(nrow(x)),
    INDEX = cohort,
    FUN = function(index) nrow(unique(x[index, , drop = FALSE]))
  )
  all(combos_per_cohort == 1L)
}

#' Helper Function performing validation Common to Data and DataOrdinal
#'
#' @rdname h_validate_common_data_slots
#' @param object (`Data` or `DataOrdinal`)\cr the object to be validated
#' @returns a `Validate` object containing the result of the validation
h_validate_common_data_slots <- function(object) {
  v <- Validate()
  v$check(
    test_double(object@x, len = object@nObs, any.missing = FALSE),
    "Doses vector x must be of type double and length nObs"
  )
  v$check(
    test_double(
      object@doseGrid,
      len = object@nGrid,
      any.missing = FALSE,
      unique = TRUE,
      sorted = TRUE
    ),
    "doseGrid must be of type double and length nGrid and contain unique, sorted values"
  )
  v$check(
    test_int(object@nGrid),
    "Number of dose grid values nGrid must be scalar integer"
  )
  v$check(
    test_integer(object@xLevel, len = object@nObs, any.missing = FALSE),
    "Levels xLevel for the doses the patients have been given must be of type integer and length nObs"
  )
  v$check(
    test_flag(object@placebo),
    "The placebo flag must be scalar logical"
  )
  v$check(
    test_subset(object@x, object@doseGrid),
    "Dose values in x must be from doseGrid"
  )
  v$check(
    h_all_equivalent(object@x, object@doseGrid[object@xLevel]),
    "x must be equivalent to doseGrid[xLevel] (up to numerical tolerance)"
  )
  if (!object@placebo) {
    v$check(
      h_doses_unique_per_cohort(dose = object@x, cohort = object@cohort),
      "There must be only one dose level per cohort"
    )
  }
  v
}

#' @describeIn v_data_objects validates that the [`Data`] object contains
#'   valid elements with respect to their types, dependency and length.
v_data <- function(object) {
  v <- h_validate_common_data_slots(object)
  v$check(
    test_integer(
      object@y,
      lower = 0,
      upper = 1,
      len = object@nObs,
      any.missing = FALSE
    ),
    "DLT vector y must be nObs long and contain 0 or 1 integers only"
  )
  v$check(
    test_logical(object@backfilled, len = object@nObs, any.missing = FALSE),
    "backfilled must be of type logical and length nObs and not contain missings"
  )
  v$check(
    test_integer(
      object@response,
      len = object@nObs,
      lower = 0,
      upper = 1,
      any.missing = TRUE
    ),
    "response must be of type integer, take values 0 or 1 or NA, and have length nObs"
  )

  v$result()
}

#' @describeIn v_data_objects validates that the [`DataDual`] object
#' contains valid biomarker vector with respect to its type and the length.
v_data_dual <- function(object) {
  v <- Validate()
  v$check(
    test_double(object@w, len = object@nObs, any.missing = FALSE),
    "Biomarker vector w must be of type double and length nObs"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataParts`] object
#' contains valid elements with respect to their types, dependency and length.
v_data_parts <- function(object) {
  v <- Validate()
  v$check(
    test_integer(
      object@part,
      lower = 1,
      upper = 2,
      len = object@nObs,
      any.missing = FALSE
    ),
    "vector part must be nObs long and contain 1 or 2 integers only"
  )
  v$check(
    test_int(object@nextPart, lower = 1, upper = 2),
    "nextPart must be integer scalar 1 or 2"
  )
  v$check(
    test_numeric(
      object@part1Ladder,
      any.missing = FALSE,
      sorted = TRUE,
      unique = TRUE
    ),
    "part1Ladder must be of type double and contain unique, sorted values"
  )
  v$check(
    test_subset(object@part1Ladder, object@doseGrid),
    "part1Ladder must have all entries from doseGrid"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataMixture`] object
#' contains valid elements with respect to their types, dependency and length.
v_data_mixture <- function(object) {
  v <- Validate()

  # In if clause so that below test_* won't fail.
  if (!test_int(object@nObsshare)) {
    return("nObsshare must be of type integer of length 1")
  }
  v$check(
    test_numeric(object@xshare, len = object@nObsshare, any.missing = FALSE),
    "Dose vector xshare must be of type double and length nObsshare"
  )
  v$check(
    test_integer(
      object@yshare,
      lower = 0,
      upper = 1,
      len = object@nObsshare,
      any.missing = FALSE
    ),
    "DLT vector yshare must be nObsshare long and contain 0 or 1 integers only"
  )
  v$check(
    test_subset(object@xshare, object@doseGrid),
    "Dose values in xshare must be from doseGrid"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataDA`] object
#' contains valid elements with respect to their types, dependency and length.
v_data_da <- function(object) {
  v <- Validate()
  # In if clause so that below test_* won't fail.
  if (!(test_number(object@Tmax) && object@Tmax > 0)) {
    return(
      "DLT window Tmax must be of type double of length 1 and greater than 0"
    )
  }
  v$check(
    test_numeric(
      object@u,
      upper = object@Tmax,
      len = object@nObs,
      any.missing = FALSE
    ) &&
      all(object@u >= 0),
    "u must be of type double, nObs length, non-negative, not missing and not greater than Tmax"
  )
  v$check(
    test_numeric(
      object@t0,
      lower = 0,
      len = object@nObs,
      any.missing = FALSE,
      sorted = TRUE
    ),
    "t0 must be of type double, nObs length, sorted non-negative"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataOrdinal`] object
#' contains valid elements with respect to their types, dependency and length.
v_data_ordinal <- function(object) {
  v <- h_validate_common_data_slots(object)
  v$check(
    test_integer(
      object@y,
      lower = 0,
      upper = length(object@yCategories) - 1,
      len = object@nObs,
      any.missing = FALSE
    ),
    "DLT vector y must be nObs long and contain integers between 0 and k-1 only, where k is the length of the vector in the yCategories slot" # nolint
  )
  v$check(
    length(unique(names(object@yCategories))) ==
      length(names(object@yCategories)),
    "yCategory labels must be unique"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataGrouped`] object
#'   contains valid group information.
v_data_grouped <- function(object) {
  v <- Validate()
  v$check(
    test_factor(
      object@group,
      levels = c("mono", "combo"),
      len = object@nObs,
      any.missing = FALSE
    ),
    "group must be factor with levels mono and combo of length nObs without missings"
  )
  v$result()
}

#' @describeIn v_data_objects validates that the [`DataCombo`] object
#'   contains valid two-drug combination data.
v_data_combo <- function(object) {
  v <- Validate()
  x_has_valid_shape <- is.matrix(object@x) &&
    is.double(object@x) &&
    identical(ncol(object@x), 2L) &&
    identical(nrow(object@x), object@nObs) &&
    !anyNA(object@x)
  xlevel_has_valid_shape <- is.matrix(object@xLevel) &&
    is.integer(object@xLevel) &&
    identical(ncol(object@xLevel), 2L) &&
    identical(nrow(object@xLevel), object@nObs) &&
    !anyNA(object@xLevel)

  v$check(
    x_has_valid_shape,
    "x must be a numeric matrix with 2 columns and nObs rows and not contain missings"
  )
  v$check(
    test_integer(
      object@y,
      lower = 0,
      upper = 1,
      len = object@nObs,
      any.missing = FALSE
    ),
    "DLT vector y must be nObs long and contain 0 or 1 integers only"
  )
  v$check(
    test_list(object@doseGrid, len = 2L, any.missing = FALSE),
    "doseGrid must be a list of length 2"
  )
  v$check(
    test_integer(object@nGrid, len = 2L, lower = 0L, any.missing = FALSE),
    "nGrid must be an integer vector of length 2 with non-negative entries"
  )
  v$check(
    xlevel_has_valid_shape,
    "xLevel must be an integer matrix with 2 columns and nObs rows and not contain missings"
  )
  v$check(
    test_character(
      object@drugNames,
      len = 2L,
      unique = TRUE,
      any.missing = FALSE
    ),
    "drugNames must be a character vector of length 2 with unique entries"
  )
  v$check(
    identical(names(object@doseGrid), object@drugNames),
    "doseGrid must be a named list with names equal to drugNames"
  )
  v$check(
    !x_has_valid_shape || identical(colnames(object@x), object@drugNames),
    "x must have column names equal to drugNames"
  )
  v$check(
    !xlevel_has_valid_shape ||
      identical(colnames(object@xLevel), object@drugNames),
    "xLevel must have column names equal to drugNames"
  )
  v$check(
    all(vapply(object@doseGrid, is.double, logical(1L))),
    "doseGrid entries must be numeric vectors"
  )
  v$check(
    all(vapply(object@doseGrid, function(grid) !anyNA(grid), logical(1L))),
    "doseGrid entries must not contain missings"
  )
  v$check(
    all(vapply(
      object@doseGrid,
      function(grid) !is.unsorted(grid, strictly = TRUE),
      logical(1L)
    )),
    "doseGrid entries must be sorted and unique"
  )
  v$check(
    identical(
      as.integer(vapply(object@doseGrid, length, integer(1L))),
      object@nGrid
    ),
    "lengths of doseGrid elements must be nGrid"
  )
  v$check(
    !x_has_valid_shape ||
      all(vapply(
        seq_along(object@drugNames),
        function(index) {
          test_subset(object@x[, index], object@doseGrid[[index]])
        },
        logical(1L)
      )),
    "dose values in x must be from the corresponding entry in doseGrid"
  )
  v$check(
    !(x_has_valid_shape && xlevel_has_valid_shape) ||
      all(vapply(
        seq_along(object@drugNames),
        function(index) {
          h_all_equivalent(
            object@x[, index],
            object@doseGrid[[index]][object@xLevel[, index]]
          )
        },
        logical(1L)
      )),
    "x must be equivalent to the corresponding doseGrid entries indexed by xLevel"
  )
  v$check(
    !x_has_valid_shape ||
      h_combo_doses_unique_per_cohort(x = object@x, cohort = object@cohort),
    "There must be only one dose combination per cohort"
  )
  v$check(
    test_logical(object@backfilled, len = object@nObs, any.missing = FALSE),
    "backfilled must be of type logical and length nObs and not contain missings"
  )
  v$check(
    test_integer(
      object@response,
      len = object@nObs,
      lower = 0,
      upper = 1,
      any.missing = TRUE
    ),
    "response must be of type integer, take values 0 or 1 or NA, and have length nObs"
  )

  v$result()
}
