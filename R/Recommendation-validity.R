#' Internal Helper Functions for Validation of [`Recommendation`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`Recommendation`] or inherited classes and therefore not exported.
#'
#' @name v_recommendation
#' @param object (`Recommendation`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_recommendation validates that the [`Recommendation`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot.
v_recommendation <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@value,
      lower = 0,
    ),
    "value must be positive real or `NA`."
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the [`RecommendationProbs`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a tibble in the `probs` slot.
v_recommendationprobs <- function(object) {
  v <- Validate()

  cols <- names(object@details)
  for (c in cols[!(cols %in% c("Dose", "Dose1", "Dose2", "Eligible"))]) {
    v$check(
      test_probabilities(
        object@details %>% dplyr::pull(c),
        any.missing = TRUE
      ),
      "every column of details apart from dose must contain non-missing probabilities."
    )
  }
  #         Default                            DataCombo
  v$check(
    test_true("Dose" %in% names(object@details) | all(c("Dose1", "Dose2") %in% names(object@details))),
    "details must contain a column named `Dose` (or `Dose1` and `Dose2`)."
  )

  v$check(
    test_true(
      all(
        object@details %>%
          dplyr::select(
            -tidyselect::starts_with("Dose"),
            -tidyselect::starts_with("Eligible")
          ) %>%
          rowSums(na.rm = all(c("Dose1", "Dose2") %in% names(object@details))) <= 1
      )
    ),
    "probabilities in every row of details must sum to less than one."
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the [`RecommendationThreePlusThree`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a flag in the `stopHere` slot.
v_recommendationthreeplusthree <- function(object) {
  v <- Validate()


  v$check(
    test_flag(object@stopHere),
    "stopHere must be a flag."
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the [`RecommendationDetails`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a tibble in the `details` slot.  Subclasses provide additional
#'   validation of the `detaiuls` slot
v_recommendationdetails <- function(object) {
  v <- Validate()
  v$check(
    test_tibble(object@details),
    "details must be a tibble."
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the tibble in the `details` slot
#' has columns named `Time`, `TargetDose`, `LowerCI`, `UpperCi` and `CIRatio`,
#' the last three of which have `NULL`s in the first row and psitive values in
#' the second.  The tibble should contain only two rows.
v_recommendationtd <- function(object) {
  v <- Validate()

  v$check(
    test_set_equal(names(object@pdetails), c("Time", "TargetDose", "LowerCI", "UpperCI", "CIRatio")),
    "incorrect column names"
  )
  v$check(
    test_true(details %>% nrow() == 2),
    "details must be a tibble with two rows"
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the tibble in the `details` slot
#' has columns named `EstimateType`, `TargetDose`, `LowerCI`, `UpperCi` and `CIRatio`,
#' the last three of which have `NULL`s in the first row and psitive values in
#' the second.  The tibble should contain only two rows.
v_recommendationmaxgain <- function(object) {
  v <- Validate()

  v$check(
    test_set_equal(names(object@details), c("EstimateType", "TargetDose", "LowerCI", "UpperCI", "CIRatio")),
    "details must be a tibble with columns EstimateType, TargetDose, LowerCI, UpperCI and CIRatio"
  )
  v$check(
    test_true(object@details %>% nrow() == 3),
    "details must be a tibble with three rows"
  )
  v$check(
    test_numeric(object@value, lower = 0, len = 3, null.ok = FALSE),
    "value must be a vector of positive reals or NA and of length 3"
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the tibble in the `details` slot
#' has columns named `Time`, `TargetDose`, `LowerCI`, `UpperCI` and `CIRatio`,
#' the last three of which have `NULL`s in the first row and pOsitive values in
#' the second.  The tibble should contain only two rows.
v_recommendationtd <- function(object) {
  v <- Validate()

  v$check(
    test_set_equal(names(object@details), c("Time", "TargetDose", "LowerCI", "UpperCI", "CIRatio")),
    "details must be a tibble with columns Time, TargetDose, LowerCI, UpperCI and CIRatio"
  )
  v$check(
    test_true(object@details %>% nrow() == 2),
    "details must be a tibble with two rows"
  )
  v$check(
    test_numeric(object@value, lower = 0, len = 2, null.ok = FALSE),
    "value must be a vector of positive reals or NA and of length 2"
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendationallocation validates that the tibble in the `details` slot
#' has columns named `Time`, `TargetDose`, `LowerCI`, `UpperCI` and `CIRatio`,
#' the last three of which have `NULL`s in the first row and positive values in
#' the second.  The tibble should contain only two rows.
v_recommendationallocation <- function(object) {
  v <- Validate()

  v$check(
    test_set_equal(names(object@details), c("Dose", "Allocation")),
    "details must be a tibble with columns Dose and Allocation"
  )
  if ("Allocation" %in% names(object@details)) {
    v$check(
      test_probabilities(object@details %>% dplyr::pull(Allocation)),
      "The Allocation column in the details tibble must contain non-missing probabilities"
    )
    v$check(
      test_true(sum(object@details %>% dplyr::pull(Allocation)) == 1),
      "The probabilities in the Allocation column in the details tibble must sum to 1"
    )
  }
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the tibble in the `details` slot
#' has columns named `Dose`, `Mean`, `StdDev` and `PosteriorLoss`, and that row
#' sums of the remaining (probability) columns are equal to 1.
v_recommendationncrmloss <- function(object) {
  v <- Validate()

  v$check(
    test_subset(c("Dose", "Mean", "StdDev", "PosteriorLoss"), names(object@details)),
    "details must be a tibble with columns including Dose, Mean, StdDev and PosteriorLoss"
  )

  v$check(
    test_true(all(round(object@details %>% dplyr::select(-c("Dose", "Mean", "StdDev", "PosteriorLoss")) %>% rowSums(), 3) == 1)),
    "probabilities in every row of details must sum to one."
  )
  v$result()
}

#' Validation Helpers for Recommendation Objects
#'
#' @describeIn v_recommendation validates that the [`RecommendationProbsDataCombo`]
#'   object contains a positive real number (or `NA_numeric_`) in the `value`
#'   slot and a tibble in the `probs` slot.
v_recommendationprobsdatacombo <- function(object) {
  v <- Validate()

  # cols <- names(object@details)
  # for (c in cols[!(cols %in% c("Dose", "Dose1", "Dose2"))]) {
  #   v$check(
  #     test_probabilities(object@details %>% dplyr::pull(c)),
  #     "every column of details apart from dose must contain non-missing probabilities."
  #   )
  # }
  # #         Default                            DataCombo
  # v$check(
  #   test_true("Dose" %in% names(object@details) | all(c("Dose1", "Dose2") %in% names(object@details))),
  #   "details must contain a column named `Dose` (or `Dose1` and `Dose2`)."
  # )
  v$result()
}
