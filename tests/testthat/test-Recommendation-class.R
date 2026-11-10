# Recommendation-class ----

test_that("Recommendation works as expected", {
  result <- expect_silent(Recommendation(value = 44))
  expect_valid(result, "Recommendation")

  testthat::expect_error(Recommendation(value = -1), "invalid class")
})

# RecommendationProbs-class ----

test_that("RecommendationProbs works as expected", {
  result <- expect_silent(
    RecommendationProbs(
      value = 44,
      details = tibble::tibble(Dose = 1, Target = 0.3, Overdose = 0.1)
    )
  )
  expect_valid(result, "RecommendationProbs")

  testthat::expect_error(
    RecommendationProbs(value = 1, details = list()),
    "invalid object for slot \"details\" in class \"RecommendationProbs\": got class \"list\""
  )
  testthat::expect_error(
    RecommendationProbs(value = 1, details = tibble::tibble(Target = -0.3, Overdose = 0.1)),
    "must contain a column named `Dose`"
  )
  testthat::expect_error(
    RecommendationProbs(value = 1, details = tibble::tibble(Dose = 1, Target = 4,  Overdose = 0.1)),
    "every column of details apart from dose must contain non-missing probabilities"
  )

  testthat::expect_error(
    RecommendationProbs(value = 1, details = tibble::tibble(Dose = 1, Target = 0.5,  Overdose = 0.9)),
    "probabilities in every row of details must sum to less than one"
  )
})

# RecommendationTD-class ----

test_that("RecommendationTD works as expected", {
  result <- expect_silent(.DefaultRecommendationTD())
  expect_valid(result, "RecommendationTD")

  testthat::expect_error(
    RecommendationTD(value = -1),
    "invalid class"
  )
  testthat::expect_error(
    RecommendationTD(value = 1, details = list()),
    paste0(
      "invalid object for slot \"details\" in class \"RecommendationTD\": ",
      "got class \"list\", should be or extend class \"tbl_df\""
    )
  )
  testthat::expect_error(
    RecommendationTD(
      value = c(75, 75),
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial", ""),
        TargetDose = c(99.8, 81.5, NA),
        LowerCI = c(NA, 24.3, NA),
        UpperCI = c(NA, 273.0, NA),
        CIRatio = c(NA, 11.2, NA)
      )
    ),
    "details must be a tibble with two rows"
  )
  testthat::expect_error(
    RecommendationTD(
      value = c(75, 75),
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial"),
        BadCol = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "details must be a tibble with columns Time, TargetDose, LowerCI, UpperCI and CIRatio"
  )
  testthat::expect_error(
    RecommendationTD(
      value = 75,
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial"),
        TargetDose = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "value must be a vector of positive reals or NA and of length 2"
  )
  testthat::expect_error(
    RecommendationTD(
      value = 1:3,
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial"),
        TargetDose = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "value must be a vector of positive reals or NA and of length 2"
  )
  testthat::expect_error(
    RecommendationTD(
      value = c(1, -1),
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial"),
        TargetDose = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "invalid class"
  )
})

# RecommendationMaxGain-class ----

test_that("RecommendationMaxGain works as expected", {
  result <- expect_silent(.DefaultRecommendationMaxGain())
  expect_valid(result, "RecommendationMaxGain")

  testthat::expect_error(
    RecommendationMaxGain(value = -1),
    "invalid class"
  )
  testthat::expect_error(
    RecommendationMaxGain(value = 1, details = list()),
    "invalid class"
  )
  testthat::expect_error(
    RecommendationMaxGain(
      value = c(75, 125, 75),
      details = tibble::tibble(
        EstimateType  = c("During trial", "End of Trial"),
        TargetDose = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "details must be a tibble with three rows"
  )
  testthat::expect_error(
    RecommendationMaxGain(
      value = c(75, 125, 75),
      details = tibble::tibble(
        EstimateType = c("During trial", "End of trial", "Max gain"),
        TargetDose = c(30.5, 19.3, 36.2),
        BadCol = c(NA, 19.3, 19.3),
        UpperCI = c(NA, 36.2, 219),
        CIRatio = c(NA, 1, 6.05)
      )
    ),
    "details must be a tibble with columns EstimateType, TargetDose, LowerCI, UpperCI and CIRatio"
  )
  testthat::expect_error(
    RecommendationMaxGain(
      value = 75,
      details = tibble::tibble(
        EstimateType = c("During trial", "End of trial", "Max gain"),
        TargetDose = c(30.5, 19.3, 36.2),
        LowerCI = c(NA, 19.3, 19.3),
        UpperCI = c(NA, 36.2, 219),
        CIRatio = c(NA, 1, 6.05)
      )
    ),
    "value must be a vector of positive reals or NA and of length 3"
  )
  testthat::expect_error(
    RecommendationMaxGain(
      value = 1:4,
      details = tibble::tibble(
        EstimateType = c("During trial", "End of trial", "Max gain"),
        TargetDose = c(30.5, 19.3, 36.2),
        LowerCI = c(NA, 19.3, 19.3),
        UpperCI = c(NA, 36.2, 219),
        CIRatio = c(NA, 1, 6.05)
      )
    ),
    "value must be a vector of positive reals or NA and of length 3"
  )
  testthat::expect_error(
    RecommendationMaxGain(
      value = c(1, -1),
      details = tibble::tibble(
        EstimateType  = c("During trial", "End of Trial"),
        TargetDose = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "invalid class"
  )
})

# RecommendationAllocation-class ----

test_that("RecommendationAllocation works as expected", {
  result <- expect_silent(.DefaultRecommendationAllocation())
  expect_valid(result, "RecommendationAllocation")

  testthat::expect_error(
    RecommendationAllocation(value = -1),
    "invalid class"
  )
  testthat::expect_error(
    RecommendationAllocation(value = 1, details = list()),
    "invalid class"
  )
  testthat::expect_error(
    RecommendationAllocation(
      value = 3,
      details = tibble::tibble(
        Dose = c(1.5, 2.5, 3.5, 4.5, 6.0, 7.0),
        BadCol = c(0.075, 0.373, 0.281, 0.081, 0.038, 0.152)
      )
    ),
    "details must be a tibble with columns Dose and Allocation"
  )

  testthat::expect_error(
    RecommendationAllocation(
      value = c(1, -1),
      details = tibble::tibble(
        details = tibble::tibble(
          Dose = c(1.5, 2.5, 3.5, 4.5, 6.0, 7.0),
          Allocation = c(0.075, 0.373, 0.281, 0.081, 0.038, 0.152)
        )
      )
    )
  )

  testthat::expect_error(
    RecommendationAllocation(
      value = c(1, -1),
      details = tibble::tibble(
        details = tibble::tibble(
          Dose = c(        1.5,   2.5,   3.5,   4.5,   6.0,   7.0),
          Allocation = c(0.075, 0.373, 0.281, 0.081, 0.038, 0.152)
        )
      )
    )
  )
})

