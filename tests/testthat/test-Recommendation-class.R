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
      probs = tibble::tibble(Dose = 1, Target = 0.3, Overdose = 0.1)
    )
  )
  expect_valid(result, "RecommendationProbs")

  testthat::expect_error(
    RecommendationProbs(value = 1, probs = list()),
    "invalid object for slot \"probs\" in class \"RecommendationProbs\": got class \"list\""
  )
  testthat::expect_error(
    RecommendationProbs(value = 1, probs = tibble::tibble(Target = -0.3, Overdose = 0.1)),
    "must contain a column named `Dose`"
  )
  testthat::expect_error(
    RecommendationProbs(value = 1, probs = tibble::tibble(Dose = 1, Target = 4,  Overdose = 0.1)),
    "every column of probs apart from dose must contain non-missing probabilities"
  )

  testthat::expect_error(
    RecommendationProbs(value = 1, probs = tibble::tibble(Dose = 1, Target = 0.5,  Overdose = 0.9)),
    "probabilities in every row of probs must sum to less than one"
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
    "invalid object for slot \"details\" in class \"RecommendationTD\": got class \"list\", should be or extend class \"tbl_df\""
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
    "details must be a tibble with columns Time, TargetDose, LowerCi, UpperCi and CIRatio"
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
    RecommendationMaxGain(
      value = c(75, 75),
      details = tibble::tibble(
        Time  = c("During trial", "End of Trial"),
        BadCol = c(99.8, 81.5),
        LowerCI = c(NA, 24.3),
        UpperCI = c(NA, 273.0),
        CIRatio = c(NA, 11.2)
      )
    ),
    "details must be a tibble with columns Time, TargetDose, LowerCi, UpperCi and CIRatio"
  )
  testthat::expect_error(
    RecommendationMaxGain(
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
    RecommendationMaxGain(
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
    RecommendationMaxGain(
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

