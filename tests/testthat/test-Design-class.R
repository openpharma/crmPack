# RuleDesign ----

test_that(".RuleDesign works as expected", {
  result <- expect_silent(.RuleDesign())
  expect_valid(result, "RuleDesign")
})

test_that("RuleDesign object can be created with user constructor", {
  next_best <- NextBestThreePlusThree()
  cohort_size <- CohortSizeConst(size = 5L)
  data <- Data(doseGrid = 2:40)

  result <- expect_silent(
    RuleDesign(
      nextBest = next_best,
      cohortSize = cohort_size,
      data = data,
      startingDose = 5
    )
  )
  expect_valid(result, "RuleDesign")
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohortSize, cohort_size)
  expect_identical(result@data, data)
  expect_identical(result@startingDose, 5)
})
