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
    RuleDesign(next_best, cohort_size, data, 5)
  )
  expect_valid(result, "RuleDesign")
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohortSize, cohort_size)
  expect_identical(result@data, data)
  expect_identical(result@startingDose, 5)
})

test_that("RuleDesign user constructor arguments names are as expected", {
  expect_function(
    RuleDesign,
    args = c("nextBest", "cohortSize", "data", "startingDose"),
    ordered = TRUE
  )
})

# Design ----

test_that(".Design works as expected", {
  result <- expect_silent(.Design())
  expect_valid(result, "Design")
})

test_that("Design object can be created with user constructor", {
  empty_data <- Data(doseGrid = 2:50)
  model <- h_get_model_log_normal()
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()
  placebo_cohort_size <- CohortSizeConst(0L)
  next_best <- h_next_best_ncrm()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

  result <- expect_silent(
    Design(
      model,
      stopping,
      increments,
      nextBest = next_best,
      cohortSize = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "Design")
  expect_identical(result@model, model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@PLcohortSize, placebo_cohort_size)
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohortSize, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)

  result <- expect_silent(
    Design(
      model,
      stopping,
      increments,
      PLcohortSize = CohortSizeConst(2L),
      nextBest = next_best,
      cohortSize = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_identical(result@PLcohortSize, CohortSizeConst(2L))
})

test_that("Design user constructor arguments names are as expected", {
  expect_function(
    Design,
    args = c("model", "stopping", "increments", "PLcohortSize", "..."),
    ordered = TRUE
  )
})

# DualDesign ----

test_that(".DualDesign works as expected", {
  result <- expect_silent(.DualDesign())
  expect_valid(result, "DualDesign")
})

test_that("DualDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_dual_endpoint_rw()
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()
  next_best <- h_next_best_dual_endpoint()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

  result <- expect_silent(
    DualDesign(
      model,
      empty_data,
      stopping = stopping,
      increments = increments,
      nextBest = next_best,
      cohortSize = cohort_size,
      startingDose = 3
    )
  )
  expect_valid(result, "DualDesign")
  expect_identical(result@model, model)
  expect_identical(result@data, empty_data)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohortSize, cohort_size)
  expect_identical(result@startingDose, 3)
})

test_that("DualDesign user constructor arguments names are as expected", {
  expect_function(
    DualDesign,
    args = c("model", "data", "..."),
    ordered = TRUE
  )
})
