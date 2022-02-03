# maxDose-IncrementsNumDoseLevels ----

increments <- IncrementsRelativePerCohortDLT(
  DLTintervals = c(0, 1, 2),
  increments = c(2.00, 0.50, 0.30)
)

# Starting point for data below (no DLTs). Last dose is 50.
data_template <- Data(
  x = c(10, 10, 16, 20, 30, 40, 50, 50, 50),
  y = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  cohort = c(0, 0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = seq(from = 10, to = 200, by = 1)
)

test_that("IncrementsRelativePerCohortDLT works correctly without DLTs", {
  data <- data_template
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 150)  # 50 * (1 + 2) = 150.
})

test_that("IncrementsRelativePerCohortDLT works correctly without DLTs in the last cohort", {
  data <- data_template
  data@y <- as.integer(c(1, 0, 0, 0, 0, 0, 0, 0, 0))  # Some DLTs, but not in the last cohort.
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 75)  # 50 * (1 + 0.5) = 75.
})


test_that("IncrementsRelativePerCohortDLT works correctly when 2 DLTs not the last cohort", {
  data <- data_template
  data@y <- as.integer(c(1, 1, 0, 0, 0, 0, 0, 0, 0))  # Some DLTs, but not in the last cohort.
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 65)  # 50 * (1 + 0.3) = 65.
})

test_that("IncrementsRelativePerCohortDLT works correctly when 1 DLT in last cohort", {
  data <- data_template
  data@y <- as.integer(c(1, 0, 0, 0, 1, 0, 1, 0, 0))  # Overall more than 1 DLT, but only 1 in last cohort.
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 75)  # 50 * (1 + 0.5) = 75.
})

test_that("IncrementsRelativePerCohortDLT works correctly when 2 DLTs in last cohort", {
  data <- data_template
  data@y <- as.integer(c(1, 0, 0, 0, 1, 0, 1, 0, 1))
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 65)  # 50 * (1 + 0.3) = 65.
})

test_that("IncrementsRelativePerCohortDLT works correctly when 3 DLTs in last cohort", {
  data <- data_template
  data@y <- as.integer(c(1, 0, 0, 0, 1, 0, 1, 1, 1))
  result <- maxDose(
    increments,
    data = data
  )
  expect_equal(result, 65)  # 50 * (1 + 0.3) = 65.
})