# validate_subjects ----

test_that("validate_subjects returns TRUE for valid object", {
  object <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  expect_true(validate_subjects(object))
})

test_that("validate_subjects returns error message for non-valid object", {
  expect_error(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 4), regexp = ".+")
})

# validate_data ----

test_that("validate_data returns TRUE for valid object", {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100)
  dose_grid <- c(plcb, seq(25, 300, 25))
  object <- expect_silent(
    .Data(
      x = x,
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      doseGrid = dose_grid,
      nGrid = length(dose_grid),
      xLevel = matchTolerance(x = x, table = dose_grid),
      placebo = TRUE,
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      nObs = 12L
    )
  )
})

test_that("validate_data returns error message for non-valid object
          (only plcbacebo in cohort 3)", {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, plcb, plcb, plcb)
  dose_grid <- c(plcb, seq(25, 300, 25))
  expect_error(
    .Data(
      x = x,
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      doseGrid = dose_grid,
      nGrid = length(dose_grid),
      xLevel = matchTolerance(x = x, table = dose_grid),
      placebo = TRUE,
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      nObs = 12L
    ),
    regexp = ".+"
  )
})

test_that("validate_data returns error message for non-valid object
          (multiplcbe doses in cohort 1)", {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 300, plcb, 50, 50, 50, plcb, 100, 100, 100)
  dose_grid <- c(plcb, seq(25, 300, 25))
  expect_error(
    .Data(
      x = x,
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      doseGrid = dose_grid,
      nGrid = length(dose_grid),
      xLevel = matchTolerance(x = x, table = dose_grid),
      placebo = TRUE,
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      nObs = 12L
    ),
    regexp = ".+"
  )
})

test_that("validate_data returns error message for non-valid object
          (wrong first xLevel)", {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100)
  dose_grid <- c(plcb, seq(25, 300, 25))
  expect_error(
    .Data(
      x = x,
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      doseGrid = dose_grid,
      nGrid = length(dose_grid),
      xLevel = c(2L, 2L, 2L, 2L, 1L, 3L, 3L, 3L, 1L, 5L, 5L, 5L),
      placebo = TRUE,
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      nObs = 12L
    ),
    regexp = ".+"
  )
})
