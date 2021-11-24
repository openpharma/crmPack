# GeneralData-class ----

test_that(".GeneralData works as expected", {
  result <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  expect_valid(result, "GeneralData")
})

# Data-class ----

test_that(".Data works as expected", {
  result <- expect_silent(.Data())
  expect_valid(result, "Data")
})

# Data-constructor ----

test_that("Data object can be created with user constructor Data", {
  result <- expect_silent(Data())
  expect_valid(result, "Data")
})

test_that("Data object can be created with custom values with Data constructor", {
  plcb <- 0.01
  result <- expect_silent(Data(
    x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
    y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
    ID = 1:12,
    cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
    doseGrid = c(plcb, seq(25, 300, 25)),
    placebo = TRUE,
  ))
  expect_valid(result, "Data")
})

test_that("Data constructor handles default IDs as expected", {
  plcb <- 0.05
  expect_warning(
    result <-
      Data(
        x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
        y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
        cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
        doseGrid = c(plcb, seq(25, 300, 25)),
        placebo = TRUE,
      ),
    regexp = "Used default patient IDs!"
  )
  expect_valid(result, "Data")
})

test_that("Data constructor works as expected without cohort and no placebo", {
  plcb <- 0.001
  expect_warning(
    result <- 
      Data(
        x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
        y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
        ID = 1:12,
        doseGrid = c(plcb, seq(25, 300, 25)),
        placebo = FALSE,
      ),
    regexp = "Used best guess cohort indices!"
  )
  expect_valid(result, "Data")
})
