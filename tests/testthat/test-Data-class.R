# GeneralData-class ----

test_that("GeneralData default constructor .GeneralData
          executes without error for valid object", {
  expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
})

# Data-class ----

test_that("Data can be created with default constructor .Data", {
  res <- expect_silent(.Data())
  expect_s4_class(res, "Data")
  expect_true(validObject(res))
})

# Data-constructors ----

test_that("Data can be created with user constructor Data", {
  res <- expect_silent(Data())
  expect_s4_class(res, "Data")
  expect_true(validObject(res))
})

test_that("Data can be created for valid object with user constructor Data", {
  plcb <- 0.001
  expect_silent(
    Data(
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = TRUE,
    )
  )
})

test_that("Data can be created for valid object without ID
          with user constructor Data", {
  plcb <- 0.001
  expect_warning(
    Data(
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = TRUE,
    ),
    regexp = "Used default patient IDs!"
  )
})

test_that("Data can be created for valid object without cohort and no placebo
          with user constructor Data", {
  plcb <- 0.001
  expect_warning(
    Data(
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      ID = 1:12,
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = FALSE,
    ),
    regexp = "Used best guess cohort indices!"
  )
})

test_that("Data can be created for valid object without ID and cohort
          and no placebo with user constructor Data", {
  plcb <- 0.001
  expect_warning(
    expect_warning(
      Data(
        x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
        y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
        doseGrid = c(plcb, seq(25, 300, 25)),
        placebo = FALSE
      ),
      regexp = "Used default patient IDs!"
    ),
    regexp = "Used best guess cohort indices!"
  )
})
