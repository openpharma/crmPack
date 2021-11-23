# GeneralData ----

test_that(".GeneralData executes without error for valid object", {
  expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
})

test_that(".GeneralData throws error for non-valid object", {
  expect_error(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 4), regexp = ".*")
})

# Data ----

test_that("Data can be created with default constructor Data.", {
  res <- expect_silent(Data())
  expect_s4_class(res, "Data")
  expect_true(validObject(res))
})

test_that("Data can be created for valid object", {
  plcb <- 0.001
  expect_silent(
    Data(
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
      ID = 1:12,
      cohort = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = TRUE
    )
  )
})
