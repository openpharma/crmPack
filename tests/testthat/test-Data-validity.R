# validate_subjects ----

test_that("validate_subjects returns TRUE for valid object", {
  object <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  expect_true(validate_subjects(object))
})

test_that("validate_subjects returns error message for non-valid object", {
  expect_error(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 4))
})
