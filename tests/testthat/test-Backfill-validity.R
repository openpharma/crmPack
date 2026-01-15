# Opening ----

## v_opening_min_dose ----

test_that("v_opening_min_dose passes for valid object", {
  object <- OpeningMinDose(min_dose = 0)
  expect_true(v_opening_min_dose(object))

  object <- OpeningMinDose(min_dose = 50)
  expect_true(v_opening_min_dose(object))
})

test_that("v_opening_min_dose returns message for non-valid min_dose", {
  err_msg <- "min_dose needs to be a non-negative numeric scalar"
  object <- OpeningMinDose(min_dose = 10)

  # Changing `min_dose` so that it is negative.
  object@min_dose <- -5
  expect_equal(v_opening_min_dose(object), err_msg)

  # Changing `min_dose` so that it is NA.
  object@min_dose <- NA_real_
  expect_equal(v_opening_min_dose(object), err_msg)

  # Changing `min_dose` so that it is not a scalar.
  object@min_dose <- c(1, 5)
  expect_equal(v_opening_min_dose(object), err_msg)
})
