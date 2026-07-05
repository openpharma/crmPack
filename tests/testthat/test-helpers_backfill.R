# h_previous_cohort ----

test_that("h_previous_cohort works as expected for Data", {
  data <- Data(
    x = c(1, 1, 3),
    y = c(0, 1, 0),
    cohort = c(1, 1, 2),
    doseGrid = 1:10,
    ID = 1:3
  )
  expect_equal(
    h_previous_cohort(data),
    list(cohort = 1L, dose = 1)
  )
})

test_that("h_previous_cohort works as expected for empty Data", {
  data <- Data(
    x = numeric(0),
    y = numeric(0),
    cohort = integer(0),
    doseGrid = 1:10,
    ID = integer(0)
  )
  expect_equal(
    h_previous_cohort(data),
    list(cohort = NA_integer_, dose = NA_real_)
  )
})

test_that("h_previous_cohort works as expected when only one cohort is present", {
  data <- Data(
    x = c(1, 1, 1),
    y = c(0, 1, 0),
    cohort = c(1, 1, 1),
    doseGrid = 1:10,
    ID = 1:3
  )
  expect_equal(
    h_previous_cohort(data),
    list(cohort = NA_integer_, dose = NA_real_)
  )
})

test_that("h_previous_cohort works as expected for DataCombo", {
  data <- DataCombo(
    x = matrix(
      c(1, 1, 3, 2, 2, 4),
      nrow = 3,
      ncol = 2,
      byrow = TRUE
    ),
    y = c(0, 1, 0),
    cohort = c(1, 2, 3),
    doseGrid = list(drug1 = 1:10, drug2 = 1:10),
    ID = 1:3
  )
  expect_equal(
    h_previous_cohort(data),
    list(cohort = 2L, dose = c(drug1 = 3, drug2 = 2))
  )
})

test_that("h_get_dose_for_cohort works as expected for Data", {
  data <- Data(
    x = c(1, 1, 3),
    y = c(0, 1, 0),
    cohort = c(1, 1, 2),
    doseGrid = 1:10,
    ID = 1:3
  )
  expect_equal(h_get_dose_for_cohort(data, cohort = 1), 1)
  expect_equal(h_get_dose_for_cohort(data, cohort = 2), 3)
})

test_that("h_get_dose_for_cohort works as expected for DataCombo", {
  data <- DataCombo(
    x = matrix(
      c(1, 1, 3, 2, 2, 4),
      nrow = 3,
      ncol = 2,
      byrow = TRUE
    ),
    y = c(0, 1, 0),
    cohort = c(1, 2, 3),
    doseGrid = list(drug1 = 1:10, drug2 = 1:10),
    ID = 1:3
  )
  expect_equal(h_get_dose_for_cohort(data, cohort = 1), c(drug1 = 1, drug2 = 1))
  expect_equal(h_get_dose_for_cohort(data, cohort = 2), c(drug1 = 3, drug2 = 2))
})
