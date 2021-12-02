# h_all_equivalent ----

test_that("h_all_equivalent returns TRUE for equivalent objects", {
  target <- structure(c(1, 2, 3.1), names = letters[1:3], some_attr = "some_attr")
  current <- structure(c(1, 2, 3.1), names = letters[4:6], some_attr = "some_attr1")

  result <- h_all_equivalent(target, current)
  expect_true(result)
})

test_that("h_all_equivalent returns TRUE for equivalent objects up to some tollerance", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.3)
  # Mean relative difference: 0.2 < tolerance = 0.3
  expect_true(result)
})

test_that("h_all_equivalent returns FALSE for non-equivalent objects", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.1)
  # Mean relative difference: 0.2 > tolerance = 0.1
  expect_false(result)
})

# h_plot_data_df ----

test_that("h_plot_data_df returns valid object for sample Data object with placebo and blinding", {
  data <- h_get_data()
  result <- h_plot_data_df(data, blind = TRUE)
  expected <- data.frame(
    patient = 1:12,
    ID = paste(" ", data@ID),
    cohort = data@cohort,
    dose = rep(c(25, 50, 100), each = 4),
    toxicity = as.factor(c(rep(0, 8), 1, rep(0, 3)))
  )

  expect_identical(result, expected)
})

test_that("h_plot_data_df valid object for sample Data object with placebo and no blinding", {
  data <- h_get_data()
  result <- h_plot_data_df(data)
  expected <- data.frame(
    patient = 1:12,
    ID = paste(" ", data@ID),
    cohort = data@cohort,
    dose = ifelse(data@x == data@doseGrid[1], 0, data@x),
    toxicity = as.factor(data@y)
  )

  expect_identical(result, expected)
})
