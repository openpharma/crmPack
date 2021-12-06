# as.list ----

test_that("Coercion creates the expected list", {
  object <- Data()
  result <- as.list(object)

  expect_class(result, "list")
  expect_identical(slotNames(object), names(result))
})

# plot-Data ----

test_that("Plot works as expected for Data object with placebo and no blinding", {
  data <- h_get_data()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of Data with placebo and no blinding", result)
})

test_that("Plot works as expected for Data object with placebo and with blinding", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger("Plot of Data with placebo and with blinding", result)
})

# plot-DataDual ----

test_that("Plot works as expected for DataDual object with placebo and no blinding", {
  data <- h_get_data_dual()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of DataDual with placebo and no blinding", result)
})

test_that("Plot works as expected for DataDual object with placebo and with blinding", {
  data <- h_get_data_dual()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger("Plot of DataDual with placebo and with blinding", result)
})
