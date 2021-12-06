# as.list ----

test_that("Coercion creates the expected list", {
  object <- Data()
  result <- as.list(object)

  expect_class(result, "list")
  expect_identical(slotNames(object), names(result))
})

# plot-Data ----

test_that("Plot works as expected for Data object with placebo", {
  data <- h_get_data()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of Data with placebo", result)
})

test_that("Plot works as expected for Data object with placebo and blinding", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger("Plot of Data with placebo and blinding", result)
})

test_that("Plot works as expected for Data object with placebo, blinding and no legend", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE, legend = FALSE)

  vdiffr::expect_doppelganger("Plot of Data with placebo, blinding and no legend", result)
})

# plot-DataDual ----

test_that("Plot works as expected for DataDual object with placebo", {
  data <- h_get_data_dual()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of DataDual with placebo", result)
})

test_that("Plot works as expected for DataDual object with placebo and blinding", {
  data <- h_get_data_dual()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger("Plot of DataDual with placebo and blinding", result)
})

# plot-DataDA ----

test_that("Plot works as expected for DataDA object with placebo", {
  data <- h_get_data_augmented()
  result <- plot(data)
  
  vdiffr::expect_doppelganger("Plot of DataDA with placebo", result)
})

test_that("Plot works as expected for DataDA object with placebo and blinding", {
  data <- h_get_data_augmented()
  result <- plot(data, blind = TRUE)
  
  vdiffr::expect_doppelganger("Plot of DataDA with placebo and blinding", result)
})
