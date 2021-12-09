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

test_that("Plot works for Data object with placebo, blinding and no legend", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE, legend = FALSE)

  vdiffr::expect_doppelganger(
    "Plot of Data with placebo, blinding and no legend", result
  )
})

# plot-DataDual ----

test_that("Plot works as expected for DataDual object with placebo", {
  data <- h_get_data_dual()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of DataDual with placebo", result)
})

test_that("Plot works for DataDual object with placebo and blinding", {
  data <- h_get_data_dual()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger(
    "Plot of DataDual with placebo and blinding", result
  )
})

# plot-DataDA ----

test_that("Plot works as expected for DataDA object with placebo", {
  data <- h_get_data_augmented()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of DataDA with placebo", result)
})

test_that("Plot works for DataDA object with placebo and blinding", {
  data <- h_get_data_augmented()
  result <- plot(data, blind = TRUE)

  vdiffr::expect_doppelganger(
    "Plot of DataDA with placebo and blinding", result
  )
})

# update-Data ----

test_that("Update of Data works as expected", {
  object <- h_get_data()
  result <- update(object, x = 25, y = c(0L, 1L, 1L))

  object@x <- c(object@x, 25, 25, 25)
  object@y <- c(object@y, c(0L, 1L, 1L))
  object@nObs <- object@nObs + 3L
  object@ID <- c(object@ID, 13L, 14L, 15L)
  object@xLevel <- c(object@xLevel, 2L, 2L, 2L)
  object@cohort <- c(object@cohort, 4L, 4L, 4L)

  expect_valid(result, "Data")
  expect_equal(result, object)
})

test_that("Update of Data works when doses are added to the old cohort", {
  object <- h_get_data()
  result <- update(object, x = 100, y = c(0L, 1L, 1L), new_cohort = FALSE)

  object@x <- c(object@x, 100, 100, 100)
  object@y <- c(object@y, 0L, 1L, 1L)
  object@nObs <- object@nObs + 3L
  object@ID <- c(object@ID, 13L, 14L, 15L)
  object@xLevel <- c(object@xLevel, 5L, 5L, 5L)
  object@cohort <- c(object@cohort, 3L, 3L, 3L)

  expect_valid(result, "Data")
  expect_equal(result, object)
})

test_that("Update of Data throws the error for a dose x out of the grid", {
  object <- h_get_data()
  expect_error(
    update(object, x = 12345, y = c(0L, 1L, 1L), new_cohort = FALSE),
    ".*Dose values in x must be from doseGrid.*"
  )
})

test_that("Update of Data, no error for non-valid update and validation off", {
  object <- h_get_data()
  expect_silent(
    update(
      object,
      x = 12345, y = c(0L, 1L, 1L), new_cohort = FALSE, check = FALSE
    )
  )
})

# update-DataParts ----

test_that("Update of DataParts works as expected", {
  object <- h_get_data_parts() # nextPart equals 1L here.
  result <- update(object, x = 200, y = c(0L, 1L))

  object@x <- c(object@x, 200, 200)
  object@y <- c(object@y, 0L, 1L)
  object@nObs <- object@nObs + 2L
  object@ID <- c(object@ID, 13L, 14L)
  object@xLevel <- c(object@xLevel, 9L, 9L)
  object@cohort <- c(object@cohort, 4L, 4L)
  object@part <- c(object@part, 1L, 1L)
  object@nextPart <- 2L

  expect_valid(result, "DataParts")
  expect_equal(result, object)
})

test_that("Update of DataParts works as expected", {
  object <- h_get_data_parts()
  # The above object has nextPart slot equals 1L and y not all equal 0.
  result <- update(object, x = 200, y = c(0L, 1L))

  object@x <- c(object@x, 200, 200)
  object@y <- c(object@y, 0L, 1L)
  object@nObs <- object@nObs + 2L
  object@ID <- c(object@ID, 13L, 14L)
  object@xLevel <- c(object@xLevel, 9L, 9L)
  object@cohort <- c(object@cohort, 4L, 4L)
  object@part <- c(object@part, 1L, 1L)
  object@nextPart <- 2L

  expect_valid(result, "DataParts")
  expect_equal(result, object)
})

test_that("Update of DataParts works, no DLT and x eq max of part1Ladder", {
  object <- h_get_data_parts()
  object@nextPart <- 1L
  object@y <- rep(0L, 12)

  result <- update(object, x = 250, y = c(0L, 0L)) # max of part1Ladder eq. 250.

  object@x <- c(object@x, 250, 250)
  object@y <- c(object@y, 0L, 0L)
  object@nObs <- object@nObs + 2L
  object@ID <- c(object@ID, 13L, 14L)
  object@xLevel <- c(object@xLevel, 11L, 11L)
  object@cohort <- c(object@cohort, 4L, 4L)
  object@part <- c(object@part, 1L, 1L)
  object@nextPart <- 2L

  expect_valid(result, "DataParts")
  expect_equal(result, object)
})
