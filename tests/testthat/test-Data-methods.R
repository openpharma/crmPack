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
  data <- h_get_data_da()
  result <- plot(data)

  vdiffr::expect_doppelganger("Plot of DataDA with placebo", result)
})

test_that("Plot works for DataDA object with placebo and blinding", {
  data <- h_get_data_da()
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
  object@y <- c(object@y, 0L, 1L, 1L)
  object@nObs <- object@nObs + 3L
  object@ID <- c(object@ID, 13L, 14L, 15L)
  object@xLevel <- c(object@xLevel, 2L, 2L, 2L)
  object@cohort <- c(object@cohort, 4L, 4L, 4L)

  expect_valid(result, "Data")
  expect_identical(result, object)
})

test_that("Update of empty Data works as expected", {
  object <- Data(
    x = c(25, 25), y = c(0L, 1L), doseGrid = 25, ID = 1:2, cohort = c(1L, 1L)
  )
  result <- update(Data(doseGrid = 25), x = 25, y = c(0L, 1L))

  expect_valid(result, "Data")
  expect_identical(result, object)
})

test_that("Update of Data works for 'empty' update", {
  object <- h_get_data()
  result <- update(object, x = numeric(0), y = integer(0))
  expect_identical(result, object)
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
  expect_identical(result, object)
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
  expect_identical(result, object)
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
  expect_identical(result, object)
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
  expect_identical(result, object)
})


# update-DataDual ----

test_that("Update of DataDual works as expected", {
  object <- h_get_data_dual()
  result <- update(object, w = c(118, 124), x = 25, y = c(0L, 1L))

  object@w <- c(object@w, 118, 124)
  object@x <- c(object@x, 25, 25)
  object@y <- c(object@y, 0L, 1L)
  object@nObs <- object@nObs + 2L
  object@ID <- c(object@ID, 13L, 14L)
  object@xLevel <- c(object@xLevel, 2L, 2L)
  object@cohort <- c(object@cohort, 4L, 4L)

  expect_valid(result, "DataDual")
  expect_identical(result, object)
})

# update-DataDA ----

test_that("Update of DataDA works as expected", {
  object <- h_get_data_da()
  result <- update(
    object = object,
    y = c(object@y, 0),
    u = c(object@u, 20),
    t0 = c(object@t0, 135),
    x = 25,
    trialtime = 140
  )

  object@x <- c(object@x, 25)
  object@y <- rep(0L, 13)
  object@nObs <- object@nObs + 1L
  object@ID <- c(object@ID, 13L)
  object@xLevel <- c(object@xLevel, 2L)
  object@cohort <- c(object@cohort, 4L)
  object@t0 <- c(object@t0, 135)
  object@u <- c(42, 30, 15, 5, 20, 25, 30, 55, 25, 30, 20, 15, 5)

  expect_valid(result, "DataDA")
  expect_identical(result, object)
})

test_that("Update of DataDA works for empty update of empty object", {
  object <- DataDA()
  result <- update(
    object = object,
    y = integer(0),
    u = numeric(0),
    t0 = numeric(0),
    x = numeric(0),
    trialtime = numeric(0)
  )
  expect_valid(result, "DataDA")
  expect_identical(result, object)
})

test_that("Update of DataDA works when no update of non-empty object", {
  object <- h_get_data_da()
  result <- update(
    object = object,
    y = object@y,
    u = object@u,
    t0 = object@t0,
    x = numeric(0),
    trialtime = 500
  )

  expect_valid(result, "DataDA")
  expect_identical(result, object)
})

test_that("Update of DataDA throws the error for empty trialtime", {
  object <- h_get_data_da()
  expect_error(
    update(
      object = object,
      y = c(object@y, 0),
      u = c(object@u, 20),
      t0 = c(object@t0, 135),
      x = 25,
      trialtime = numeric(0)
    ),
    "Assertion on 'trialtime' failed: Must have length 1."
  )
})

# getEff-DataDual ----

test_that("getEff-DataDual works as expected", {
  data <- h_get_data_dual()
  result <- getEff(data)
  expected <- list(
    x_dlt = 100,
    w_dlt = 87,
    x_no_dlt = c(0.001, 25, 25, 25, 0.001, 50, 50, 50, 0.001, 100, 100),
    w_no_dlt = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 48)
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (no DLT)", {
  data <- DataDual(
    x = c(25, 50),
    y = c(0, 0),
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data)
  expected <- list(
    x_dlt = NULL,
    w_dlt = NULL,
    x_no_dlt = c(25, 50),
    w_no_dlt = c(0.31, 0.42)
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (DLT only)", {
  data <- DataDual(
    x = c(25, 50),
    y = c(1, 1),
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data)
  expected <- list(
    x_dlt = c(25, 50),
    w_dlt = c(0.31, 0.42),
    x_no_dlt = NULL,
    w_no_dlt = NULL
  )

  expect_identical(result, expected)
})
