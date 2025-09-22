# plot-Data ----

test_that("Plot works as expected for Data object with placebo", {
  data <- h_get_data()
  result <- plot(data)

  expect_doppel("Plot of Data with placebo", result)
})

test_that("Plot works as expected for Data object with placebo and blinding", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE)

  expect_doppel("Plot of Data with placebo and blinding", result)
})

test_that("Plot works for Data object with placebo, blinding and no legend", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE, legend = FALSE)

  expect_doppel(
    "Plot of Data with placebo, blinding and no legend",
    result
  )
})

# plot-DataDual ----

test_that("Plot works as expected for DataDual object with placebo", {
  data <- h_get_data_dual()
  result <- plot(data)

  expect_doppel("Plot of DataDual with placebo", result)
})

test_that("Plot works for DataDual object with placebo and blinding", {
  data <- h_get_data_dual()
  result <- plot(data, blind = TRUE)

  expect_doppel(
    "Plot of DataDual with placebo and blinding",
    result
  )
})

# plot-DataDA ----

test_that("Plot works as expected for DataDA object with placebo", {
  data <- h_get_data_da()
  result <- plot(data)

  expect_doppel("Plot of DataDA with placebo", result)
})

test_that("Plot works for DataDA object with placebo and blinding", {
  data <- h_get_data_da()
  result <- plot(data, blind = TRUE)

  expect_doppel(
    "Plot of DataDA with placebo and blinding",
    result
  )
})

# plot-DataOrdinal ----

test_that("Plot works as expected for DataOrdinal object with placebo", {
  data <- h_get_data_ordinal()
  result <- plot(data)

  expect_doppel("plot-DataOrdinal-placebo", result)
})

test_that("Plot works as expected for DataOrdinal object with placebo and blinding", {
  data <- h_get_data_ordinal()
  result <- plot(data, blind = TRUE)

  expect_doppel("plot-DataOrdinal-placebo-blinding", result)
})

test_that("Plot works for DataOrdinal object with placebo, blinding and no legend", {
  data <- h_get_data()
  result <- plot(data, blind = TRUE, legend = FALSE)

  expect_doppel("plot-DataOrdinal-placebo-blinding-nolegend", result)
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
    x = c(25, 25),
    y = c(0L, 1L),
    doseGrid = 25,
    ID = 1:2,
    cohort = c(1L, 1L)
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
      x = 12345,
      y = c(0L, 1L, 1L),
      new_cohort = FALSE,
      check = FALSE
    )
  )
})

# update-DataOrdinal
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

test_that("Update of empty DataOrdinal works as expected", {
  object <- DataOrdinal(
    x = c(25, 25),
    y = c(0L, 1L),
    doseGrid = 25,
    ID = 1:2,
    cohort = c(1L, 1L)
  )
  result <- update(DataOrdinal(doseGrid = 25), x = 25, y = c(0L, 1L))

  expect_valid(result, "DataOrdinal")
  expect_identical(result, object)
})

test_that("Update of DataOrdinal works for 'empty' update", {
  object <- h_get_data_ordinal()
  result <- update(object, x = numeric(0), y = integer(0))
  expect_identical(result, object)
})

test_that("Update of DataOrdinal works when doses are added to the old cohort", {
  object <- h_get_data_ordinal()
  result <- update(object, x = 60, y = c(0L, 1L, 2L), new_cohort = FALSE)

  object@x <- c(object@x, 60, 60, 60)
  object@y <- c(object@y, 0L, 1L, 2L)
  object@nObs <- object@nObs + 3L
  object@ID <- c(object@ID, 11L, 12L, 13L)
  object@xLevel <- c(object@xLevel, 6L, 6L, 6L)
  object@cohort <- c(object@cohort, 6L, 6L, 6L)

  expect_valid(result, "DataOrdinal")
  expect_identical(result, object)
})

test_that("Update of DataOrdinal throws the error for a dose x out of the grid", {
  object <- h_get_data_ordinal()
  expect_error(
    update(object, x = 12345, y = c(0L, 1L, 1L), new_cohort = FALSE),
    ".*Dose values in x must be from doseGrid.*"
  )
})

test_that("Update of DataOrdinal, no error for non-valid update and validation off", {
  object <- h_get_data_ordinal()
  expect_silent(
    update(
      object,
      x = 12345,
      y = c(0L, 1L, 1L),
      new_cohort = FALSE,
      check = FALSE
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
    x_no_dlt = c(0.001, 25, 25, 25, 0.001, 50, 50, 50, 0.001, 100, 100),
    w_no_dlt = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 48),
    x_dlt = 100,
    w_dlt = 87
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected, no_dlt", {
  data <- h_get_data_dual()
  result <- getEff(data, no_dlt = TRUE)
  expected <- list(
    x_no_dlt = c(0.001, 25, 25, 25, 0.001, 50, 50, 50, 0.001, 100, 100),
    w_no_dlt = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 48)
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (no DLT)", {
  data <- DataDual(
    x = c(25, 50),
    y = c(0, 0),
    ID = 1:2,
    cohort = 1:2,
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data)
  expected <- list(
    x_no_dlt = c(25, 50),
    w_no_dlt = c(0.31, 0.42),
    x_dlt = NULL,
    w_dlt = NULL
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (no DLT), no_dlt", {
  data <- DataDual(
    x = c(25, 50),
    y = c(0, 0),
    ID = 1:2,
    cohort = 1:2,
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data, no_dlt = TRUE)
  expected <- list(
    x_no_dlt = c(25, 50),
    w_no_dlt = c(0.31, 0.42)
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (DLT only)", {
  data <- DataDual(
    x = c(25, 50),
    y = c(1, 1),
    ID = 1:2,
    cohort = 1:2,
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data)
  expected <- list(
    x_no_dlt = NULL,
    w_no_dlt = NULL,
    x_dlt = c(25, 50),
    w_dlt = c(0.31, 0.42)
  )

  expect_identical(result, expected)
})

test_that("getEff-DataDual works as expected (DLT only), no_dlt", {
  data <- DataDual(
    x = c(25, 50),
    y = c(1, 1),
    ID = 1:2,
    cohort = 1:2,
    w = c(0.31, 0.42),
    doseGrid = c(25, 50)
  )
  result <- getEff(data, no_dlt = TRUE)
  expected <- list(
    x_no_dlt = NULL,
    w_no_dlt = NULL
  )

  expect_identical(result, expected)
})

# ngrid ----

## generic ----

test_that("ngrid throws the error for non valid ignore_placebo", {
  expect_error(
    ngrid(NULL, ignore_placebo = c(TRUE, TRUE)),
    "Assertion on 'ignore_placebo' failed: Must have length 1."
  )
  expect_error(
    ngrid(NULL, ignore_placebo = 1),
    "Assertion on 'ignore_placebo' failed: Must be of type 'logical flag', not 'double'."
  )
})

## Data ----

test_that("ngrid-Data works as expected with placebo in grid", {
  data <- h_get_data()
  expect_identical(ngrid(data), 12L)
  expect_identical(ngrid(data, FALSE), 13L)

  data_1 <- Data(doseGrid = c(0.001, 25), placebo = TRUE)
  expect_identical(ngrid(data_1), 1L)
  expect_identical(ngrid(data_1, FALSE), 2L)

  data_2 <- Data(doseGrid = 0.001, placebo = TRUE)
  expect_identical(ngrid(data_2), 0L)
  expect_identical(ngrid(data_2, FALSE), 1L)

  data_empty <- Data(placebo = TRUE)
  expect_identical(ngrid(data_empty), 0L)
  expect_identical(ngrid(data_empty, FALSE), 0L)
})

test_that("ngrid-Data works as expected without placebo in grid", {
  data <- h_get_data(placebo = FALSE)
  expect_identical(ngrid(data), 12L)
  expect_identical(ngrid(data, FALSE), 12L)

  data_1 <- Data(doseGrid = 25, placebo = FALSE)
  expect_identical(ngrid(data_1), 1L)
  expect_identical(ngrid(data_1, FALSE), 1L)

  data_2 <- Data(doseGrid = 0.001, placebo = FALSE)
  expect_identical(ngrid(data_2), 1L)
  expect_identical(ngrid(data_2, FALSE), 1L)

  data_empty <- Data(placebo = FALSE)
  expect_identical(ngrid(data_empty), 0L)
  expect_identical(ngrid(data_empty, FALSE), 0L)
})

# dose_grid_range ----

## generic ----

test_that("dose_grid_range throws the error for non valid ignore_placebo", {
  data <- h_get_data()
  expect_error(
    dose_grid_range(data, ignore_placebo = c(TRUE, TRUE)),
    "Assertion on 'ignore_placebo' failed: Must have length 1."
  )
  expect_error(
    dose_grid_range(data, ignore_placebo = 1),
    "Assertion on 'ignore_placebo' failed: Must be of type 'logical flag', not 'double'."
  )
})

## Data ----

test_that("dose_grid_range-Data works as expected with placebo in grid", {
  data <- h_get_data()
  expect_identical(dose_grid_range(data), c(25, 300))
  expect_identical(dose_grid_range(data, FALSE), c(0.001, 300))

  data_1 <- Data(doseGrid = c(0.001, 25), placebo = TRUE)
  expect_identical(dose_grid_range(data_1), c(25, 25))
  expect_identical(dose_grid_range(data_1, FALSE), c(0.001, 25))

  data_2 <- Data(doseGrid = 0.001, placebo = TRUE)
  expect_identical(dose_grid_range(data_2), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_2, FALSE), c(0.001, 0.001))

  data_empty <- Data(placebo = TRUE)
  expect_identical(dose_grid_range(data_empty), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_empty, FALSE), c(-Inf, Inf))
})

test_that("dose_grid_range-Data works as expected without placebo in grid", {
  data <- h_get_data(placebo = FALSE)
  expect_identical(dose_grid_range(data), c(25, 300))
  expect_identical(dose_grid_range(data, FALSE), c(25, 300))

  data_1 <- Data(doseGrid = c(0.001, 25), placebo = FALSE)
  expect_identical(dose_grid_range(data_1), c(0.001, 25))
  expect_identical(dose_grid_range(data_1, FALSE), c(0.001, 25))

  data_2 <- Data(doseGrid = 10, placebo = FALSE)
  expect_identical(dose_grid_range(data_2), c(10, 10))
  expect_identical(dose_grid_range(data_2, FALSE), c(10, 10))

  data_empty <- Data(placebo = FALSE)
  expect_identical(dose_grid_range(data_empty), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_empty, FALSE), c(-Inf, Inf))
})

## DataOrdinal ----

test_that("dose_grid_range-DataOrdinal works as expected with placebo in grid", {
  data <- h_get_data_ordinal()
  expect_identical(dose_grid_range(data), c(10, 100))
  expect_identical(dose_grid_range(data, FALSE), c(10, 100))

  data_1 <- DataOrdinal(doseGrid = c(0.001, 25), placebo = TRUE)
  expect_identical(dose_grid_range(data_1), c(25, 25))
  expect_identical(dose_grid_range(data_1, FALSE), c(0.001, 25))

  data_2 <- DataOrdinal(doseGrid = 0.001, placebo = TRUE)
  expect_identical(dose_grid_range(data_2), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_2, FALSE), c(0.001, 0.001))

  data_empty <- DataOrdinal(placebo = TRUE)
  expect_identical(dose_grid_range(data_empty), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_empty, FALSE), c(-Inf, Inf))
})

test_that("dose_grid_range-DataOrdinal works as expected without placebo in grid", {
  data <- h_get_data_ordinal()
  data@placebo <- TRUE
  expect_identical(dose_grid_range(data), c(20, 100))
  expect_identical(dose_grid_range(data, FALSE), c(10, 100))

  data_1 <- DataOrdinal(doseGrid = c(0.001, 25), placebo = FALSE)
  expect_identical(dose_grid_range(data_1), c(0.001, 25))
  expect_identical(dose_grid_range(data_1, FALSE), c(0.001, 25))

  data_2 <- DataOrdinal(doseGrid = 10, placebo = FALSE)
  expect_identical(dose_grid_range(data_2), c(10, 10))
  expect_identical(dose_grid_range(data_2, FALSE), c(10, 10))

  data_empty <- DataOrdinal(placebo = FALSE)
  expect_identical(dose_grid_range(data_empty), c(-Inf, Inf))
  expect_identical(dose_grid_range(data_empty, FALSE), c(-Inf, Inf))
})

test_that("tidy-DataGeneral creates the correct tibble", {
  d <- Data(
    x = c(1, 3, 5),
    y = c(0, 0, 0),
    doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    placebo = FALSE,
    ID = 1:3,
    cohort = 1:3
  )
  expected <- tibble(
    ID = 1:3,
    Cohort = 1:3,
    Dose = c(1, 3, 5),
    XLevel = 1:3,
    Tox = FALSE,
    Placebo = FALSE,
    NObs = 3,
    NGrid = 11,
    DoseGrid = list(c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))
  )
  class(expected) <- c("tbl_Data", class(expected))

  expect_equal(tidy(d), expected)

  d@ID <- 5:7
  expected$ID <- 5:7
  expect_equal(tidy(d), expected)

  d@cohort <- 5:7
  expected$Cohort <- 5:7
  expect_equal(tidy(d), expected)

  d@x[3] <- 10
  expected$Dose[3] <- 10
  expect_equal(tidy(d), expected)

  d@xLevel[3] <- 4L
  expected$XLevel[3] <- 4L
  expect_equal(tidy(d), expected)

  d@placebo <- TRUE
  expected$Placebo <- TRUE
  expect_equal(tidy(d), expected)

  d@y <- c(0L, 1L, 0L)
  expected$Tox <- c(FALSE, TRUE, FALSE)
  expect_equal(tidy(d), expected)
})

test_that("tidy-Dataordinal creates the correct tibble", {
  tidyData <- .DefaultDataOrdinal() %>% tidy()
  x <- .DefaultDataOrdinal() %>% tidy()
  actual <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      AnyTox = any(dplyr::across(c(starts_with("Cat"), -Cat0), any)),
      ExpectedCat0 = !AnyTox
    )

  expect_equal(actual$Cat0, actual$ExpectedCat0)
})
