# Data ----

h_get_data <- function(empty = FALSE, placebo = TRUE) {
  dose_grid <- seq(25, 300, 25)
  if (placebo) {
    dose_grid <- c(0.001, dose_grid)
  }

  if (empty) {
    Data(
      doseGrid = dose_grid,
      placebo = placebo
    )
  } else {
    x <- if (placebo) {
      c(0.001, 25, 25, 25, 0.001, 50, 50, 50, 0.001, 100, 100, 100)
    } else {
      c(25, 25, 25, 25, 50, 50, 50, 50, 100, 100, 100, 100)
    }
    Data(
      x = x,
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      doseGrid = dose_grid,
      placebo = placebo,
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      response = as.integer(c(0, 1, 0, NA, 0, 1, NA, 0, 0, 1, 1, NA)),
      backfilled = c(
        FALSE,
        FALSE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        TRUE,
        FALSE,
        FALSE,
        TRUE,
        TRUE
      )
    )
  }
}

# Sample data to test e.g. maxDose of IncrementsDoseLevels method.
h_get_data_1 <- function() {
  Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 12, 12, 12, 16, 16, 16, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0),
    ID = 1:17,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
  )
}

# Used e.g. by mcmc for LogisticKadaneBetaGamma.
h_get_data_2 <- function() {
  Data(
    x = c(1.5, 1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5),
    y = c(0, 0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1:9,
    cohort = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    doseGrid = c(1.5, 2.5, 3.5, 4.5, 6, 7),
    placebo = FALSE
  )
}

# DataDual ----

h_get_data_dual <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataDual(d)
  } else {
    .DataDual(
      d,
      w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48)
    )
  }
}

# DataParts ----

h_get_data_parts <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataParts(d)
  } else {
    .DataParts(
      d,
      part = c(1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L),
      nextPart = 1L,
      part1Ladder = seq(25, 250, 25)
    )
  }
}

h_get_data_parts_1 <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataParts(d)
  } else {
    .DataParts(
      d,
      part = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
      nextPart = 1L,
      part1Ladder = seq(25, 250, 25)
    )
  }
}

# DataMixture ----

h_get_data_mixture <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataMixture(d)
  } else {
    .DataMixture(
      d,
      xshare = seq(25, 100, 25),
      yshare = c(0L, 1L, 1L, 1L),
      nObsshare = 4L
    )
  }
}

# DataDA ----

h_get_data_da <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataDA(d)
  } else {
    .DataDA(
      d,
      u = c(42, 30, 15, 5, 20, 25, 30, 60, 25, 30, 35, 40),
      t0 = c(0, 15, 30, 40, 55, 70, 75, 85, 95, 105, 120, 125),
      Tmax = 60
    )
  }
}

h_get_data_da_2 <- function() {
  DataDA(
    u = c(42, 30, 15, 5, 20, 25, 30, 60),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
    Tmax = 60,
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(1, 0, 1, 0, 0, 0, 1, 0),
    ID = 1:8,
    cohort = c(1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
}

# Sample data to test stopping rule of StoppingSpecificDose method.
h_get_data_sr_1 <- function() {
  Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1:8,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
}

# Sample data to test stopping rule of StoppingSpecificDose method.
h_get_data_sr_2 <- function() {
  Data(
    x = c(
      0.1,
      0.5,
      1.5,
      3,
      6,
      10,
      10,
      10,
      50,
      50,
      50,
      80,
      80,
      80,
      80,
      80,
      80,
      80,
      80,
      80,
      80,
      80,
      80
    ),
    y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    ID = 1:23,
    cohort = c(
      0,
      1,
      2,
      3,
      4,
      5,
      5,
      5,
      6,
      6,
      6,
      7,
      7,
      7,
      8,
      8,
      8,
      9,
      9,
      9,
      10,
      10,
      10
    ),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )
}

# Sample ordinal data ----
h_get_data_ordinal <- function() {
  DataOrdinal(
    x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
    y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
    ID = 1L:10L,
    cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
    doseGrid = c(seq(from = 10, to = 100, by = 10)),
    yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
    placebo = FALSE
  )
}

# DataGrouped ----

h_get_data_grouped <- function(empty = FALSE, placebo = TRUE) {
  d <- h_get_data(empty, placebo)
  if (empty) {
    .DataGrouped(d)
  } else {
    .DataGrouped(
      d,
      group = factor(
        c(
          "mono",
          "mono",
          "combo",
          "combo",
          "mono",
          "mono",
          "combo",
          "combo",
          "mono",
          "mono",
          "combo",
          "combo"
        ),
        levels = c("mono", "combo")
      )
    )
  }
}
