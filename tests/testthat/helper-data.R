h_get_data <- function() {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100)
  dose_grid <- c(plcb, seq(25, 300, 25))

  Data(
    x = x,
    y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
    doseGrid = dose_grid,
    placebo = TRUE,
    ID = 1:12,
    cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
  )
}

h_get_data_no_plcb <- function() {
  x <- c(25, 25, 25, 50, 50, 50, 100, 100, 100)
  dose_grid <- c(seq(25, 300, 25))

  Data(
    x = x,
    y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
    doseGrid = dose_grid,
    placebo = FALSE,
    ID = 1:9,
    cohort = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L)
  )
}

h_get_data_dual <- function() {
  d <- h_get_data()
  .DataDual(
    d,
    w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48)
  )
}

h_get_data_parts <- function() {
  d <- h_get_data()
  .DataParts(
    d,
    part = c(1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L),
    nextPart = 1L,
    part1Ladder = seq(25, 250, 25)
  )
}

h_get_data_mixture <- function() {
  d <- h_get_data()
  .DataMixture(
    d,
    xshare = seq(25, 100, 25),
    yshare = c(0L, 1L, 1L, 1L),
    nObsshare = 4L
  )
}

h_get_data_augmented <- function() { # nolintr
  d <- h_get_data()
  .DataDA(
    d,
    u = c(42, 30, 15, 5, 20, 25, 30, 60, 25, 30, 35, 40),
    t0 = c(0, 15, 30, 40, 55, 70, 75, 85, 95, 105, 120, 125),
    Tmax = 60
  )
}
