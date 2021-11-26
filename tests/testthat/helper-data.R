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

h_get_data_dual <- function() {
  d <- h_get_data()
  DataDual(
    w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48),
    x = d@x,
    y = d@y,
    doseGrid = d@doseGrid,
    placebo = d@placebo,
    ID = d@ID,
    cohort = d@cohort
  )
}

h_get_data_parts <- function() {
  d <- h_get_data()
  DataParts(
    part = c(1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L),
    nextPart = 1L,
    part1Ladder = seq(25, 250, 25),
    x = d@x,
    y = d@y,
    doseGrid = d@doseGrid,
    placebo = d@placebo,
    ID = d@ID,
    cohort = d@cohort
  )
}

h_get_data_mixture <- function() {
  d <- h_get_data()
  DataMixture(
    xshare = seq(25, 100, 25),
    yshare = c(0L, 1L, 1L, 1L),
    nObsshare = 4L,
    x = d@x,
    y = d@y,
    doseGrid = d@doseGrid,
    placebo = d@placebo,
    ID = d@ID,
    cohort = d@cohort
  )
}
