#' `h_get_data`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is the helper function which creates a [`Data-class`] object
#' for an example data.
#'
#' @return The [`Data-class`] object.
#'
h_get_data <- function() {
  plcb <- 0.001
  x <- c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100)
  dose_grid <- c(plcb, seq(25, 300, 25))

  .Data(
    x = x,
    y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
    doseGrid = dose_grid,
    nGrid = length(dose_grid),
    xLevel = matchTolerance(x = x, table = dose_grid),
    placebo = TRUE,
    ID = 1:12,
    cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
    nObs = 12L
  )
}
