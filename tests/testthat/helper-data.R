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
