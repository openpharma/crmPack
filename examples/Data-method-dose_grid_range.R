my_data <- Data(
  x = c(10, 50, 90, 100, 0.001, 20, 30, 30),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(1L, 2L, 3L, 4L, 5L, 5L, 6L, 6L),
  doseGrid = c(0.001, seq(from = 10, to = 100, by = 10)),
  placebo = TRUE
)
dose_grid_range(my_data)
dose_grid_range(my_data, ignore_placebo = FALSE)
