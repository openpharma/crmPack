data <- DataOrdinal(
  x = c(10, 20, 30, 40, 50, 50, 50, 60, 60, 60),
  y = as.integer(c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2)),
  ID = 1L:10L,
  cohort = as.integer(c(1:4, 5, 5, 5, 6, 6, 6)),
  doseGrid = c(seq(from = 10, to = 100, by = 10)),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L),
  placebo = FALSE
)

plot(data)
