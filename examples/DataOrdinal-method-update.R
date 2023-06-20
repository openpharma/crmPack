d <- DataOrdinal(
  x = c(25, 25, 25, 25, 50, 50, 50, 50, 100, 100, 100, 100),
  y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE,
  ID = 1:12,
  cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
  yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L)
)

update(d, x = 50, y = c(0L, 0L, 1L))
