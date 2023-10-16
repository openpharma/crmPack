# Create some data of class 'DataOrdinal'.
my_data <- DataOrdinal(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1L:8L,
  cohort = c(1L:5L, 6L, 6L, 6L),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  yCategories = c("No tox" = 0L, "AE" = 1L, "DLT" = 2L)
)

# Update the data with a new cohort.
my_data1 <- update(my_data, x = 20, y = c(0L, 1L, 2L))
my_data1
