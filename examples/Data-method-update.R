# Create some data of class 'Data'.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Update the data with a new cohort.
my_data1 <- update(my_data, x = 20, y = c(0L, 1L, 1L))
