# Create some data of class 'Data'.
my_data <- Data(
  x = c(0.001, 0.1, 0.1, 0.5, 0.001, 3, 3, 0.001, 10, 10, 10),
  y = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0),
  cohort = c(1, 1, 1, 2, 3, 3, 3, 4, 4, 4, 4),
  doseGrid = c(0.001, 0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  placeb = TRUE
)

# Plot the data.
plot(my_data)
