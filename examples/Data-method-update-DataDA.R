# Create an object of class 'DataDA'.
my_data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60
)

# Update the data.
my_data1 <- update(
  object = my_data,
  y = c(my_data@y, 0), # The y will be updated according to u.
  u = c(my_data@u, 20),
  t0 = c(my_data@t0, 5),
  x = 20,
  trialtime = 120 # This is the global timeline for a trial.
)
