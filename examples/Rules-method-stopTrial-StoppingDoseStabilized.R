data <- Data(
  x = c(1, 2, 3, 4),
  y = c(0, 1, 0, 0),
  cohort = 1:4,
  ID = 1:4,
  doseGrid = 1:4
)

# Stop if the next best dose has been administered to the previous cohort already.
my_stopping <- StoppingDoseStabilized()
stopTrial(stopping = my_stopping, dose = 4, data = data)
