# Create data in which the last three cohorts received the same dose.
data <- Data(
  x = c(1, 2, 2, 2),
  y = c(0, 0, 0, 0),
  cohort = 1:4,
  ID = 1:4,
  doseGrid = 1:3
)

# Stop if the next best dose has been administered to three consecutive
# cohorts.
my_stopping <- StoppingDoseStabilized(nCohorts = 3)
stopTrial(stopping = my_stopping, dose = 2, data = data)
