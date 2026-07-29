# Stop when the next best dose has remained unchanged for three consecutive
# cohorts.
my_stopping <- StoppingDoseStabilized(nCohorts = 3)
