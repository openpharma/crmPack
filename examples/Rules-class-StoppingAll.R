# Define some stopping rules.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
my_stopping3 <- StoppingMinPatients(nPatients = 20)

# Create a list of stopping rules (of class `StoppingAll`) which would then be
# summarized by the `all` function, meaning that the study would be stopped only
# if all of the single stopping rules are `TRUE`.
my_stopping <- StoppingAll(
  stop_list = c(my_stopping1, my_stopping2, my_stopping3)
)
