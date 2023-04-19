empty_data <- Data(doseGrid = seq(25, 300, 25))

my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = empty_data
)

# The escalation rule.
my_next_best <- NextBestTD(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

my_size <- CohortSizeConst(size = 3)

# The increments for the dose-escalation process:
#  the maximum increase of 200% for doses up to the maximum dose in grid,
#  the maximum increase of 200% for dose above the maximum dose in grid.
my_increments <- IncrementsRelative(
  intervals = range(empty_data@doseGrid),
  increments = c(2, 2)
)

# Stop when the maximum sample size of 36 patients is reached.
my_stopping <- StoppingMinPatients(nPatients = 36)

# The design with all the above information and starting with a dose of 25.
# This design incorporates only DLT responses and no DLT samples are involved
# during the simulation.
design <- TDDesign(
  model = my_model,
  stopping = my_stopping,
  increments = my_increments,
  nextBest = my_next_best,
  cohortSize = my_size,
  data = empty_data,
  startingDose = 25
)
