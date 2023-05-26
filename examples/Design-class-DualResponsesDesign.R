empty_data <- DataDual(doseGrid = seq(25, 300, 25))

tox_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = empty_data
)

eff_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = empty_data
  )

my_nextbest <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

my_increments <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)

my_size <- CohortSizeConst(size = 3)
my_stopping <- StoppingMinPatients(nPatients = 36)

design <- DualResponsesDesign(
  nextBest = my_next_best,
  cohortSize = my_size,
  startingDose = 25,
  model = tox_model,
  eff_model = eff_model,
  data = empty_data,
  stopping = my_stopping,
  increments = my_increments
)
