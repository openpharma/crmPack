# Define the dose-grid.
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))


# Initialize the CRM model.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)


my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(my_size1, my_size2)

# Choose the rule for stopping.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping1 & my_stopping2) | my_stopping3 | StoppingMissingDose()

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

my_options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

\donttest{
examine(my_design, my_options)
}

# Example where examine stops because stopping rule already fulfilled.
my_stopping4 <- StoppingMinPatients(nPatients = 3)
my_stopping <- (my_stopping1 & my_stopping2) | my_stopping4

my_design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

\donttest{
examine(my_design, mcmcOptions = my_options)
}

# Example where examine stops because infinite looping
# (note that here a very low threshold is used for the parameter
# "maxNoIncrement" in "examine" to keep the execution time short).
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.00001)
)

my_stopping <- (my_stopping1 & my_stopping2) | StoppingMissingDose()

design <- Design(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

\donttest{
examine(my_design, mcmcOptions = my_options, maxNoIncrement = 2)
}