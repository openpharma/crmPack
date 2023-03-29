# Define the dose grid.
empty_data <- Data(
  doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
  x = c(1, 3, 5, 10, 15),
  y = c(0, 0, 0, 0, 1)
)

# Initialize the CRM model.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  refDose = 56
)

# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

# Choose the rule for the cohort size.
my_size_1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohortSize = c(1, 3)
)
my_size_2 <- CohortSizeDLT(
  DLTintervals = c(0, 1),
  cohortSize = c(1, 3)
)
my_size <- maxSize(my_size_1, my_size_2)

# Choose the rule for stopping.
my_stopping_1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping_2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping_3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping_1 & my_stopping_2) | my_stopping_3

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohortSize = my_size,
  data = empty_data,
  startingDose = 3
)

samples <- mcmc(empty_data, model, McmcOptions(burnin = 5000, step = 2, samples = 20000))

samples %>% tidy()
