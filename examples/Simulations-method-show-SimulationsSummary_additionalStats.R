# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size
my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(my_size1, my_size2)

# Choose the rule for stopping
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping1 & my_stopping2) |
  my_stopping3 |
  StoppingMissingDose()

# Choose the rule for dose increments
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design
design <- Design(
  model = model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 3
)

## define the true function
my_truth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Run the simulation on the desired design
# We only generate 1 trial outcome here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 1000
)
time <- system.time(
  my_sims <- simulate(
    design,
    args = NULL,
    truth = my_truth,
    nsim = 1,
    seed = 819,
    mcmcOptions = options,
    # add list with additional statistics to be reported
    derive = list(
      max_mtd = max,
      mean_mtd = mean,
      median_mtd = median
    ),
    parallel = FALSE
  )
)[3]

# Show the Summary of the Simulations
show(summary(my_sims, truth = my_truth))
