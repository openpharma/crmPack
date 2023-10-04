# nolint start

# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov =
    matrix(c(1, -0.5, -0.5, 1),
      nrow = 2
    ),
  ref_dose = 56
)

# Choose the rule for selecting the next dose
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size
mySize1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1L, 3L)
)
mySize2 <- CohortSizeDLT(
  intervals = c(0L, 1L),
  cohort_size = c(1L, 3L)
)
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingMinCohorts(nCohorts = 3L)
myStopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
myStopping3 <- StoppingMinPatients(nPatients = 20L)
myStopping <- (myStopping1 & myStopping2) | myStopping3

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design
design <- Design(
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  cohort_size = mySize,
  data = emptydata,
  startingDose = 3
)

## define the true function
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Run the simulation on the desired design
# We only generate 1 trial outcomes here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(
  burnin = 100,
  step = 1,
  samples = 2000
)
time <- system.time(mySims <- simulate(design,
  args = NULL,
  truth = myTruth,
  nsim = 1,
  seed = 819,
  mcmcOptions = options,
  parallel = FALSE
))[3]

# nolint end
