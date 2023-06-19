# nolint start

# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))

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
  cohort_size = c(1, 3)
)
mySize2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
myStopping3 <- StoppingMinPatients(nPatients = 20)
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
  cohortSize = mySize,
  data = emptydata,
  startingDose = 3
)

# Examine the design
set.seed(4235)
# MCMC parameters are set to small values only to show this example. They should be
# increased for a real case.
options <- McmcOptions(burnin = 10, step = 1, samples = 20)
examine(design, options)

## example where examine stops because stopping rule already fulfilled
myStopping4 <- StoppingMinPatients(nPatients = 3)
myStopping <- (myStopping1 & myStopping2) | myStopping4
design <- Design(
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  cohortSize = mySize,
  data = emptydata,
  startingDose = 3
)
examine(design, mcmcOptions = options)

## example where examine stops because infinite looping
## (note that here a very low threshold is used for the parameter
## "maxNoIncrement" in "examine" to keep the execution time short)
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.00001)
)
myStopping <- (myStopping1 & myStopping2) | StoppingMissingDose()
design <- Design(
  model = model,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  cohortSize = mySize,
  data = emptydata,
  startingDose = 3
)
examine(design, mcmcOptions=options, maxNoIncrement = 2)

# nolint end
