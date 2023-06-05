## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(crmPack)

## -----------------------------------------------------------------------------
# Define the dose grid.
empty_data <- Data(doseGrid = c(1, 3, 9, 20, 30, 45, 60, 80, 100))

## -----------------------------------------------------------------------------
# Initialize the CRM model.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

## ---- fig.width=5-------------------------------------------------------------
vignetteMcmcOptions <- McmcOptions(burnin = 100, step = 2, samples = 1000)
prior_samples <- mcmc(
  data = empty_data,
  model = model,
  options = vignetteMcmcOptions
)
plot(prior_samples, model, empty_data)

## -----------------------------------------------------------------------------
# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 30),
  increments = c(1, 0.5)
)

## -----------------------------------------------------------------------------
# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

## -----------------------------------------------------------------------------
# Choose the rule for the cohort size.
my_size_1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size_2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(my_size_1, my_size_2)

## -----------------------------------------------------------------------------
# Choose the rule for stopping.
my_stopping_1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping_2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping_3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping_1 & my_stopping_2) | my_stopping_3

## -----------------------------------------------------------------------------
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

