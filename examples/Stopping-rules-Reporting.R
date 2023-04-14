# Define the dose-grid.
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- LogisticLogNormal(
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

# Choose the rule for the cohort-size.
my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(mySize1, mySize2)

my_stopping1 <- StoppingMinCohorts(nCohorts = 3, report_label = "")
my_stopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5, report_label = "Stopping2")
my_stopping3 <- StoppingMinPatients(nPatients = 20, report_label = "Stopping3")

my_stopping <- StoppingAll(
  stop_list =
    list(
      StoppingAny(
        stop_list =
          list(my_stopping1, my_stopping3),
        report_label = "StoppingAnyLabel"
      ),
      my_stopping2
    ),
  report_label = "StoppingAllLabel"
)

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
my_design <- Design(
  model = model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohortSize = my_size,
  data = emptydata,
  startingDose = 3
)

## define the true function.
my_truth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Run the simulation on the desired design.
# We only generate 1 trial outcomes here for illustration, for the actual study
# this should be increased of course
my_options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 1000
)
time <- system.time(my_sims <- simulate(my_design,
  args = NULL,
  truth = my_truth,
  nsim = 3,
  seed = 819,
  mcmcOptions = my_options,
  parallel = FALSE
))[3]

# Summarize the Results of the Simulations.
summary(my_sims, truth = my_truth)
