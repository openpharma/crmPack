# nolint start

# Define the dose-grid and a hypothetical observed data scenario.
data <- Data(
  x = c(1, 3, 3, 5, 5, 5),
  y = c(0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 2, 3, 3, 3),
  doseGrid = c(1, 3, 5, 10, 15, 20, 25)
)

# Initialize the CRM model.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for stopping.
stopping <- StoppingMinPatients(nPatients = 20) | StoppingMissingDose()

# Choose the rule for dose increments.
increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = model,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = data@doseGrid), # empty data here.
  startingDose = 1
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

\donttest{
result <- scenario(design, data, options)
result$fit
result$next_dose
result$cohort_size
result$stop
}

# nolint end
