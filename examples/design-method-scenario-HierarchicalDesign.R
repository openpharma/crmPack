# nolint start

dose_grid <- c(1, 3, 5, 10, 15, 20, 25)

# Define hypothetical observed data for two related arms.
data <- HierarchicalData(
  arm_a = Data(
    x = c(1, 3, 3, 5),
    y = c(0, 0, 0, 1),
    cohort = c(1, 2, 2, 3),
    doseGrid = dose_grid
  ),
  arm_b = Data(
    x = c(1, 1, 3, 3),
    y = c(0, 0, 0, 0),
    cohort = c(1, 1, 2, 2),
    doseGrid = dose_grid
  )
)

model_a <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)
model_b <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)

next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
stopping <- StoppingMinPatients(nPatients = 20) | StoppingMissingDose()
increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

design_a <- Design(
  model = model_a,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = dose_grid),
  startingDose = 1
)
design_b <- Design(
  model = model_b,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = dose_grid),
  startingDose = 1
)

design <- HierarchicalDesign(
  DesignArm(
    name = "arm_a",
    design = design_a
  ),
  DesignArm(
    name = "arm_b",
    design = design_b
  ),
  exchangeable_parameters = list(
    intercept = list(
      arm_a = "alpha0",
      arm_b = "alpha0"
    ),
    slope = list(
      arm_a = "alpha1",
      arm_b = "alpha1"
    )
  )
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
