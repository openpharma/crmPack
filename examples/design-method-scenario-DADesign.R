# nolint start

# Define a hypothetical time-to-DLT scenario.
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  ID = 1L:8L,
  cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6))
)

npiece <- 10
t_max <- 60
lambda_prior <- function(k) {
  npiece / (t_max * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece,
  l = as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece), 2, lambda_prior))),
  c_par = 2
)

size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

design <- DADesign(
  model = model,
  increments = IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  ),
  nextBest = NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  ),
  stopping = StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  ) | StoppingMinPatients(nPatients = 50) | StoppingMissingDose(),
  cohort_size = maxSize(size1, size2),
  data = DataDA(doseGrid = data@doseGrid, Tmax = data@Tmax),
  safetyWindow = SafetyWindowConst(c(6, 2), 7, 7),
  startingDose = 3
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
