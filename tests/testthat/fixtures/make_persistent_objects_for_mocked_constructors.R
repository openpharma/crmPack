# Some default constructors (and other functions) have long execution times
# To speed up unit testing, create persistent versions of the objects returned by
# these default constructors.  As an example, at the time of writing, the
# execution time of test-CrmPackClass-class.R is reduced frm 67 to 3 seconds
# by the use of these mocks.

# Simulations ----

design <- .DefaultDesign()
myTruth <- probFunction(design@model, alpha0 = 7, alpha1 = 8)

.default_simulations <- simulate(
  design,
  args = NULL,
  truth = myTruth,
  nsim = 1,
  seed = 819,
  mcmcOptions = .DefaultMcmcOptions(),
  parallel = FALSE
)

saveRDS(
  .default_simulations,
  testthat::test_path("fixtures", "default_simulations.Rds")
)

# DASimulations ----

design <- .DefaultDADesign()
myTruth <- probFunction(design@model, alpha0 = 2, alpha1 = 3)
exp_cond.cdf <- function(x, onset = 15) {  #nolint
  a <- stats::pexp(28, 1 / onset, lower.tail = FALSE)
  1 - (stats::pexp(x, 1 / onset, lower.tail = FALSE) - a) / (1 - a)
}

.default_da_simulations <- simulate(
  design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond.cdf,
  trueTmax = 80,
  nsim = 2,
  seed = 819,
  mcmcOptions = .DefaultMcmcOptions(),
  firstSeparate = TRUE,
  deescalate = FALSE,
  parallel = FALSE
)

saveRDS(
  .default_da_simulations,
  testthat::test_path("fixtures", "default_da_simulations.Rds")
)

# DualSimulationsSummary ----

empty_data <- DataDual(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 30))

my_model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  rw1 = TRUE
)

my_next_best <- NextBestDualEndpoint(
  target = c(0.9, 1),
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

my_stopping1 <- StoppingTargetBiomarker(
  target = c(0.9, 1),
  prob = 0.5
)

my_stopping <- my_stopping1 | StoppingMinPatients(10) | StoppingMissingDose()

my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

my_design <- DualDesign(
  model = my_model,
  data = empty_data,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = CohortSizeConst(3),
  startingDose = 3
)

beta_mod <- function(dose, e0, eMax, delta1, delta2, scal) {
  maxDens <- (delta1^delta1) * (delta2^delta2) / ((delta1 + delta2)^(delta1 + delta2))
  dose <- dose / scal
  e0 + eMax / maxDens * (dose^delta1) * (1 - dose)^delta2
}

true_biomarker <- function(dose) {
  beta_mod(dose, e0 = 0.2, eMax = 0.6, delta1 = 5, delta2 = 5 * 0.5 / 0.5, scal = 100)
}

true_tox <- function(dose) {
  pnorm((dose - 60) / 10)
}

.default_dual_simulations_summary <- simulate(
  object = my_design,
  trueTox = true_tox,
  trueBiomarker = true_biomarker,
  sigma2W = 0.01,
  rho = 0,
  nsim = 1,
  parallel = FALSE,
  seed = 3,
  startingDose = 6,
  mcmcOptions = .DefaultMcmcOptions()
)

saveRDS(
  .default_dual_simulations_summary,
  testthat::test_path("fixtures", "default_dual_simulations_summary.Rds")
)
