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
exp_cond_cdf <- function(x, onset = 15) {
  # nolint
  a <- stats::pexp(28, 1 / onset, lower.tail = FALSE)
  1 - (stats::pexp(x, 1 / onset, lower.tail = FALSE) - a) / (1 - a)
}

.default_da_simulations <- simulate(
  design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond_cdf,
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

# DualSimulations ----

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
  maxDens <- (delta1^delta1) *
    (delta2^delta2) /
    ((delta1 + delta2)^(delta1 + delta2))
  dose <- dose / scal
  e0 + eMax / maxDens * (dose^delta1) * (1 - dose)^delta2
}

true_biomarker <- function(dose) {
  beta_mod(
    dose,
    e0 = 0.2,
    eMax = 0.6,
    delta1 = 5,
    delta2 = 5 * 0.5 / 0.5,
    scal = 100
  )
}

true_tox <- function(dose) {
  pnorm((dose - 60) / 10)
}

.default_dual_simulations <- simulate(
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
  .default_dual_simulations,
  testthat::test_path("fixtures", "default_dual_simulations.Rds")
)

# PseudoSimulations ----

# Create data
pseudo_data <- Data(doseGrid = seq(25, 300, 25))

# Create model
pseudo_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = pseudo_data
)

# Create NextBest rule
pseudo_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

# Create design
pseudo_design <- TDsamplesDesign(
  model = pseudo_model,
  nextBest = pseudo_next_best,
  stopping = StoppingMinPatients(nPatients = 12),
  increments = IncrementsRelative(
    intervals = c(25, 300),
    increments = c(2, 2)
  ),
  cohort_size = CohortSizeConst(size = 3),
  data = pseudo_data,
  startingDose = 25
)

# Create truth function
pseudo_truth <- probFunction(pseudo_model, phi1 = -53.66584, phi2 = 10.50499)

.default_pseudo_simulations <- simulate(
  object = pseudo_design,
  args = NULL,
  truth = pseudo_truth,
  nsim = 1,
  seed = 819,
  mcmcOptions = .DefaultMcmcOptions(),
  parallel = FALSE
)

saveRDS(
  .default_pseudo_simulations,
  testthat::test_path("fixtures", "default_pseudo_simulations.Rds")
)

# PseudoDualSimulations ----

# Create dual data
pseudo_dual_data <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

# Create DLE model
pseudo_dle_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = pseudo_dual_data
)

# Create efficacy model
pseudo_eff_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = pseudo_dual_data
)

# Create dual design
pseudo_dual_design <- DualResponsesDesign(
  nextBest = NextBestMaxGain(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3
  ),
  model = pseudo_dle_model,
  eff_model = pseudo_eff_model,
  stopping = StoppingMinPatients(nPatients = 12),
  increments = IncrementsRelative(
    intervals = c(25, 300),
    increments = c(2, 2)
  ),
  cohort_size = CohortSizeConst(size = 3),
  data = pseudo_dual_data,
  startingDose = 25
)

# Create truth functions
pseudo_truth_dle <- probFunction(
  pseudo_dle_model,
  phi1 = -53.66584,
  phi2 = 10.50499
)
pseudo_truth_eff <- efficacyFunction(
  pseudo_eff_model,
  theta1 = -4.818429,
  theta2 = 3.653058
)

.default_pseudo_dual_simulations <- simulate(
  object = pseudo_dual_design,
  args = NULL,
  trueDLE = pseudo_truth_dle,
  trueEff = pseudo_truth_eff,
  trueNu = 1 / 0.025,
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

saveRDS(
  .default_pseudo_dual_simulations,
  testthat::test_path("fixtures", "default_pseudo_dual_simulations.Rds")
)

# PseudoDualFlexiSimulations ----

## Simulate dose-escalation procedure based on DLE and efficacy responses where DLE
## and efficacy samples are used
data <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

## First for the DLE model
## The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

## The escalation rule using the 'NextBestMaxGainSamples' class
mynextbest <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

## The increments (see Increments class examples)
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)

## cohort size of 3
mySize <- CohortSizeConst(size = 3)

## Stop only when 10 subjects are treated (only for illustration such a low
## sample size)
myStopping <- StoppingMinPatients(nPatients = 10)

## Simulate dose-escalation procedure based on DLE and efficacy responses where DLE
## and efficacy samples are used
## when the efficacy model is of 'EffFlexi' class

Effmodel <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)


## Specified the design
design <- DualResponsesSamplesDesign(
  nextBest = mynextbest,
  cohort_size = mySize,
  startingDose = 25,
  model = DLEmodel,
  eff_model = Effmodel,
  data = data,
  stopping = myStopping,
  increments = myIncrements
)
## specified the true DLE curve and the true expected efficacy values at all dose levels
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)

myTruthEff <- c(
  -0.5478867,
  0.1645417,
  0.5248031,
  0.7604467,
  0.9333009,
  1.0687031,
  1.1793942,
  1.2726408,
  1.3529598,
  1.4233411,
  1.4858613,
  1.5420182
)
## The true gain curve can also be seen
d1 <- data@doseGrid
myTruthGain <- (myTruthEff) / (1 + (myTruthDLE(d1) / (1 - myTruthDLE(d1))))

.default_pseudo_dual_flexi_simulations <- simulate(
  object = design,
  args = NULL,
  trueDLE = myTruthDLE,
  trueEff = myTruthEff,
  trueSigma2 = 0.025,
  trueSigma2betaW = 1,
  mcmcOptions = .DefaultMcmcOptions(),
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

saveRDS(
  .default_pseudo_dual_flexi_simulations,
  testthat::test_path("fixtures", "default_pseudo_dual_flexi_simulations.Rds")
)
