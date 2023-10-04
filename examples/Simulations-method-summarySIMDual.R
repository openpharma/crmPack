# Obtain the plot for the simulation results if DLE and efficacy responses
# are considered in the simulations.

# Specified simulations when no samples are used.
emptydata <- DataDual(doseGrid = seq(25, 300, 25))

# The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class.
dle_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

# The efficacy model of 'ModelEff' (e.g 'Effloglog') class.
eff_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# The escalation rule using the 'NextBestMaxGain' class.
my_next_best <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

# Allow increase of 200%.
my_increments <- IncrementsRelative(intervals = 0, increments = 2)

# Cohort size of 3.
my_size <- CohortSizeConst(size = 3L)

# Stop when 36 subjects are treated or next dose is NA.
my_stopping <- StoppingMinPatients(nPatients = 36L) | StoppingMissingDose()

# Specify the design. (For details please refer to the 'DualResponsesDesign' example.)
my_design <- DualResponsesDesign(
  nextBest = my_next_best,
  model = dle_model,
  eff_model = eff_model,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# Specify the true DLE and efficacy curves.
my_truth_dle <- probFunction(dle_model, phi1 = -53.66584, phi2 = 10.50499)
my_truth_eff <- efficacyFunction(eff_model, theta1 = -4.818429, theta2 = 3.653058)

# Specify the simulations and generate the 2 trials.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff,
  trueNu = 1 / 0.025,
  nsim = 2,
  seed = 819,
  parallel = FALSE
)

# Produce a summary of the simulations.
summary(
  my_sim,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff
)

# Example where DLE and efficacy samples are involved.
# Please refer to design-method 'simulate DualResponsesSamplesDesign' examples for details.

# Specify the next best rule.
my_next_best <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

# Specify the design.
my_design <- DualResponsesSamplesDesign(
  nextBest = my_next_best,
  cohort_size = my_size,
  startingDose = 25,
  model = dle_model,
  eff_model = eff_model,
  data = emptydata,
  stopping = my_stopping,
  increments = my_increments
)

# For illustration purpose 50 burn-ins to generate 200 samples are used.
my_options <- McmcOptions(burnin = 50, step = 2, samples = 200)

# For illustration purpose 2 simulation are created.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff,
  trueNu = 1 / 0.025,
  nsim = 2,
  mcmcOptions = my_options,
  seed = 819,
  parallel = FALSE
)

# Produce a summary of the simulations.
summary(
  my_sim,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff
)
