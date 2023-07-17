# Obtain the plot for the simulation results if only DLE responses are
# considered in the simulations.

# Specified simulations when no DLE samples are used.
emptydata <- Data(doseGrid = seq(25, 300, 25))

# The design only incorporate DLE responses and DLE samples are involved.
# Specify the model of 'ModelTox' class eg 'LogisticIndepBeta' class model.
my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

# The escalation rule.
td_next_best <- NextBestTD(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

# The cohort size is 3 subjects.
my_size <- CohortSizeConst(size = 3)

# Allow increase of 200%.
my_increments <- IncrementsRelative(intervals = 0, increments = 2)

# Specify the stopping rule with maximum sample size of 36 patients or when the
# next dose is NA.
my_stopping <- StoppingMinPatients(nPatients = 36) | StoppingMissingDose()

# Specify the design. (For details please refer to the 'TDDesign' example.)
my_design <- TDDesign(
  model = my_model,
  nextBest = td_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# Specify the truth of the DLE responses.
my_truth <- probFunction(my_model, phi1 = -53.66584, phi2 = 10.50499)

# For illustration purpose only 1 simulation is produced.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  truth = my_truth,
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

# Summary of the simulations.
my_sum <- summary(
  my_sim,
  truth = my_truth
)

# Show the summary of the simulated results in a data frame.
show(my_sum)

# Example where DLE samples are involved.

# The escalation rule.
td_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

# The design.
my_design <- TDsamplesDesign(
  model = my_model,
  nextBest = td_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# For illustration purposes 2 trails are simulated with 50 burn-ins to generate
# 200 samples.
my_options <- McmcOptions(burnin = 50, step = 2, samples = 200)

my_sim <- simulate(
  object = my_design,
  args = NULL,
  truth = my_truth,
  nsim = 2,
  seed = 819,
  mcmcOptions = my_options,
  parallel = FALSE
)

# Produce a summary of the simulations.
my_sum <- summary(
  my_sim,
  truth = my_truth
)

# Show the summary of the simulated results in a data frame.
show(my_sum)
