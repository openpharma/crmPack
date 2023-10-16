# Assemble ingredients for our group design.
my_stopping <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5) |
  StoppingMinPatients(20) |
  StoppingMissingDose()
my_increments <- IncrementsDoseLevels(levels = 3L)
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.3),
  overdose = c(0.3, 1),
  max_overdose_prob = 0.3
)
my_cohort_size <- CohortSizeConst(3)
empty_data <- Data(doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)))
my_model <- LogisticLogNormalGrouped(
  mean = c(-4, -4, -4, -4),
  cov = diag(rep(6, 4)),
  ref_dose = 0.1
)

# Put together the design. Note that if we only specify the mono arm,
# then the combo arm is having the same settings.
my_design <- DesignGrouped(
  model = my_model,
  mono = Design(
    model = my_model,
    stopping = my_stopping,
    increments = my_increments,
    nextBest = my_next_best,
    cohort_size = my_cohort_size,
    data = empty_data,
    startingDose = 0.1
  ),
  first_cohort_mono_only = TRUE,
  same_dose = TRUE
)

# Set up a realistic simulation scenario.
my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
my_combo_truth <- function(x) plogis(-4 + 0.5 * log(x / 0.1))
matplot(
  x = empty_data@doseGrid,
  y = cbind(
    mono = my_truth(empty_data@doseGrid),
    combo = my_combo_truth(empty_data@doseGrid)
  ),
  type = "l",
  ylab = "true DLT prob",
  xlab = "dose"
)
legend("topright", c("mono", "combo"), lty = c(1, 2), col = c(1, 2))

# Start the simulations.
set.seed(123)
my_sims <- simulate(
  my_design,
  nsim = 4, # This should be at least 100 in actual applications.
  seed = 123,
  truth = my_truth,
  combo_truth = my_combo_truth
)

# Looking at the summary of the simulations:
mono_sims_sum <- summary(my_sims$mono, truth = my_truth)
combo_sims_sum <- summary(my_sims$combo, truth = my_combo_truth)

mono_sims_sum
combo_sims_sum

plot(mono_sims_sum)
plot(combo_sims_sum)

# Looking at specific simulated trials:
trial_index <- 1
plot(my_sims$mono@data[[trial_index]])
plot(my_sims$combo@data[[trial_index]])
