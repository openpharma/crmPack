# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Initialize the CRM model used to model the data.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(
  burnin = 100, step = 2, samples = 2000, rng_kind = "Mersenne-Twister", rng_seed = 94
)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  my_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Define the stopping rule such that the study would be stopped if the
# the MTD can be estimated with sufficient precision, i.e. if robust coefficient
# of variation is below 40%.
my_stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 40)

# Evaluate if to stop the trial.
stopTrial(
  stopping = my_stopping,
  dose = dose_recommendation$value,
  samples = my_samples,
  model = my_model,
  data = my_data
)
