# Example of usage for `NextBestMinDist` NextBest class.

# Create the data.
my_data <- Data(
  x = c(0.01, 1.5, 1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5),
  y = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
  ID = 1:10,
  cohort = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3),
  doseGrid = c(0.01, 1.5, 2.5, 3.5, 4.5, 6, 7),
  placebo = TRUE
)

# Initialize the CRM model used to model the data.
my_model <- LogisticKadaneBetaGamma(
  theta = 0.3,
  xmin = 1.5,
  xmax = 7,
  alpha = 1,
  beta = 19,
  shape = 0.5625,
  rate = 0.125
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsNumDoseLevels(max_levels = 1)

next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose
# based on the 'NextBestMinDist' class.
next_best_min_dist <- NextBestMinDist(target = 0.3)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = next_best_min_dist,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)
