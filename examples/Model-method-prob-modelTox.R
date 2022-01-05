
# Create data from the 'DataDual' class.
my_data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(from = 25, to = 300, by = 25)
)

# Initialize a model from 'ModelTox' class e.g using 'LogisticIndepBeta' model.
dlt_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# Get samples from posterior.
set.seed(94)
my_options <- McmcOptions(burnin = 100, step = 2, samples = 200)
dlt_sample <- mcmc(data = my_data, model = dlt_model, options = my_options)

# Posterior for Prob(DLT | dose = 100).
prob(dose = 100, model = dlt_model, samples = dlt_sample)
