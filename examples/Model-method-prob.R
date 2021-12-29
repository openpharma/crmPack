# Create some data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Initialize a model, e.g. 'LogisticLogNormal'.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  refDose = 56
)

# Get samples from posterior.
set.seed(94)
my_options <- McmcOptions(burnin = 100, step = 2, samples = 2000)
my_samples <- mcmc(data = my_data, model = my_model, options = my_options)

# Posterior for Prob(DLT | dose = 50).
tox_prob <- prob(dose = 50, model = my_model, samples = my_samples)
