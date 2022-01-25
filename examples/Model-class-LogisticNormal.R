# Define the dose-grid.
empty_data <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

my_model <- LogisticNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  refDose = 50
)

my_options <- McmcOptions(burnin = 100, step = 2, samples = 1000)

samples <- mcmc(empty_data, my_model, my_options)
samples
