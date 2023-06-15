# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(
    0.1, 0.5, 1.5, 3, 6,
    seq(from = 10, to = 80, by = 2)
  )
)

# Initialize a model
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Get posterior for all model parameters
options <- McmcOptions(
  burnin = 100,
  step = 2,
  samples = 2000
)
set.seed(94)
samples <- mcmc(data, model, options)

# Approximate the posterior distribution with a bivariate normal
# max.time and maxit are very small only for the purpose of showing the example. They
# should be increased for a real case.
set.seed(94)
approximation <- approximate(
  object = samples,
  model = model,
  data = data,
  logNormal = TRUE,
  control = list(
    threshold.stop = 0.1,
    max.time = 1,
    maxit = 1
  )
)

posterior <- approximation$model

# nolint end
