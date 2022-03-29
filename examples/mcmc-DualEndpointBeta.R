# Create some data from the class `DataDual`.
plcb <- 0.001
my_data <- DataDual(
  w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48),
  x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
  y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
  doseGrid = c(plcb, seq(25, 300, 25)),
  placebo = TRUE,
  ID = 1:12,
  cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
)

# Initialize the CRM model.
my_model <- DualEndpointBeta(
  mean = c(0, 1),
  cov = diag(2),
  ref_dose = 2,
  use_log_dose = FALSE,
  sigma2W = c(a = 1, b = 2),
  rho = c(a = 1.5, b = 2.5),
  E0 = 2,
  Emax = 50,
  delta1 = 6,
  mode = 9,
  ref_dose_beta = my_data@doseGrid[my_data@nGrid] + 10
)

# Sample from the posterior distribution.
my_options <- McmcOptions(
  burnin = 50,
  step = 2,
  samples = 4,
  rng_kind = "Mersenne-Twister",
  rng_seed = 1
)

samples <- mcmc(data = my_data, model = my_model, options = my_options)
samples
