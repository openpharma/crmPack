
# Obtain the gain value for a given dose, a pseudo DLE and efficacy models
# without DLE and efficacy samples.
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)
data <- Data(doseGrid = seq(25, 300, 25), placebo = FALSE)
mcmc_opts <- McmcOptions(burnin = 100, step = 2, samples = 200)

# DLE model and samples.
model_dle <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

# Efficacy model and samples.
model_eff <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# Gain value for dose level 75.
gain(
  dose = 75,
  model_dle = model_dle,
  model_eff = model_eff
)
