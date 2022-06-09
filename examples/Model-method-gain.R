# Obtain the gain value for a given dose, a pseudo DLE and efficacy models
# as well as DLE and efficacy samples.
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)
mcmc_opts <- McmcOptions(burnin = 100, step = 2, samples = 200)

# DLE model and samples.
model_dle <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

samples_dle <- mcmc(emptydata, model_dle, mcmc_opts)

# Efficacy model (Effloglog) and samples.
model_effloglog <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

samples_effloglog <- mcmc(emptydata, model_effloglog, mcmc_opts)

# Gain values for dose level 75 and Effloglog efficacy model.
gain(
  dose = 75,
  model_dle = model_dle,
  samples_dle = samples_dle,
  model_eff = model_effloglog,
  samples_eff = samples_effloglog
)

# Efficacy model (EffFlexi) and samples.
model_effflexi <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = emptydata
)

samples_effflexi <- mcmc(emptydata, model_effflexi, mcmc_opts)

# Gain values for dose level 75 and EffFlexi efficacy model.
gain(
  dose = 75,
  model_dle = model_dle,
  samples_dle = samples_dle,
  model_eff = model_effflexi,
  samples_eff = samples_effflexi
)
