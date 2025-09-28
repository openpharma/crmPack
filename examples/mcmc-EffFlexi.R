## obtain mcmc efficacy samples given the data, 'EffFlexi' model (efficacy model) and
## mcmc simulations options
## data must be of 'DataDual' class
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25)
)
## model must be of 'EffFlexi' class

effmodel <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)

## options must be ''McmcOptions' class
options <- McmcOptions(burnin = 100, step = 2, samples = 200)
set.seed(94)
samples <- mcmc(data = data, model = effmodel, options = options)
