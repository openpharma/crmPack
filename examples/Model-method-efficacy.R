# Obtain the expected efficacy value for a given dose, a given pseudo efficacy
# model (in flexible form for prior) and efficacy samples.

# Empty data (i.e. no observed data), dose grid only.
my_data <- DataDual(doseGrid = seq(25, 300, 25))

my_model <- EffFlexi(
  Eff = c(1.223, 2.513),
  Effdose = c(25, 300),
  sigma2 = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  smooth = "RW2",
  data = my_data
)

my_options <- McmcOptions(
  burnin = 100, step = 2, samples = 200, rng_kind = "Mersenne-Twister", rng_seed = 94
)

my_samples <- mcmc(data = my_data, model = my_model, options = my_options)

# Efficacy for dose 75.
efficacy(dose = 75, model = my_model, samples = my_samples)

# Obtain the expected efficacy value for a given dose, a given pseudo efficacy
# model (linear log-log efficacy) and no samples.
my_model_ll <- Effloglog(
  Eff = c(1.223, 2.513),
  Effdose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data,
  c = 0
)

efficacy(dose = 75, model = my_model_ll)
