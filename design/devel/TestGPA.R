data <- DataDual(
  x = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    10,
    10,
    10,
    20,
    20,
    20,
    40,
    40,
    40,
    50,
    50,
    50
  ),
  y = c(
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    1,
    0,
    0,
    1,
    0,
    1,
    1
  ),
  w = c(
    0.31,
    0.42,
    0.59,
    0.45,
    0.6,
    0.7,
    0.55,
    0.6,
    0.52,
    0.54,
    0.56,
    0.43,
    0.41,
    0.39,
    0.34,
    0.38,
    0.21
  ),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)
plot(data)

model <- DualEndpointEmax(
  mu = c(0, 1),
  Sigma = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  ED50 = c(0, 200),
  refDose = 500
)

options <- McmcOptions(
  burnin = 20000,
  step = 3,
  samples = 10000
)
# be sure that mcmc class was loaded (otherwise conflict with coda package)
samples <- mcmc(data, model, options)

print(plot(samples, model, data, extrapolate = FALSE))
print(plot(samples, model, data, extrapolate = TRUE))

nextBest1 <- NextBestDualEndpoint(
  target = c(0.9, 1.0),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

nextBest(nextBest = nextBest1, doselimit = 1000, samples, model, data)

model <- DualEndpointRW(
  mu = c(0, 1),
  Sigma = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  smooth = "RW1"
)
options <- McmcOptions(
  burnin = 20000,
  step = 3,
  samples = 10000
)
# be sure that mcmc class was loaded (otherwise conflict with coda package)
samples <- mcmc(data, model, options)

nextBest1 <- NextBestDualEndpoint(
  target = c(0.9, 1.0),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

nextBest(nextBest = nextBest1, doselimit = 1000, samples, model, data)


# ---------------------------------
# Lets familiarize with the RW1
# ---------------------------------
data <- DataDual(
  x = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    10,
    10,
    10,
    20,
    20,
    20,
    40,
    40,
    40,
    50,
    50,
    50
  ),
  y = c(
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    1,
    0,
    0,
    1,
    0,
    1,
    1
  ),
  w = c(
    0.31,
    0.42,
    0.59,
    0.45,
    0.6,
    0.7,
    0.55,
    0.6,
    0.52,
    0.54,
    0.56,
    0.43,
    0.41,
    0.39,
    0.34,
    0.38,
    0.21
  ),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  )
)

model <- DualEndpointRW(
  mu = c(0, 1),
  Sigma = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = c(a = 20, b = 50),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = 0.0,
  smooth = "RW1"
)

options <- McmcOptions(
  burnin = 20000,
  step = 3,
  samples = 10000
)
emptydata <- DataDual(doseGrid = data@doseGrid)
priorsamples <- mcmc(emptydata, model, options)
