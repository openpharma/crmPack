#####################################################################################
## Author: Daniel Sabanés Bové [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[dual.R] by DSB Son 11/01/2015 14:55>
##
## Description:
## Test the dual endpoint stuff. For development only!!
##
## History:
## 24/03/2014   file creation
## 22/12/2014   test the new JAGS implementation
## 08/05/2018   version to test package directly on convergence/extrapolation
###################################################################################

library(crmPack)

## set up the model
model <- DualEndpointRW(
  mu = c(0, 1),
  Sigma = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  ## c(a=20, b=50), ## gives very unstable results!!
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  ## c(a=20, b=10)
  smooth = "RW1"
)

data <- DataDual(
  c(
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
  y = as.integer(c(
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
  )),
  w = c(
    0.3,
    0.4,
    0.5,
    0.4,
    0.6,
    0.7,
    0.5,
    0.6,
    0.5,
    0.5,
    0.55,
    0.4,
    0.41,
    0.39,
    0.3,
    0.3,
    0.2
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
data
data@nGrid
data@nObs

options <- McmcOptions(
  burnin = 10000,
  step = 2,
  samples = 50000
)


samples <- mcmc(data, model, options, verbose = TRUE)

str(samples)

plot(samples@data$betaW[, 40], type = "l")
plot(samples@data$betaW[, 30], type = "l")
## convergence is obtained both for RW1 and RW2 - good

plot(samples, model, data)
plot(samples, model, data, extrapolate = FALSE)
