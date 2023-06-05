## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# nolint start

## ----Data---------------------------------------------------------------------
library(crmPack)
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  ID = as.integer(1:8),
  cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6)),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    ),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = rep(0, 8),
  Tmax = 60
)

emptydata <- DataDA(
  doseGrid = c(
    0.1, 0.5, 1, 1.5, 3, 6,
    seq(from = 10, to = 80, by = 2)
  ),
  Tmax = 60
)

## ----Model--------------------------------------------------------------------
npiece_ <- 10
Tmax_ <- 60

lambda_prior <- function(k) {
  npiece_ / (Tmax_ * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
  c_par = 2
)

## ----Posterior----------------------------------------------------------------
options <- McmcOptions(
  burnin = 10,
  step = 2,
  samples = 1e2
)

set.seed(94)
samples <- mcmc(data, model, options)

## ----Diagnose-----------------------------------------------------------------
library(ggmcmc)
alpha0samples <- get(samples, "alpha0")

print(ggs_traceplot(alpha0samples))

print(ggs_autocorrelation(alpha0samples))

## ----Fit----------------------------------------------------------------------
plot(samples, model, data, hazard = TRUE)

plot(samples, model, data, hazard = FALSE)

## ----Prior--------------------------------------------------------------------
emptydata <- DataDA(doseGrid = c(
  0.1, 0.5, 1.5, 3, 6,
  seq(from = 10, to = 80, by = 2)
), Tmax = 60)

Priorsamples <- mcmc(emptydata, model, options)

plot(Priorsamples, model, emptydata, hazard = FALSE)

## ----Rules--------------------------------------------------------------------
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

nextMaxDose <- maxDose(myIncrements, data = data)

myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

mySize1 <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
mySize2 <- CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
mySize <- maxSize(mySize1, mySize2)

myStopping1 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping2 <- StoppingMinPatients(nPatients = 50)
myStopping <- (myStopping1 | myStopping2)

## ----Recommend----------------------------------------------------------------
doseRecommendation <- nextBest(myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)


doseRecommendation$plot
doseRecommendation$value

## ----Window-------------------------------------------------------------------
mysafetywindow <- SafetyWindowConst(c(6, 2), 7, 7)

design <- DADesign(
  model = model,
  increments = myIncrements,
  nextBest = myNextBest,
  stopping = myStopping,
  cohortSize = mySize,
  data = emptydata,
  safetyWindow = mysafetywindow,
  startingDose = 3
)

## ----Truth--------------------------------------------------------------------
myTruth <- probFunction(model, alpha0 = 2, alpha1 = 3)
curve(myTruth(x), from = 0, to = 100, ylim = c(0, 1))

onset <- 15
exp_cond.cdf <- function(x) {
  1 - (pexp(x, 1 / onset, lower.tail = FALSE) - pexp(28, 1 / onset, lower.tail = FALSE)) / pexp(28, 1 / onset)
}

## ----Simulate-----------------------------------------------------------------
mySims <- simulate(design,
  args = NULL,
  truthTox = myTruth,
  truthSurv = exp_cond.cdf, # piece_exp_cond.cdf,
  trueTmax = 80,
  nsim = 2,
  seed = 819,
  mcmcOptions = options,
  firstSeparate = TRUE,
  deescalate = FALSE,
  parallel = FALSE
)

## ----Interpret----------------------------------------------------------------
a <- summary(mySims, truth = myTruth)
b <- mySims@data[[1]]

plot(mySims)
plot(b)

mySims@stopReasons[[2]]

# nolint end

