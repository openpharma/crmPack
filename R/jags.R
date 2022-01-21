# nolint start

# These are JAGS models specification for different classes model classes.

# LogisticNormal ----

# jm_data_* data model that uses logistic normal 
jm_data_logistic <- function() {
  # The logistic likelihood.
  for (i in 1:nObs) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
    StandLogDose[i] <- log(x[i] / refDose)
  }
}

jm_prior_logistic_normal <- function() {
  # The multivariate normal prior on the coefficients.
  theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2, 1:2])
  alpha0 <- theta[1]
  alpha1 <- theta[2]
  # Dummy to use `refDose` here. It is contained in the `modelspecs` list
  # so it must occur also here.
  # bla <- refDose + 1 # Presumably not needed anymore, WW.
}

jd_specs_logistic_normal <- function(refDose, prec, mean) {
  function() {
    list(refDose = refDose, priorPrec = prec, priorMean = mean)
  }
}

ji_logistic <-function() {
  list(theta = c(0, 1))
}

j_sample_two <- c("alpha0", "alpha1")

# LogisticLogNormal ----

jm_prior_logistic_log_normal <- function() {
  # The multivariate normal prior on the (transformed) coefficients.
  priorPrec[1:2, 1:2] <- inverse(priorCov[, ])
  theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2, 1:2])
  # Extract actual coefficients.
  alpha0 <- theta[1]
  alpha1 <- exp(theta[2])

  # Dummy to use `refDose` here. It is contained in the `modelspecs` list
  # so it must occur also here.
  # bla <- refDose + 1 # Presumably not needed anymore, WW.
}

jd_specs_logistic_log_normal <- function(refDose, cov, mean) {
  function() {
    list(refDose = refDose, priorCov = cov, priorMean = mean)
  }
}

# nolint end
