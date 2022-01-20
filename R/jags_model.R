# nolint start

# LogisticNormal ----

jags_model_data_ln <- function() {
  # The logistic likelihood.
  for (i in 1:nObs) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
    StandLogDose[i] <- log(x[i] / refDose)
  }
}

jags_model_prior_ln <- function() {
  # The multivariate normal prior on the coefficients.
  theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2, 1:2])
  alpha0 <- theta[1]
  alpha1 <- theta[2]
  # Dummy to use `refDose` here. It is contained in the `modelspecs` list
  # so it must occur also here.
  # bla <- refDose + 1 # Presumably not needed anymore, WW.
}

# LogisticLogNormal ----

jags_model_prior_lln <- function() {
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

# nolint end
