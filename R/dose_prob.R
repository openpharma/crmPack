# nolint start

# Logistic ----

dose_logistic <- function(prob, alpha0, alpha1) {
  StandLogDose <- (logit(prob) - alpha0) / alpha1
  exp(StandLogDose) * refDose
}

prob_logistic <- function(dose, alpha0, alpha1) {
  StandLogDose <- log(dose / refDose)
  plogis(alpha0 + alpha1 * StandLogDose)
}

# nolint end
