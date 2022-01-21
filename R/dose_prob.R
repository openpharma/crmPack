# Logistic ----

d_logistic <- function(refDose) { # nolintr
  function(prob, alpha0, alpha1) {
    stand_log_dose <- (logit(prob) - alpha0) / alpha1
    exp(stand_log_dose) * refDose
  }
}

p_logistic <- function(refDose) { # nolintr
  function(dose, alpha0, alpha1) {
    stand_log_dose <- log(dose / refDose)
    plogis(alpha0 + alpha1 * stand_log_dose)
  }
}
