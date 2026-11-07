# Extracted from test-Rules-methods1.R:466

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()

# test -------------------------------------------------------------------------
data <- h_get_data(placebo = FALSE)
model <- h_get_logistic_log_normal()
samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )
result <- recommend(
    nb_ncrm_loss,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
