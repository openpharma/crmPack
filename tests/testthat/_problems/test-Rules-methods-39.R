# Extracted from test-Rules-methods.R:39

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
    list(alpha0 = c(-1, 0, 0.5, 1), alpha1 = c(0.1, 0.2, 0.3, 0.4))
  )
nb_ewoc <- NextBestEWOC(
    target = 0.30,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
doselimit <- 45
result <- nextBest(
    nb_ewoc,
    doselimit,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
eligible <- with(
    as.data.frame(result$probs),
    dose[overdose <= nb_ewoc@max_overdose_prob & dose <= doselimit]
  )
expected_next <- if (length(eligible) > 0) max(eligible) else NA_real_
expect_identical(result$value, expected_next)
expect_true(all(c("dose", "overdose") %in% colnames(result$probs)))
expect_doppel("Plot of nextBest-NextBestEWOC", result$plot)
