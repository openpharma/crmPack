# Extracted from test-helpers.R:93

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- h_get_data()
data@placebo <- FALSE
df <- h_plot_data_df(data)
result <- ggplot(df, aes(x = patient, y = dose)) +
    geom_point() +
    h_plot_data_cohort_lines(df$cohort, placebo = data@placebo)
expect_doppel(
    "h_plot_data_cohort_lines without placebo",
    result
  )
