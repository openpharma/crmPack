# nolint start

# Mock slow simulation summary constructors with pre-computed fixtures
testthat::local_mocked_bindings(
  .DefaultDualSimulationsSummary = function(...) {
    readRDS(testthat::test_path("fixtures", "default_dual_simulations_summary.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoSimulationsSummary = function(...) {
    readRDS(testthat::test_path("fixtures", "default_pseudo_simulations_summary.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoDualSimulationsSummary = function(...) {
    readRDS(testthat::test_path("fixtures", "default_pseudo_dual_simulations_summary.Rds"))
  }
)

# nolint end

# GeneralSimulations ----

test_that("knit_print.GeneralSimulations works correctly", {
  expect_snap(knit_print(.DefaultGeneralSimulations(), asis = FALSE))
})

test_that("knit_print.GeneralSimulations handles asis parameter", {
  x <- .DefaultGeneralSimulations()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# Simulations ----

test_that("knit_print.Simulations works correctly", {
  x <- .DefaultSimulations()

  result <- knit_print(x, asis = FALSE)

  expect_true(grepl("### Simulation Results", result, fixed = TRUE))
  expect_true(grepl("Stopping reasons:", result, fixed = TRUE))
  expect_true(grepl("%", result, fixed = TRUE))
})

test_that("knit_print.Simulations handles asis parameter", {
  x <- .DefaultSimulations()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# DualSimulations ----

test_that("knit_print.DualSimulations works correctly", {
  expect_snap(knit_print(.DefaultDualSimulations(), asis = FALSE))
})

test_that("knit_print.DualSimulations handles asis parameter", {
  x <- .DefaultDualSimulations()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# PseudoSimulations ----

test_that("knit_print.PseudoSimulations works correctly", {
  # nolint start
  x <- PseudoSimulations(
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end
  expect_snap(knit_print(x, asis = FALSE))
})

test_that("knit_print.PseudoSimulations handles asis parameter", {
  # nolint start
  x <- PseudoSimulations(
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# PseudoDualSimulations ----

test_that("knit_print.PseudoDualSimulations works correctly", {
  # nolint start
  x <- PseudoDualSimulations(
    fit_eff = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_gstar_estimates = c(100, 110),
    final_gstar_at_dose_grid = c(100, 100),
    final_gstar_cis = list(c(80, 120), c(85, 135)),
    final_gstar_ratios = c(1.5, 1.59),
    final_optimal_dose = c(100, 110),
    final_optimal_dose_at_dose_grid = c(100, 100),
    sigma2_est = c(0.1, 0.12),
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end
  expect_snap(knit_print(x, asis = FALSE))
})

test_that("knit_print.PseudoDualSimulations handles asis parameter", {
  # nolint start
  x <- PseudoDualSimulations(
    fit_eff = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_gstar_estimates = c(100, 110),
    final_gstar_at_dose_grid = c(100, 100),
    final_gstar_cis = list(c(80, 120), c(85, 135)),
    final_gstar_ratios = c(1.5, 1.59),
    final_optimal_dose = c(100, 110),
    final_optimal_dose_at_dose_grid = c(100, 100),
    sigma2_est = c(0.1, 0.12),
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# PseudoDualFlexiSimulations ----

test_that("knit_print.PseudoDualFlexiSimulations works correctly", {
  skip_on_cran_but_not_ci()

  # nolint start
  x <- PseudoDualFlexiSimulations(
    sigma2_beta_w_est = c(0.01, 0.015),
    fit_eff = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_gstar_estimates = c(100, 110),
    final_gstar_at_dose_grid = c(100, 100),
    final_gstar_cis = list(c(80, 120), c(85, 135)),
    final_gstar_ratios = c(1.5, 1.59),
    final_optimal_dose = c(100, 110),
    final_optimal_dose_at_dose_grid = c(100, 100),
    sigma2_est = c(0.1, 0.12),
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end
  expect_snap(knit_print(x, asis = FALSE))
})

test_that("knit_print.PseudoDualFlexiSimulations handles asis parameter", {
  skip_on_cran_but_not_ci()

  # nolint start
  x <- PseudoDualFlexiSimulations(
    sigma2_beta_w_est = c(0.01, 0.015),
    fit_eff = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_gstar_estimates = c(100, 110),
    final_gstar_at_dose_grid = c(100, 100),
    final_gstar_cis = list(c(80, 120), c(85, 135)),
    final_gstar_ratios = c(1.5, 1.59),
    final_optimal_dose = c(100, 110),
    final_optimal_dose_at_dose_grid = c(100, 100),
    sigma2_est = c(0.1, 0.12),
    fit = list(c(0.1, 0.2), c(0.15, 0.25)),
    final_td_target_during_trial_estimates = c(50, 75),
    final_td_target_end_of_trial_estimates = c(60, 80),
    final_td_target_during_trial_at_dose_grid = c(50, 75),
    final_td_target_end_of_trial_at_dose_grid = c(50, 75),
    final_tdeot_cis = list(c(0.2, 0.4), c(0.3, 0.5)),
    final_tdeot_ratios = c(2, 1.67),
    final_cis = list(c(0.1, 0.3), c(0.15, 0.35)),
    final_ratios = c(3, 2.33),
    stop_report = matrix(TRUE, nrow = 2, ncol = 1),
    stop_reasons = list("A", "B"),
    data = list(
      Data(x = 1:2, y = 0:1, doseGrid = 1:2, ID = 1L:2L, cohort = 1L:2L),
      Data(x = 3:4, y = 0:1, doseGrid = 3:4, ID = 1L:2L, cohort = 1L:2L)
    ),
    doses = c(1, 2),
    seed = 123L
  )
  # nolint end

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# DASimulations ----

test_that("knit_print.DASimulations works correctly", {
  skip_on_cran_but_not_ci()

  design <- .DefaultDADesign()
  my_truth <- probFunction(design@model, alpha0 = 2, alpha1 = 3)
  exp_cond_cdf <- function(x, onset = 15) {
    a <- stats::pexp(28, 1 / onset, lower.tail = FALSE)
    1 - (stats::pexp(x, 1 / onset, lower.tail = FALSE) - a) / (1 - a)
  }

  x <- simulate(
    design,
    args = NULL,
    truthTox = my_truth,
    truthSurv = exp_cond_cdf,
    trueTmax = 80,
    nsim = 2,
    seed = 819,
    mcmcOptions = .DefaultMcmcOptions(),
    firstSeparate = TRUE,
    deescalate = FALSE,
    parallel = FALSE
  )

  result <- knit_print(x, asis = FALSE)

  expect_true(grepl("Trial duration:", result, fixed = TRUE))
  expect_true(grepl("Mean =", result, fixed = TRUE))
  expect_true(grepl("Range =", result, fixed = TRUE))
})

test_that("knit_print.DASimulations handles asis parameter", {
  skip_on_cran_but_not_ci()

  x <- .DefaultDASimulations()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# GeneralSimulationsSummary ----

test_that("knit_print.GeneralSimulationsSummary works correctly", {
  skip_on_cran_but_not_ci()

  x <- .DefaultSimulations()
  my_truth <- function(dose) plogis(-4 + 0.5 * log(dose))
  xsum <- summary(x, truth = my_truth)

  result <- knit_print(xsum, asis = FALSE)

  expect_true(grepl("### Simulation Summary", result, fixed = TRUE))
  expect_true(grepl("Target toxicity interval:", result, fixed = TRUE))
  expect_true(grepl("Target dose interval:", result, fixed = TRUE))
  expect_true(grepl("Dose most often selected as MTD:", result, fixed = TRUE))
  expect_true(grepl("Proportion selecting target MTD:", result, fixed = TRUE))
})

test_that("knit_print.GeneralSimulationsSummary handles asis parameter", {
  skip_on_cran_but_not_ci()

  x <- .DefaultSimulations()
  my_truth <- function(dose) plogis(-4 + 0.5 * log(dose))
  xsum <- summary(x, truth = my_truth)

  result_asis <- knit_print(xsum, asis = TRUE)
  result_no_asis <- knit_print(xsum, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# SimulationsSummary ----

test_that("knit_print.SimulationsSummary works correctly", {
  skip_on_cran_but_not_ci()

  x <- .DefaultSimulations()
  my_truth <- function(dose) plogis(-4 + 0.5 * log(dose))
  xsum <- summary(x, truth = my_truth)

  result <- knit_print(xsum, asis = FALSE)

  expect_true(grepl("### Simulation Summary", result, fixed = TRUE))
  expect_true(grepl("Fitted toxicity at dose most selected:", result, fixed = TRUE))
})

test_that("knit_print.SimulationsSummary handles asis parameter", {
  skip_on_cran_but_not_ci()

  x <- .DefaultSimulations()
  my_truth <- function(dose) plogis(-4 + 0.5 * log(dose))
  xsum <- summary(x, truth = my_truth)

  result_asis <- knit_print(xsum, asis = TRUE)
  result_no_asis <- knit_print(xsum, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# DualSimulationsSummary ----

test_that("knit_print.DualSimulationsSummary works correctly", {
  expect_snap(knit_print(.DefaultDualSimulationsSummary(), asis = FALSE))
})

test_that("knit_print.DualSimulationsSummary handles asis parameter", {
  x <- .DefaultDualSimulationsSummary()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# PseudoSimulationsSummary ----

test_that("knit_print.PseudoSimulationsSummary works correctly", {
  expect_snap(knit_print(.DefaultPseudoSimulationsSummary(), asis = FALSE))
})

test_that("knit_print.PseudoSimulationsSummary handles asis parameter", {
  x <- .DefaultPseudoSimulationsSummary()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})

# PseudoDualSimulationsSummary ----

test_that("knit_print.PseudoDualSimulationsSummary works correctly", {
  expect_snap(knit_print(.DefaultPseudoDualSimulationsSummary(), asis = FALSE))
})

test_that("knit_print.PseudoDualSimulationsSummary handles asis parameter", {
  x <- .DefaultPseudoDualSimulationsSummary()

  result_asis <- knit_print(x, asis = TRUE)
  result_no_asis <- knit_print(x, asis = FALSE)

  expect_s3_class(result_asis, "knit_asis")
  expect_type(result_no_asis, "character")
})
