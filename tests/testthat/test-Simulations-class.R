# GeneralSimulations-class ----
test_that("GeneralSimulations generator function works as expected", {
  result <- expect_silent(.GeneralSimulations())
  expect_valid(result, "GeneralSimulations")
})

test_that("GeneralSimulations object can be created with the user constructor", {
  data <- list(
    Data(
      x = 1:2,
      y = 0:1,
      doseGrid = 1:2,
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    Data(
      x = 3:4,
      y = 0:1,
      doseGrid = 3:4,
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )

  doses <- c(1, 2)

  seed <- as.integer(123)

  result <- expect_silent(
    GeneralSimulations(
      data,
      doses,
      seed
    )
  )

  expect_valid(result, "GeneralSimulations")
  expect_identical(result@data, data)
  expect_identical(result@doses, doses)
  expect_identical(result@seed, seed)
})

test_that("GeneralSimulations user constructor arguments names are as expected", {
  expect_function(
    GeneralSimulations,
    args = c("data", "doses", "seed"),
    ordered = TRUE
  )
})

# Simulations-class ----
test_that("Simulations generator function works as expected", {
  result <- expect_silent(.Simulations())
  expect_valid(result, "Simulations")
})

test_that("Simulations object can be created with the user constructor", {
  fit <- list(
    c(0.1, 0.2),
    c(0.3, 0.4)
  )
  stop_reasons <- list("A", "B")

  stop_report <- matrix(c(TRUE, FALSE), nrow = 2)

  additional_stats <- list(a = 1, b = 1)

  data <- list(
    Data(
      x = 1:2,
      y = 0:1,
      doseGrid = 1:2,
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    Data(
      x = 3:4,
      y = 0:1,
      doseGrid = 3:4,
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )

  doses <- c(1, 2)

  seed <- as.integer(123)

  result <- expect_silent(
    Simulations(
      fit = fit,
      stop_reasons = stop_reasons,
      stop_report = stop_report,
      additional_stats = additional_stats,
      data,
      doses,
      seed
    )
  )

  expect_valid(result, "Simulations")
  expect_identical(result@fit, fit)
  expect_identical(result@stop_reasons, stop_reasons)
})

test_that("Simulations user constructor arguments names are as expected", {
  expect_function(
    Simulations,
    args = c("fit", "stop_reasons", "stop_report", "additional_stats", "..."),
    ordered = TRUE
  )
})

# DualSimulations-class ----
test_that("DualSimulations generator function works as expected", {
  result <- expect_silent(.DualSimulations())
  expect_valid(result, "DualSimulations")
})

test_that("DualSimulations object can be created with the user constructor", {
  rho_est <- c(0.25, 0.35)
  sigma2w_est <- c(0.15, 0.25)
  fit_biomarker <- list(c(0.3, 0.4), c(0.4, 0.5))

  data_list <- list(
    Data(
      x = 1:2,
      y = 0:1,
      doseGrid = 1:2,
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    Data(
      x = 3:4,
      y = 0:1,
      doseGrid = 3:4,
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )

  doses <- c(1, 2)
  seed <- as.integer(123)

  fit <- list(
    c(0.1, 0.2),
    c(0.3, 0.4)
  )

  stop_report <- matrix(c(TRUE, FALSE), nrow = 2)

  stop_reasons <- list("A", "B")

  additional_stats <- list(a = 1, b = 1)

  result <- expect_silent(
    DualSimulations(
      rho_est = rho_est,
      sigma2w_est = sigma2w_est,
      fit_biomarker = fit_biomarker,
      fit = fit,
      stop_report = stop_report,
      stop_reasons = stop_reasons,
      additional_stats = additional_stats,
      data = data_list,
      doses = doses,
      seed = seed
    )
  )

  expect_valid(result, "DualSimulations")
  expect_identical(result@rho_est, rho_est)
  expect_identical(result@sigma2w_est, sigma2w_est)
  expect_identical(result@fit_biomarker, fit_biomarker)
})

test_that("DualSimulations user constructor arguments names are as expected", {
  expect_function(
    DualSimulations,
    args = c("rho_est", "sigma2w_est", "fit_biomarker", "..."),
    ordered = TRUE
  )
})

# GeneralSimulationsSummary ----
test_that("GeneralSimulationsSummary generates object correctly", {
  target_value <- 1
  target_dose_interval_value <- 2
  nsim_value <- 3L
  mean_tox_risk_value <- 4
  dose_selected_value <- 5

  result <- expect_silent(
    .GeneralSimulationsSummary(
      target = target_value,
      target_dose_interval = target_dose_interval_value,
      nsim = nsim_value,
      prop_dlts = list(),
      mean_tox_risk = mean_tox_risk_value,
      dose_selected = dose_selected_value,
      tox_at_doses_selected = 6,
      prop_at_target = 7,
      dose_most_selected = 8,
      obs_tox_rate_at_dose_most_selected = 9,
      n_obs = list(),
      n_above_target = 10L,
      dose_grid = 11,
      placebo = TRUE
    )
  )

  expect_valid(result, "GeneralSimulationsSummary")

  expect_identical(result@target, target_value)
  expect_identical(result@target_dose_interval, target_dose_interval_value)
  expect_identical(result@nsim, nsim_value)
  expect_identical(result@mean_tox_risk, mean_tox_risk_value)
  expect_identical(result@dose_selected, dose_selected_value)
})

test_that("GeneralSimulationsSummary cannot be instantiated directly", {
  expect_error(.DefaultGeneralSimulationsSummary(),
    "Class GeneralSimulationsSummary cannot be instantiated directly",
    fixed = FALSE
  )
})

# SimulationsSummary ----
test_that("SimulationsSummary generates object correctly", {

  stop_report <- matrix(c(TRUE, FALSE), nrow = 2)
  fit_at_dose_most_selected <- 123
  additional_stats <- list(a = 1, b = 1)
  mean_fit <- list(c(0.3, 0.4), c(0.4, 0.5))

  result <- expect_silent(
    .SimulationsSummary(
      stop_report = stop_report,
      fit_at_dose_most_selected = fit_at_dose_most_selected,
      additional_stats = additional_stats,
      mean_fit = mean_fit
    )
  )

  expect_valid(result, "SimulationsSummary")

  expect_identical(result@stop_report, stop_report)
  expect_identical(result@fit_at_dose_most_selected, fit_at_dose_most_selected)
  expect_identical(result@additional_stats, additional_stats)
  expect_identical(result@mean_fit, mean_fit)
})

test_that(".DefaultSimulationsSummary cannot be instantiated directly", {
  expect_error(.DefaultSimulationsSummary(),
    "Class SimulationsSummary cannot be instantiated directly",
    fixed = FALSE
  )
})
