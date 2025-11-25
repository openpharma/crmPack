testthat::local_mocked_bindings(
  .DefaultSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultDualSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_dual_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_pseudo_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoDualSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_simulations.Rds"
    ))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoDualFlexiSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_flexi_simulations.Rds"
    ))
  }
)

# plot ----

## plot-GeneralSimulations ----

test_that("plot-GeneralSimulations works correctly", {
  mySims <- .DefaultSimulations()

  # Test default plot (includes both trajectory and dosesTried, so returns gtable)
  result <- plot(mySims)
  expect_s3_class(result, "gtable")
  expect_equal(dim(result), c(2, 1)) # Should have 2 rows, 1 column
  expect_true(all(c("heights", "widths", "grobs") %in% names(result)))

  # Test trajectory plot only
  result_trajectory <- plot(mySims, type = "trajectory")
  expect_s3_class(result_trajectory, "ggplot")
  expect_equal(result_trajectory$labels$x, "Patient")
  expect_equal(result_trajectory$labels$y, "Dose Level")
  expect_doppel("plot_generalSims_trajectory", result_trajectory)

  # Test dosesTried plot only
  result_doses <- plot(mySims, type = "dosesTried")
  expect_s3_class(result_doses, "ggplot")
  expect_equal(result_doses$labels$x, "Dose level")
  expect_equal(result_doses$labels$y, "Average proportion [%]")
  expect_doppel("plot_generalSims_dosesTried", result_doses)

  # Test both plot types explicitly
  result_both <- plot(mySims, type = c("trajectory", "dosesTried"))
  expect_s3_class(result_both, "gtable")
  expect_equal(dim(result_both), c(2, 1)) # Should have 2 rows, 1 column
})

test_that("plot-GeneralSimulations fails gracefully with bad input", {
  mySims <- .DefaultSimulations()

  expect_error(plot(mySims, type = "invalid_type"), "should be one of")
  # The function doesn't error with mixed valid/invalid types, it just filters out invalid ones
  result_mixed <- plot(mySims, type = c("trajectory", "invalid"))
  expect_s3_class(result_mixed, "ggplot")
  expect_error(plot(mySims, type = character(0)), "must be of length")
})

## plot-DualSimulations ----

test_that("plot-DualSimulations works correctly", {
  mySims <- .DefaultDualSimulations()

  # Test plots specific to DualSimulations
  result <- plot(mySims, type = c("trajectory", "sigma2W"))
  expect_s3_class(result, "gtable")
  expect_equal(dim(result), c(2, 1)) # Should have 2 rows, 1 column

  result_rho <- plot(mySims, type = "rho")
  expect_s3_class(result_rho, "ggplot")
  expect_doppel("plot_dualSims_rho", result_rho)
  expect_equal(result_rho$labels$x, "")
  expect_equal(result_rho$labels$y, "Correlation estimates")

  # Test sigma2W plot specifically
  result_sigma2w <- plot(mySims, type = "sigma2W")
  expect_s3_class(result_sigma2w, "ggplot")
  expect_doppel("plot_dualSims_sigma2w", result_sigma2w)
  expect_equal(result_sigma2w$labels$y, "Biomarker variance estimates")
})

# summary ----

## summary-GeneralSimulations ----

test_that("summary-GeneralSimulations works correctly", {
  mySims <- .DefaultSimulations()
  myTruth <- function(x) plogis(x)

  # Test summary with truth function
  result <- summary(mySims, truth = myTruth)
  expect_s4_class(result, "GeneralSimulationsSummary")

  # Check specific slot values for consistency
  expect_equal(result@target, c(0.2, 0.35))
  expect_equal(result@nsim, length(mySims@data))
  expect_true(is.numeric(result@dose_selected))
  expect_true(is.numeric(result@n_obs))
  expect_true(all(result@prop_dlts >= 0 & result@prop_dlts <= 1))
  expect_true(all(result@mean_tox_risk >= 0 & result@mean_tox_risk <= 1))
  expect_true(result@prop_at_target >= 0 & result@prop_at_target <= 1)
  expect_equal(length(result@dose_selected), result@nsim)

  # Test with custom target interval
  result_custom <- summary(mySims, truth = myTruth, target = c(0.15, 0.25))
  expect_s4_class(result_custom, "GeneralSimulationsSummary")
  expect_equal(result_custom@target, c(0.15, 0.25))
  expect_equal(result_custom@nsim, result@nsim) # Same number of simulations

  # Results may differ due to different target interval (but not guaranteed with all truth functions)
  expect_true(is.numeric(result_custom@prop_at_target))
})

test_that("summary-GeneralSimulations fails gracefully with bad input", {
  mySims <- .DefaultSimulations()
  myTruth <- function(x) plogis(x)

  # Test that missing truth function fails.
  expect_error(summary(mySims), "\"truth\" is missing")

  # Test ranges of target interval errors.
  expect_error(
    summary(mySims, truth = myTruth, target = c(0.5, 0.3)),
    "target[1] < target[2]",
    fixed = TRUE
  )
  expect_error(
    summary(mySims, truth = myTruth, target = c(-0.1, 0.3)),
    "not >= 0"
  )
  expect_error(
    summary(mySims, truth = myTruth, target = c(0.2, 1.1)),
    "not <= 1"
  )
})

# Add regression tests for specific expected values
test_that("summary-GeneralSimulations produces expected values", {
  mySims <- .DefaultSimulations()
  myTruth <- function(x) plogis(x - 3) # Simple truth function

  result <- summary(mySims, truth = myTruth, target = c(0.2, 0.35))

  # These values should remain stable across refactoring
  expect_equal(result@nsim, length(mySims@data))
  expect_true(result@dose_most_selected > 0) # Should select some dose
  expect_true(result@prop_at_target >= 0 && result@prop_at_target <= 1)

  # Check consistency of slot lengths
  expect_equal(length(result@dose_selected), result@nsim)
  expect_equal(length(result@tox_at_doses_selected), result@nsim)
  expect_equal(length(result@n_above_target), result@nsim)
})

## summary-Simulations ----

test_that("summary-Simulations works correctly", {
  mySims <- .DefaultSimulations()
  myTruth <- plogis

  result <- summary(mySims, truth = myTruth)
  expect_s4_class(result, "SimulationsSummary")
  expect_snapshot(result)

  # Check that it has additional slots compared to GeneralSimulationsSummary
  expect_true("fit_at_dose_most_selected" %in% slotNames(result))
  expect_true("mean_fit" %in% slotNames(result))
  expect_true("stop_report" %in% slotNames(result))
  expect_true("additional_stats" %in% slotNames(result))

  # Check specific slot values
  expect_true(is.numeric(result@fit_at_dose_most_selected))
  expect_equal(length(result@fit_at_dose_most_selected), result@nsim)
  expect_true(all(
    result@fit_at_dose_most_selected >= 0 &
      result@fit_at_dose_most_selected <= 1
  ))

  # Check mean_fit structure
  expect_true(is.list(result@mean_fit))
  expect_true("truth" %in% names(result@mean_fit))
  expect_true("average" %in% names(result@mean_fit))
  expect_true("lower" %in% names(result@mean_fit))
  expect_true("upper" %in% names(result@mean_fit))
  expect_equal(length(result@mean_fit$truth), length(result@dose_grid))
})

## summary-DualSimulations ----

test_that("summary-DualSimulations works correctly", {
  mySims <- .DefaultDualSimulations()
  myTruthTox <- function(dose) pnorm((dose - 60) / 10)
  myTruthBio <- function(dose) {
    pmax(0.1, pmin(0.95, 0.2 + 0.6 * (dose / 100)^0.5))
  }

  result <- summary(mySims, trueTox = myTruthTox, trueBiomarker = myTruthBio)
  expect_s4_class(result, "DualSimulationsSummary")
  expect_snapshot(result)

  # Check dual-specific slots
  expect_true("biomarker_fit_at_dose_most_selected" %in% slotNames(result))
  expect_true("mean_biomarker_fit" %in% slotNames(result))

  # Check biomarker fit values
  expect_true(is.numeric(result@biomarker_fit_at_dose_most_selected))
  expect_equal(length(result@biomarker_fit_at_dose_most_selected), result@nsim)

  # Check mean_biomarker_fit structure
  expect_true(is.list(result@mean_biomarker_fit))
  expect_true("truth" %in% names(result@mean_biomarker_fit))
  expect_true("average" %in% names(result@mean_biomarker_fit))
  expect_true("lower" %in% names(result@mean_biomarker_fit))
  expect_true("upper" %in% names(result@mean_biomarker_fit))
  expect_equal(
    length(result@mean_biomarker_fit$truth),
    length(result@dose_grid)
  )

  # Check that it inherits from SimulationsSummary
  expect_true("fit_at_dose_most_selected" %in% slotNames(result))
  expect_true("mean_fit" %in% slotNames(result))
})

if (FALSE) {
  ## summary-PseudoSimulations ----

  test_that("summary-PseudoSimulations works correctly", {
    # Create pseudo simulation
    emptydata <- Data(doseGrid = seq(25, 300, 25))

    model <- LogisticIndepBeta(
      binDLE = c(1.05, 1.8),
      DLEweights = c(3, 3),
      DLEdose = c(25, 300),
      data = emptydata
    )

    design <- TDDesign(
      model = model,
      nextBest = NextBestTD(prob_target_drt = 0.35, prob_target_eot = 0.3),
      stopping = StoppingMinPatients(nPatients = 6),
      increments = IncrementsRelative(intervals = 0, increments = 2),
      cohort_size = CohortSizeConst(size = 3),
      data = emptydata,
      startingDose = 25
    )

    myTruth <- probFunction(model, phi1 = -53.66584, phi2 = 10.50499)
    options <- McmcOptions(burnin = 10, step = 2, samples = 20)

    mySims <- simulate(
      design,
      args = NULL,
      truth = myTruth,
      nsim = 2,
      seed = 819,
      parallel = FALSE,
      mcmcOptions = options
    )

    result <- summary(mySims, truth = myTruth)
    expect_s4_class(result, "PseudoSimulationsSummary")
  })

  # show ----

  ## show-GeneralSimulationsSummary ----

  test_that("show-GeneralSimulationsSummary works correctly", {
    # Create simulation and summary
    emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))
    model <- LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    )

    design <- Design(
      model = model,
      nextBest = NextBestNCRM(
        target = c(0.2, 0.35),
        overdose = c(0.35, 1),
        max_overdose_prob = 0.25
      ),
      stopping = StoppingMinPatients(nPatients = 6),
      increments = IncrementsRelative(
        intervals = c(0, 20),
        increments = c(1, 0.33)
      ),
      cohort_size = CohortSizeConst(size = 3),
      data = emptydata,
      startingDose = 3
    )

    myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)
    options <- McmcOptions(burnin = 10, step = 2, samples = 20)

    mySims <- simulate(
      design,
      args = NULL,
      truth = myTruth,
      nsim = 2,
      seed = 819,
      mcmcOptions = options,
      parallel = FALSE
    )
    simSummary <- summary(mySims, truth = myTruth)

    # Test that show method works (produces output)
    expect_output(show(simSummary))

    # Show methods should print something
    result <- capture.output(show(simSummary))
    expect_true(length(result) > 0)
  })

  ## show-SimulationsSummary ----

  test_that("show-SimulationsSummary works correctly", {
    emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))
    model <- LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    )

    design <- Design(
      model = model,
      nextBest = NextBestNCRM(
        target = c(0.2, 0.35),
        overdose = c(0.35, 1),
        max_overdose_prob = 0.25
      ),
      stopping = StoppingMinPatients(nPatients = 6),
      increments = IncrementsRelative(
        intervals = c(0, 20),
        increments = c(1, 0.33)
      ),
      cohort_size = CohortSizeConst(size = 3),
      data = emptydata,
      startingDose = 3
    )

    myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)
    options <- McmcOptions(burnin = 10, step = 2, samples = 20)

    mySims <- simulate(
      design,
      args = NULL,
      truth = myTruth,
      nsim = 2,
      seed = 819,
      mcmcOptions = options,
      parallel = FALSE
    )
    simSummary <- summary(mySims, truth = myTruth)

    # Test that show method works (produces output)
    expect_output(show(simSummary))

    # Show methods should print something
    result <- capture.output(show(simSummary))
    expect_true(length(result) > 0)
  })

  ## show-DualSimulationsSummary ----

  test_that("show-DualSimulationsSummary works correctly", {
    mySims <- .DefaultDualSimulations()
    myTruthTox <- function(dose) pnorm((dose - 60) / 10)
    myTruthBio <- function(dose) {
      pmax(0.1, pmin(0.95, 0.2 + 0.6 * (dose / 100)^0.5))
    }

    simSummary <- summary(
      mySims,
      trueTox = myTruthTox,
      trueBiomarker = myTruthBio
    )

    # Test that show method works (produces output)
    expect_output(show(simSummary))

    # Show methods should print something
    result <- capture.output(show(simSummary))
    expect_true(length(result) > 0)

    # Check for specific content in the output
    expect_true(any(grepl("Summary of.*simulations", result)))
    expect_true(any(grepl("Target toxicity interval", result)))
    expect_true(any(grepl("biomarker", result, ignore.case = TRUE)))
    expect_true(any(grepl("Number of patients", result)))
    expect_true(any(grepl("Doses selected", result)))
  })

  # plot summary objects ----

  ## plot-GeneralSimulationsSummary ----

  test_that("plot-GeneralSimulationsSummary works correctly", {
    emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))
    model <- LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    )

    design <- Design(
      model = model,
      nextBest = NextBestNCRM(
        target = c(0.2, 0.35),
        overdose = c(0.35, 1),
        max_overdose_prob = 0.25
      ),
      stopping = StoppingMinPatients(nPatients = 6),
      increments = IncrementsRelative(
        intervals = c(0, 20),
        increments = c(1, 0.33)
      ),
      cohort_size = CohortSizeConst(size = 3),
      data = emptydata,
      startingDose = 3
    )

    myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)
    options <- McmcOptions(burnin = 10, step = 2, samples = 20)

    mySims <- simulate(
      design,
      args = NULL,
      truth = myTruth,
      nsim = 3,
      seed = 819,
      mcmcOptions = options,
      parallel = FALSE
    )
    simSummary <- summary(mySims, truth = myTruth)

    # Test different plot types
    result_n_obs <- plot(simSummary, type = "nObs")
    expect_s3_class(result_n_obs, "ggplot")

    result_dose_selected <- plot(simSummary, type = "doseSelected")
    expect_s3_class(result_dose_selected, "ggplot")

    result_prop_dlts <- plot(simSummary, type = "propDLTs")
    expect_s3_class(result_prop_dlts, "ggplot")

    result_n_above_target <- plot(simSummary, type = "nAboveTarget")
    expect_s3_class(result_n_above_target, "ggplot")

    # Test multiple plot types
    result_multiple <- plot(simSummary, type = c("nObs", "doseSelected"))
    expect_s3_class(result_multiple, "gtable")
  })

  ## plot-SimulationsSummary ----

  test_that("plot-SimulationsSummary works correctly", {
    emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))
    model <- LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    )

    design <- Design(
      model = model,
      nextBest = NextBestNCRM(
        target = c(0.2, 0.35),
        overdose = c(0.35, 1),
        max_overdose_prob = 0.25
      ),
      stopping = StoppingMinPatients(nPatients = 6),
      increments = IncrementsRelative(
        intervals = c(0, 20),
        increments = c(1, 0.33)
      ),
      cohort_size = CohortSizeConst(size = 3),
      data = emptydata,
      startingDose = 3
    )

    myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)
    options <- McmcOptions(burnin = 10, step = 2, samples = 20)

    mySims <- simulate(
      design,
      args = NULL,
      truth = myTruth,
      nsim = 3,
      seed = 819,
      mcmcOptions = options,
      parallel = FALSE
    )
    simSummary <- summary(mySims, truth = myTruth)

    # Test meanFit plot (specific to SimulationsSummary)
    result_mean_fit <- plot(simSummary, type = "meanFit")
    expect_s3_class(result_mean_fit, "ggplot")

    # Test combination with general plots
    result_multiple <- plot(simSummary, type = c("meanFit", "nObs"))
    expect_s3_class(result_multiple, "gtable")
  })

  ## plot-DualSimulationsSummary ----

  test_that("plot-DualSimulationsSummary works correctly", {
    mySims <- .DefaultDualSimulations()
    myTruthTox <- function(dose) pnorm((dose - 60) / 10)
    myTruthBio <- function(dose) {
      pmax(0.1, pmin(0.95, 0.2 + 0.6 * (dose / 100)^0.5))
    }

    simSummary <- summary(
      mySims,
      trueTox = myTruthTox,
      trueBiomarker = myTruthBio
    )

    # Test meanBiomarkerFit plot (specific to DualSimulationsSummary)
    result_mean_bio_fit <- plot(simSummary, type = "meanBiomarkerFit")
    expect_s3_class(result_mean_bio_fit, "ggplot")
    expect_equal(result_mean_bio_fit$labels$x, "Dose level")
    expect_equal(result_mean_bio_fit$labels$y, "Biomarker level")

    # Check that plot has the expected structure
    expect_true(length(result_mean_bio_fit$layers) > 0)
    expect_equal(length(simSummary@dose_grid), length(simSummary@dose_grid))

    # Test combination with other plots
    result_multiple <- plot(simSummary, type = c("meanBiomarkerFit", "meanFit"))
    expect_s3_class(result_multiple, "gtable")
    expect_equal(dim(result_multiple), c(2, 1)) # Should have 2 rows, 1 column

    # Test additional dual-specific plot types
    result_n_obs <- plot(simSummary, type = "nObs")
    expect_s3_class(result_n_obs, "ggplot")

    result_dose_selected <- plot(simSummary, type = "doseSelected")
    expect_s3_class(result_dose_selected, "ggplot")
  })

  # show-PseudoSimulationsSummary ----

  test_that("show-PseudoSimulationsSummary works correctly", {
    pseudo_sims <- .DefaultPseudoSimulations()
    pseudo_summary <- summary(
      pseudo_sims,
      truth = function(x) plogis(-3 + 0.05 * x),
      targetEndOfTrial = 0.3,
      targetDuringTrial = 0.35
    )

    # Test that show method produces output
    expect_output(show(pseudo_summary), "Summary of")
    expect_output(show(pseudo_summary), "simulations")
    expect_output(show(pseudo_summary), "Target probability of DLE")
    expect_output(show(pseudo_summary), "dose level corresponds")
    expect_output(show(pseudo_summary), "Number of patients overall")
    expect_output(show(pseudo_summary), "Number of patients treated above")

    # Test that it returns invisibly a data frame
    result <- capture.output(show_result <- show(pseudo_summary))
    expect_true(is.data.frame(show_result))
    expect_true(ncol(show_result) > 0)
  })

  test_that("show-PseudoSimulationsSummary produces expected output format", {
    pseudo_sims <- .DefaultPseudoSimulations()
    pseudo_summary <- summary(pseudo_sims, truth = function(x) {
      plogis(-3 + 0.05 * x)
    })

    # Capture the output for detailed testing
    output <- capture.output(show(pseudo_summary))

    # Test specific content patterns
    expect_true(any(grepl("Target probability.*end of a trial.*%", output)))
    expect_true(any(grepl("dose level corresponds.*target.*TDEOT", output)))
    expect_true(any(grepl("TDEOT at dose Grid", output)))
    expect_true(any(grepl("Target.*during a trial.*%", output)))
    expect_true(any(grepl("TDDT at dose Grid", output)))
    expect_true(any(grepl("Number of patients overall", output)))
    expect_true(any(grepl(
      "Number of patients treated above.*end of a trial",
      output
    )))
    expect_true(any(grepl(
      "Number of patients treated above.*during a trial",
      output
    )))
  })

  # plot-PseudoSimulationsSummary ----

  test_that("plot-PseudoSimulationsSummary works correctly", {
    pseudo_sims <- .DefaultPseudoSimulations()
    pseudo_summary <- summary(pseudo_sims, truth = function(x) {
      plogis(-3 + 0.05 * x)
    })

    # Test default plot types
    result <- plot(pseudo_summary)
    expect_s3_class(result, "gtable")
    expect_equal(dim(result)[1], 2) # Should have 2 plots by default

    # Test individual plot types
    result_nobs <- plot(pseudo_summary, type = "nObs")
    expect_s3_class(result_nobs, "ggplot")
    expect_equal(result_nobs$labels$x, "Number of patients in total")

    result_dose <- plot(pseudo_summary, type = "doseSelected")
    expect_s3_class(result_dose, "ggplot")
    expect_equal(result_dose$labels$x, "MTD estimate")

    result_prop <- plot(pseudo_summary, type = "propDLE")
    expect_s3_class(result_prop, "ggplot")
    expect_equal(result_prop$labels$x, "Proportion of DLE [%]")

    result_target <- plot(pseudo_summary, type = "nAboveTargetEndOfTrial")
    expect_s3_class(result_target, "ggplot")
    expect_equal(
      result_target$labels$x,
      "Number of patients above target"
    )

    result_mean <- plot(pseudo_summary, type = "meanFit")
    expect_s3_class(result_mean, "ggplot")
    expect_true(grepl(
      "Probability of DLE [%]",
      result_mean$labels$y,
      fixed = TRUE
    ))
  })

  test_that("plot-PseudoSimulationsSummary handles multiple types", {
    pseudo_sims <- .DefaultPseudoSimulations()
    pseudo_summary <- summary(pseudo_sims, truth = function(x) {
      plogis(-3 + 0.05 * x)
    })

    # Test multiple specific types
    result_multi <- plot(pseudo_summary, type = c("nObs", "doseSelected"))
    expect_s3_class(result_multi, "gtable")
    expect_equal(dim(result_multi)[1], 2) # Should have 2 plots

    # Test with all types explicitly
    all_types <- c(
      "nObs",
      "doseSelected",
      "propDLE",
      "nAboveTargetEndOfTrial",
      "meanFit"
    )
    result_all <- plot(pseudo_summary, type = all_types)
    expect_s3_class(result_all, "gtable")
    expect_equal(dim(result_all)[1], 2) # Should have 2 plots
  })

  test_that("plot-PseudoSimulationsSummary fails gracefully with bad input", {
    pseudo_sims <- .DefaultPseudoSimulations()
    pseudo_summary <- summary(pseudo_sims, truth = function(x) {
      plogis(-3 + 0.05 * x)
    })

    expect_error(
      plot(pseudo_summary, type = "invalid_type"),
      "should be one of"
    )
    expect_error(plot(pseudo_summary, type = character(0)), "must be of length")
  })

  # plot-PseudoDualSimulations ----

  test_that("plot-PseudoDualSimulations works correctly", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()

    # Test default plot types
    result <- plot(pseudo_dual_sims)
    expect_s3_class(result, "gtable")

    # Test individual plot types
    result_trajectory <- plot(pseudo_dual_sims, type = "trajectory")
    expect_s3_class(result_trajectory, "ggplot")
    expect_equal(result_trajectory$labels$x, "Patient")
    expect_equal(result_trajectory$labels$y, "Dose Level")

    result_doses <- plot(pseudo_dual_sims, type = "dosesTried")
    expect_s3_class(result_doses, "ggplot")
    expect_equal(result_doses$labels$x, "Dose level")
    expect_equal(result_doses$labels$y, "Average proportion [%]")

    result_sigma2 <- plot(pseudo_dual_sims, type = "sigma2")
    expect_s3_class(result_sigma2, "ggplot")
    expect_true(grepl(
      "Efficacy variance estimates",
      result_sigma2$labels$y,
      ignore.case = TRUE
    ))
  })

  test_that("plot-PseudoDualSimulations handles type combinations", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()

    # Test multiple types
    result_multi <- plot(pseudo_dual_sims, type = c("trajectory", "sigma2"))
    expect_s3_class(result_multi, "gtable")

    # Test all available types
    result_all <- plot(
      pseudo_dual_sims,
      type = c("trajectory", "dosesTried", "sigma2")
    )
    expect_s3_class(result_all, "gtable")
  })

  test_that("plot-PseudoDualSimulations fails gracefully with bad input", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()

    expect_error(
      plot(pseudo_dual_sims, type = "invalid_type"),
      "should be one of"
    )
    expect_error(
      plot(pseudo_dual_sims, type = character(0)),
      "must be of length"
    )
  })

  # plot-PseudoDualFlexiSimulations ----

  test_that("plot-PseudoDualFlexiSimulations works correctly", {
    pseudo_dual_flexi_sims <- .DefaultPseudoDualFlexiSimulations()

    # Test default plot types
    result <- plot(pseudo_dual_flexi_sims)
    expect_s3_class(result, "gtable")

    # Test individual plot types
    result_trajectory <- plot(pseudo_dual_flexi_sims, type = "trajectory")
    expect_s3_class(result_trajectory, "ggplot")
    expect_equal(result_trajectory$labels$x, "Patient")
    expect_equal(result_trajectory$labels$y, "Dose Level")

    result_doses <- plot(pseudo_dual_flexi_sims, type = "dosesTried")
    expect_s3_class(result_doses, "ggplot")
    expect_equal(result_doses$labels$x, "Dose level")
    expect_equal(result_doses$labels$y, "Average proportion [%]")

    result_sigma2 <- plot(pseudo_dual_flexi_sims, type = "sigma2")
    expect_s3_class(result_sigma2, "ggplot")
    expect_true(grepl(
      "Efficacy variance estimates",
      result_sigma2$labels$y,
      ignore.case = TRUE
    ))

    result_sigma2beta_w <- plot(pseudo_dual_flexi_sims, type = "sigma2betaW")
    expect_s3_class(result_sigma2beta_w, "ggplot")
    expect_true(grepl(
      "Random walk model variance estimates",
      result_sigma2beta_w$labels$y,
      ignore.case = TRUE
    ))
  })

  # summary-PseudoDualSimulations ----

  test_that("summary-PseudoDualSimulations works correctly", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()

    # Test basic summary call
    result <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    expect_s4_class(result, "PseudoDualSimulationsSummary")

    # Test with custom target values
    result_custom <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x,
      targetEndOfTrial = 0.25,
      targetDuringTrial = 0.30
    )

    expect_s4_class(result_custom, "PseudoDualSimulationsSummary")
    expect_equal(result_custom@target_end_of_trial, 0.25)
  })

  test_that("summary-PseudoDualSimulations produces expected structure", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()

    result <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    # Test inheritance from parent class
    expect_true(is(result, "PseudoDualSimulationsSummary"))
    expect_true(is(result, "PseudoSimulationsSummary"))

    # Test that dual-specific slots exist
    expect_true("target_gstar" %in% slotNames(result))
    expect_true("target_gstar_at_dose_grid" %in% slotNames(result))
    expect_true("gstar_summary" %in% slotNames(result))
    expect_true("eff_fit_at_dose_most_selected" %in% slotNames(result))
    expect_true("mean_eff_fit" %in% slotNames(result))
  })

  # summary-PseudoDualFlexiSimulations ----

  test_that("summary-PseudoDualFlexiSimulations works correctly", {
    pseudo_dual_flexi_sims <- .DefaultPseudoDualFlexiSimulations()

    # Test basic summary call
    result <- summary(
      pseudo_dual_flexi_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    expect_s4_class(result, "PseudoDualSimulationsSummary")

    # Test with custom target values
    result_custom <- summary(
      pseudo_dual_flexi_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x,
      targetEndOfTrial = 0.25,
      targetDuringTrial = 0.30
    )

    expect_s4_class(result_custom, "PseudoDualSimulationsSummary")
    expect_equal(result_custom@target_end_of_trial, 0.25)
  })

  # show-PseudoDualSimulationsSummary ----

  test_that("show-PseudoDualSimulationsSummary works correctly", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()
    pseudo_dual_summary <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    # Test that show method produces output
    expect_output(show(pseudo_dual_summary), "Target Gstar")
    expect_output(show(pseudo_dual_summary), "maximum gain value")
    expect_output(show(pseudo_dual_summary), "Target Gstar at dose Grid")

    # Test that it calls parent method
    expect_output(show(pseudo_dual_summary), "Summary of")
    expect_output(show(pseudo_dual_summary), "simulations")

    # Test that it returns a data frame invisibly
    result_output <- capture.output(show_result <- show(pseudo_dual_summary))
    expect_true(is.data.frame(show_result))
  })

  test_that("show-PseudoDualSimulationsSummary includes dual-specific content", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()
    pseudo_dual_summary <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    output <- capture.output(show(pseudo_dual_summary))

    # Test dual-specific content
    expect_true(any(grepl("Target Gstar.*maximum gain value", output)))
    expect_true(any(grepl("Target Gstar at dose Grid", output)))

    # Test that parent content is also included
    expect_true(any(grepl("Target probability of DLE", output)))
    expect_true(any(grepl("Number of patients", output)))
  })

  # plot-PseudoDualSimulationsSummary ----

  test_that("plot-PseudoDualSimulationsSummary works correctly", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()
    pseudo_dual_summary <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    # Test default behavior
    result <- plot(pseudo_dual_summary)
    expect_s3_class(result, "gtable")

    # Test dual-specific plot type
    result_eff <- plot(pseudo_dual_summary, type = "meanEffFit")
    expect_s3_class(result_eff, "ggplot")
    expect_true(
      grepl("efficacy", result_eff$labels$y, ignore.case = TRUE) ||
        grepl("eff", result_eff$labels$y, ignore.case = TRUE)
    )

    # Test combination with parent types
    result_combo <- plot(pseudo_dual_summary, type = c("nObs", "meanEffFit"))
    expect_s3_class(result_combo, "gtable")
    expect_equal(dim(result_combo)[1], 2)

    # Test all available types
    all_types <- c(
      "nObs",
      "doseSelected",
      "propDLE",
      "nAboveTargetEndOfTrial",
      "meanFit",
      "meanEffFit"
    )
    result_all <- plot(pseudo_dual_summary, type = all_types)
    expect_s3_class(result_all, "gtable")
    expect_equal(dim(result_all)[1], 2)
  })

  test_that("plot-PseudoDualSimulationsSummary handles edge cases", {
    pseudo_dual_sims <- .DefaultPseudoDualSimulations()
    pseudo_dual_summary <- summary(
      pseudo_dual_sims,
      trueDLE = function(x) plogis(-3 + 0.05 * x),
      trueEff = function(x) 0.2 + 0.004 * x
    )

    # Test error handling
    expect_error(
      plot(pseudo_dual_summary, type = "invalid_type"),
      "should be one of"
    )
    expect_error(
      plot(pseudo_dual_summary, type = character(0)),
      "must be of length"
    )

    # Test single dual-specific type
    result_single <- plot(pseudo_dual_summary, type = "meanEffFit")
    expect_s3_class(result_single, "ggplot")
  })
}
