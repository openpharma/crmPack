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

# plot ----

## plot-GeneralSimulations ----

test_that("plot-GeneralSimulations works correctly", {
  mySims <- .DefaultSimulations()

  # Test default plot (includes both trajectory and dosesTried, so returns gtable)
  result <- plot(mySims)
  expect_s3_class(result, "gtable")

  # Test trajectory plot only
  result_trajectory <- plot(mySims, type = "trajectory")
  expect_s3_class(result_trajectory, "ggplot")

  # Test dosesTried plot only
  result_doses <- plot(mySims, type = "dosesTried")
  expect_s3_class(result_doses, "ggplot")

  # Test both plot types explicitly
  result_both <- plot(mySims, type = c("trajectory", "dosesTried"))
  expect_s3_class(result_both, "gtable")
})

test_that("plot-GeneralSimulations fails gracefully with bad input", {
  mySims <- .DefaultSimulations()

  expect_error(plot(mySims, type = "invalid_type"), "should be one of")
})

## plot-DualSimulations ----

test_that("plot-DualSimulations works correctly", {
  mySims <- .DefaultDualSimulations()

  # Test plots specific to DualSimulations
  result <- plot(mySims, type = c("trajectory", "sigma2W"))
  expect_s3_class(result, "gtable")

  result_rho <- plot(mySims, type = "rho")
  expect_s3_class(result_rho, "ggplot")
})

# summary ----

## summary-GeneralSimulations ----

test_that("summary-GeneralSimulations works correctly", {
  mySims <- .DefaultSimulations()
  myTruth <- function(x) plogis(x)

  # Test summary with truth function
  result <- summary(mySims, truth = myTruth)
  expect_s4_class(result, "GeneralSimulationsSummary")

  # Test with custom target interval
  result_custom <- summary(mySims, truth = myTruth, target = c(0.15, 0.25))
  expect_s4_class(result_custom, "GeneralSimulationsSummary")
})

test_that("summary-GeneralSimulations fails gracefully with bad input", {
  mySims <- .DefaultSimulations()

  # Test that missing truth function fails
  expect_error(summary(mySims), "\"truth\" is missing")

  # Test that invalid target fails - TODO: correct function
  # expect_error(summary(mySims, truth = myTruth, target = c(0.5, 0.3)), "target")
})

## summary-Simulations ----

test_that("summary-Simulations works correctly", {
  mySims <- .DefaultSimulations()
  myTruth <- plogis

  result <- summary(mySims, truth = myTruth)
  expect_s4_class(result, "SimulationsSummary")

  # Check that it has additional slots compared to GeneralSimulationsSummary
  expect_true("fit_at_dose_most_selected" %in% slotNames(result))
  expect_true("mean_fit" %in% slotNames(result))
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
})

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
  result_nObs <- plot(simSummary, type = "nObs")
  expect_s3_class(result_nObs, "ggplot")

  result_doseSelected <- plot(simSummary, type = "doseSelected")
  expect_s3_class(result_doseSelected, "ggplot")

  result_propDLTs <- plot(simSummary, type = "propDLTs")
  expect_s3_class(result_propDLTs, "ggplot")

  result_nAboveTarget <- plot(simSummary, type = "nAboveTarget")
  expect_s3_class(result_nAboveTarget, "ggplot")

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
  result_meanFit <- plot(simSummary, type = "meanFit")
  expect_s3_class(result_meanFit, "ggplot")

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
  result_meanBioFit <- plot(simSummary, type = "meanBiomarkerFit")
  expect_s3_class(result_meanBioFit, "ggplot")

  # Test combination with other plots
  result_multiple <- plot(simSummary, type = c("meanBiomarkerFit", "meanFit"))
  expect_s3_class(result_multiple, "gtable")
})
