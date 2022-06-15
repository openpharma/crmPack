# simulate ----

## NextBestInfTheory ----

test_that("NextBestInfTheory produces consistent results for empty data", {
  emptydata <- Data(doseGrid = seq(from = 40, to = 200, by = 10))

  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.056373, 0.847935, 0.847935, 2.722500), nrow = 2)
  )

  stop_rule <- StoppingMinPatients(nPatients = 30)
  increments <- IncrementsRelative(interval = 0, increments = 1)
  new_my_next_best <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)
  cohort <- CohortSizeConst(size = 3)
  my_truth <- probFunction(model, alpha0 = 175, alpha1 = 5)

  design <- Design(
    model = model,
    stopping = stop_rule,
    increments = increments,
    nextBest = new_my_next_best,
    cohortSize = cohort,
    data = emptydata,
    startingDose = 40
  )

  sim <- simulate(
    design,
    nsim = 5,
    truth = my_truth,
    mcmcOptions = h_get_mcmc_options()
  )

  result <- summary(sim, truth = my_truth, target = new_my_next_best@target)

  expect_equal(
    result@fitAtDoseMostSelected,
    c(0.985602, 0.985602, 0.985602, 0.985602, 0.985602),
    tolerance = 1e-07
  )
  expect_equal(result@propDLTs, rep(1L, 5))
  expect_equal(result@meanToxRisk, rep(1L, 5))
  expect_equal(result@doseSelected, rep(40, 5))
  expect_equal(result@toxAtDosesSelected, rep(1L, 5))
  expect_snapshot(result@meanFit)
})

test_that("NextBestInfTheory produces consistent results with a dataset", {
  my_data <- h_get_data(placebo = FALSE)

  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.056373, 0.847935, 0.847935, 2.722500), nrow = 2)
  )

  stop_rule <- StoppingMinPatients(nPatients = 5)
  increments <- IncrementsRelative(interval = 0, increments = 1)
  new_my_next_best <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)
  cohort <- CohortSizeConst(size = 3)
  my_truth <- probFunction(model, alpha0 = 175, alpha1 = 5)

  design <- Design(
    model = model,
    stopping = stop_rule,
    increments = increments,
    nextBest = new_my_next_best,
    cohortSize = cohort,
    data = my_data,
    startingDose = 25
  )

  sim <- simulate(
    design,
    nsim = 5,
    truth = my_truth,
    mcmcOptions = h_get_mcmc_options()
  )

  result <- summary(sim, truth = my_truth, target = new_my_next_best@target)
  expect_equal(
    result@fitAtDoseMostSelected,
    c(0.222, 0.222, 0.222, 0.222, 0.222),
    tolerance = 1e-02
  )
  expect_equal(result@propDLTs, rep(0.267, 5), tolerance = 1e-02)
  expect_equal(result@meanToxRisk, rep(1L, 5))
  expect_equal(result@doseSelected, rep(50, 5))
  expect_equal(result@toxAtDosesSelected, rep(1L, 5))
  expect_snapshot(result@meanFit)
})
