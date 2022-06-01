# nextBest ----

## NextBestMTD ----

test_that(".NextBestMTD works as expected", {
  result <- expect_silent(.NextBestMTD())
  expect_valid(result, "NextBestMTD")
})

test_that("NextBestMTD object can be created with user constructor", {
  result <- expect_silent(
    NextBestMTD(
      target = 0.3,
      derive = function(mtd_samples) {
        mean(mtd_samples)
      }
    )
  )
  expect_valid(result, "NextBestMTD")
})

## NextBestNCRM ----

test_that(".NextBestNCRM works as expected", {
  result <- expect_silent(.NextBestNCRM())
  expect_valid(result, "NextBestNCRM")
})

test_that("NextBestNCRM object can be created with user constructor", {
  result <- expect_silent(
    NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    )
  )
  expect_valid(result, "NextBestNCRM")
})

## NextBestThreePlusThree ----

test_that(".NextBestThreePlusThree works as expected", {
  result <- expect_silent(.NextBestThreePlusThree())
  expect_valid(result, "NextBestThreePlusThree")
})

test_that("NextBestThreePlusThree object can be created with user constructor", {
  result <- expect_silent(NextBestThreePlusThree())
  expect_valid(result, "NextBestThreePlusThree")
})

## NextBestInfTheory ----

test_that(".NextBestInfTheory works as expected", {
  result <- expect_silent(.NextBestInfTheory())
  expect_valid(result, "NextBestInfTheory")
})

test_that("NextBestNCRM object can be created with user constructor", {
  expect_silent(NextBestInfTheory(target = 0.25, asymmetry = 0.1))
})

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

# IncrementsRelativeDLTCurrent-class ----

test_that(".IncrementsRelativeDLTCurrent works as expected", {
  result <- expect_silent(.IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

# IncrementsRelativeDLTCurrent-constructor ----

test_that("IncrementsRelativeDLTCurrent object can be created with user constructor", {
  result <- expect_silent(IncrementsRelativeDLTCurrent())
  expect_valid(result, "IncrementsRelativeDLTCurrent")
})

test_that("IncrementsRelativeDLTCurrent fails if DLTintervals is not integer", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(0, 0.6, 1),
      increments = c(1, 0.50)
    ),
    "elements 2 of vector are not integers!"
  )
})

test_that("IncrementsRelativeDLTCurrent fails if DLTintervals is not sorted and unique", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(1, 0),
      increments = c(1, 3)
    ),
    "DLTintervals has to be sorted and have unique values"
  )
})

test_that("IncrementsRelativeDLTCurrent warns if increments does not have same length as intervals", {
  expect_error(
    IncrementsRelativeDLTCurrent(
      DLTintervals = c(0, 1),
      increments = c(1, 3, 100)
    ),
    "increments must have same length as DLTintervals"
  )
})

# IncrementsNumDoseLevels-class ----

test_that(".IncrementsNumDoseLevels works as expected", {
  result <- expect_silent(.IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})

# IncrementsNumDoseLevels-constructor ----

test_that("IncrementsNumDoseLevels object can be created with user constructor", {
  result <- expect_silent(IncrementsNumDoseLevels())
  expect_valid(result, "IncrementsNumDoseLevels")
})



# IncrementsHSRBeta-class ----

test_that(".IncrementsHSRBeta works as expected", {
  result <- expect_silent(.IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})

# IncrementsHSRBeta-constructor ----

test_that("IncrementsHSRBeta object can be created with user constructor", {
  result <- expect_silent(IncrementsHSRBeta())
  expect_valid(result, "IncrementsHSRBeta")
})



# StoppingMTDCV-class ----

test_that(".StoppingMTDCV works as expected", {
  result <- expect_silent(.StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})

# StoppingMTDCV-constructor ----

test_that("StoppingMTDCV object can be created with user constructor", {
  result <- expect_silent(StoppingMTDCV())
  expect_valid(result, "StoppingMTDCV")
})


# StoppingLowestDoseHSRBeta-class ----

test_that(".StoppingLowestDoseHSRBeta works as expected", {
  result <- expect_silent(.StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})

# StoppingLowestDoseHSRBeta-constructor ----

test_that("StoppingLowestDoseHSRBeta object can be created with user constructor", {
  result <- expect_silent(StoppingLowestDoseHSRBeta())
  expect_valid(result, "StoppingLowestDoseHSRBeta")
})
