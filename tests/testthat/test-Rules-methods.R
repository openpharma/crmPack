# maxDose-IncrementsNumDoseLevels ----

test_that("IncrementsNumDoseLevels works correctly if basislevel 'last' is defined", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2,
    basisLevel = "last"
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel='last'.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel is not defined and default is used", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel not defined, then reference value is used.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel 'max' is defined", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2,
    basisLevel = "max"
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 20) # maxDose is 20 if basislevel='max'.
})

# maxDose-IncrementsRelativeDLTCurrent ----

test_that("IncrementsRelativeDLTCurrent works correctly", {
  increments <- IncrementsRelativeDLTCurrent(
    DLTintervals = c(0, 1, 3),
    increments = c(1, 0.33, 0.2)
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 13.3) # maxDose is 13.3 because last dose was 10 with 1 DLT.
})

# maxDose-IncrementsHSRBeta ----

test_that("IncrementsHSRBeta works correctly if toxcicity probability is below threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 300) # maxDose is 300 as toxicity probability of no dose is above 0.95.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.9)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability"
), {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 1L] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability of placebo is above threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@x == 0.001] <- c(1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 300) # maxDose is 300 as placebo is ignored.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability and placebo == T, but not appplied"
), {
  my_data <- h_get_data()
  my_data@x <- c(rep(25, 4), rep(50, 4), rep(100, 4))
  my_data@y[my_data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability (no placebo)"
), {
  my_data <- h_get_data(placebo = FALSE)
  my_data@y[my_data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.90.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability (no placebo)", {
  my_data <- h_get_data(placebo = FALSE)
  my_data@y[my_data@cohort == 3] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})


# stopTrial-StoppingMTDCV ----

test_that("StoppingMTDCV works correctly if CV is below threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = TRUE))
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 50)
  result <- stopTrial(
    stopping = stopping,
    dose = 7,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = "CV of MTD is 40 % and thus below the required precision threshold of 50 %"
  )
  expect_identical(result, expected) # CV is 23% < 30%.
})

test_that("StoppingMTDCV works correctly if CV is above threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = TRUE))
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 20)
  result <- stopTrial(
    stopping = stopping,
    dose = 7,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "CV of MTD is 40 % and thus above the required precision threshold of 20 %"
  )
  expect_identical(result, expected) # CV is 23% > 20%.
})

# stopTrial-StoppingLowestDoseHSRBeta ----

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.9)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic",
      "based on posterior Beta distribution using a Beta(1,1) prior",
      "is 24% and thus below the required 90% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is toxic", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic",
      "based on posterior Beta distribution using a Beta(1,1) prior",
      "is 24% and thus above the required 10% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
  my_data <- h_get_data()
  my_data@x[my_data@cohort == 1] <- c(0.001, 75, 75, 75)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Lowest active dose not tested, stopping rule not applied."
  )
  expect_identical(result, expected) # First active dose not applied.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
  my_data <- h_get_data(placebo = FALSE)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.9)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic based on",
      "posterior Beta distribution using a Beta(1,1) prior is 17% and thus",
      "below the required 90% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is toxic", {
  my_data <- h_get_data(placebo = FALSE)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic based on",
      "posterior Beta distribution using a Beta(1,1) prior is 17% and thus",
      "above the required 10% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
  my_data <- h_get_data(placebo = FALSE)
  my_data@x[my_data@cohort == 1] <- c(75, 75, 75, 75)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(small = TRUE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Lowest active dose not tested, stopping rule not applied."
  )
  expect_identical(result, expected) # First active dose not applied.
})

test_that("NextBestInfTheory can be initiliazed ", {
  expect_silent(NextBestInfTheory(target = 0.25, asymmetry = 0.1))
})

test_that("h_info_theory_dist works as expected", {
  # Values calculated using a different program.
  p <- c(0.01, 0.2, 0.7)
  gamma <- c(0.5, 0, 0.3)
  a <- c(1, 1.8, 0.5)
  expected <- c(24.25, 0.76, 1.16)
  calculated <- h_info_theory_dist(p, gamma, a)
  expect_equal(calculated, expected, tolerance = 0.01)
})

test_that("NextBestInfTheory produces consistent results for empty data", {
  emptydata <- Data(doseGrid = seq(from = 40, to = 200, by = 10))

  sigma_0 <- 1.0278
  sigma_1 <- 1.65
  rho <- 0.5
  cov <- matrix(
    c(sigma_0^2, rho * sigma_0 * sigma_1, rho * sigma_0 * sigma_1, sigma_1^2),
    nrow = 2
  )
  model <- LogisticLogNormal(mean = c(-4.47, 0.0033), cov = cov)

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
    mcmcOptions = h_get_mcmc_options(small = TRUE, fixed = TRUE)
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
  my_data <- h_get_data_no_plcb()

  sigma_0 <- 1.0278
  sigma_1 <- 1.65
  rho <- 0.5
  cov <- matrix(
    c(sigma_0^2, rho * sigma_0 * sigma_1, rho * sigma_0 * sigma_1, sigma_1^2),
    nrow = 2
  )
  model <- LogisticLogNormal(mean = c(-4.47, 0.0033), cov = cov)

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
    mcmcOptions = h_get_mcmc_options(small = TRUE, fixed = TRUE)
  )
  result <- summary(sim, truth = my_truth, target = new_my_next_best@target)
  expect_equal(
    result@fitAtDoseMostSelected,
    c(0.23991, 0.23991, 0.23991, 0.23991, 0.23991),
    tolerance = 1e-05
  )
  expect_equal(result@propDLTs, rep(0.333, 5), tolerance = 1e-02)
  expect_equal(result@meanToxRisk, rep(1L, 5))
  expect_equal(result@doseSelected, rep(50, 5))
  expect_equal(result@toxAtDosesSelected, rep(1L, 5))
  expect_snapshot(result@meanFit)
})
