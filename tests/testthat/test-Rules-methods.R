# nextBest ----

## NextBestMTD ----

test_that("nextBest-NextBestMTD returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  result <- nextBest(nb_mtd, doselimit = 90, samples, model, data)
  expect_identical(result$value, 75)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMTD", result$plot)
})

test_that("nextBest-NextBestMTD returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  result <- nextBest(nb_mtd, doselimit = numeric(0), samples, model, data)
  expect_identical(result$value, 100)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMTD without doselimit", result$plot)
})

## NextBestNCRM ----

test_that("nextBest-NextBestNCRM returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, doselimit = 45, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM", result$plot)
})

test_that("nextBest-NextBestNCRM returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, doselimit = numeric(0), samples, model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM without doselimit", result$plot)
})

## NextBestNCRMLoss ----

test_that("nextBest-NextBestNCRMLoss returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25,
    losses = c(1, 0, 2)
  )

  result <- nextBest(nb_ncrm_loss, doselimit = 45, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss", result$plot)
})

test_that("nextBest-NextBestNCRMLoss returns expected values of the objects (loss function of 4 elements)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35), overdose = c(0.35, 0.6), unacceptable_int = c(0.6, 1),
    max_overdose_prob = 0.25, losses = c(1, 0, 1, 2)
  )
  result <- nextBest(nb_ncrm_loss, samples, doselimit = numeric(0), model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss with losses of 4", result$plot)
})


## NextBestInfTheory ----

test_that("nextBest-NextBestInfTheory returns correct next dose", {
  data <- h_get_data(placebo = FALSE)
  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.06, 0.85, 0.85, 2.72), nrow = 2)
  )
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  nb_it <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)

  result <- nextBest(nb_it, doselimit = 75, samples, model, data)
  expect_identical(result$value, 25)
})

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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
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
