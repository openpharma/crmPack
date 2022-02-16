# maxDose-IncrementsNumDoseLevels ----

# Sample data to test maxDose of IncrementsNumDoseLevels method.
my_data <- Data(x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 12, 12, 12, 16, 16, 16, 10, 10, 10),
                y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0),
                cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
                doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
                )

test_that("IncrementsNumDoseLevels works correctly if basislevel 'last' is defined", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2,
    basisLevel = "last"
    )
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel='last'.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel is not defined and default is used", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2
  )
  result <- maxDose(
    increments,
    data = my_data
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
    data = my_data
  )
  expect_equal(result, 20) # maxDose is 20 if basislevel='max'.
})


# stopTrial-StoppingMTDCV ----

# Sample data to test Stopping Rule MTD precisely estimated: CV(MTD) <= 30%.
my_data <- h_get_data()
# Model that leads to a CV of 25% given the data and the options.
my_model <- LogisticKadane(0.3, xmin = 0.001, xmax = 100)
my_options <- McmcOptions(
  burnin = 10^3, step = 1, samples = 10^4, rng_kind = "Mersenne-Twister", rng_seed = 94
)

my_samples <- mcmc(my_data, my_model, my_options)

test_that("StoppingMTDCV works correctly if CV is below threshold", {
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 30)
  result <- stopTrial(
    stopping = stopping,
    dose = 7,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = "CV of MTD is 23 % and thus below the required precision threshold of 30 %"
  )
  expect_identical(result, expected) # CV is 23% < 30%.
})

test_that("StoppingMTDCV works correctly if CV is above threshold", {
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
    message = "CV of MTD is 23 % and thus above the required precision threshold of 20 %"
  )
  expect_identical(result, expected) # CV is 23% > 20%.
})


# stopTrial-StoppingLowestDoseHSRBeta ----

# Sample data to test Stopping Rule lowest active dose is toxic.
my_data <- h_get_data()
my_model <- LogisticKadane(0.3, xmin = 0.001, xmax = 100)
my_options <- McmcOptions(
  burnin = 1, step = 1, samples = 1, rng_kind = "Mersenne-Twister", rng_seed = 94
)
my_samples <- mcmc(my_data, my_model, my_options)

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
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
    message = "Probability that the lowest active dose of 25 being toxic based on posterior Beta distribution using a Beta(1,1) prior is 24% and thus below the required 90% threshold."
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
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
    message = "Probability that the lowest active dose of 25 being toxic based on posterior Beta distribution using a Beta(1,1) prior is 24% and thus above the required 10% threshold."
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

my_data <- h_get_data_no_plcb()
my_model <- LogisticKadane(0.3, xmin = 0.001, xmax = 100)
my_options <- McmcOptions(
  burnin = 1, step = 1, samples = 1, rng_kind = "Mersenne-Twister", rng_seed = 94
)
my_samples <- mcmc(my_data, my_model, my_options)

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
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
    message = "Probability that the lowest active dose of 25 being toxic based on posterior Beta distribution using a Beta(1,1) prior is 24% and thus below the required 90% threshold."
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
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
    message = "Probability that the lowest active dose of 25 being toxic based on posterior Beta distribution using a Beta(1,1) prior is 24% and thus above the required 10% threshold."
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

my_data <- h_get_data()
my_data@x[my_data@cohort==1] <- c(0.001,75,75,75)

my_model <- LogisticKadane(0.3, xmin = 0.001, xmax = 100)
my_options <- McmcOptions(
  burnin = 1, step = 1, samples = 1, rng_kind = "Mersenne-Twister", rng_seed = 94
)
my_samples <- mcmc(my_data, my_model, my_options)

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
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

my_data <- h_get_data_no_plcb()
my_data@x[my_data@cohort==1] <- c(75,75,75)

my_model <- LogisticKadane(0.3, xmin = 0.001, xmax = 100)
my_options <- McmcOptions(
  burnin = 1, step = 1, samples = 1, rng_kind = "Mersenne-Twister", rng_seed = 94
)
my_samples <- mcmc(my_data, my_model, my_options)

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
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
