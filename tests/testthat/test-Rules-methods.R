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
