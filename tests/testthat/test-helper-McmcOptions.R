test_that("h_get_mcmc_options works", {
  x <- h_get_mcmc_options()
  expect_equal(x@rng_kind, "base::Mersenne-Twister")
  expect_equal(x@rng_seed, 1L)
  expect_equal(x@burnin, 50L)
  expect_equal(x@iterations, 58L) # 50 (burnin) + [2 (step) * 4 (samples)]

  x <- h_get_mcmc_options(samples = 10L)
  expect_equal(x@rng_kind, "base::Mersenne-Twister")
  expect_equal(x@rng_seed, 1L)
  expect_equal(x@burnin, 50L)
  expect_equal(x@iterations, 70L) # 50 (burnin) + [2 (step) * 10 (samples)]

  x <- h_get_mcmc_options(burnin = 100L)
  expect_equal(x@rng_kind, "base::Mersenne-Twister")
  expect_equal(x@rng_seed, 1L)
  expect_equal(x@burnin, 100L)
  expect_equal(x@iterations, 108L) # 100 (burnin) + [2 (step) * 4 (samples)]

  x <- h_get_mcmc_options(burnin = 100L, fixed = FALSE)
  expect_equal(x@rng_kind, "base::Mersenne-Twister")
  expect_integer(x@rng_seed, lower = 1L)
  expect_equal(x@burnin, 100L)
  expect_equal(x@iterations, 108L) # 100 (burnin) + [2 (step) * 4 (samples)]
})
