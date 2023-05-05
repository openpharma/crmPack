# McmcOptions ----

test_that(".McmcOptions works as expected", {
  result <- expect_silent(.McmcOptions())
  expect_valid(result, "McmcOptions")
})

test_that(".McmcOptions works as expected with custom values", {
  result <- expect_silent(
    .McmcOptions(
      iterations = 100L,
      burnin = 10L,
      step = 4L,
      rng_kind = "base::Wichmann-Hill",
      rng_seed = 2L
    )
  )
  expect_valid(result, "McmcOptions")
})

test_that("McmcOptions object can be created with user constructor", {
  result <- expect_silent(McmcOptions())
  expect_valid(result, "McmcOptions")
})

test_that("McmcOptions object can be created with custom values", {
  result <- expect_silent(
    McmcOptions(
      burnin = 1000L,
      samples = 1000L,
      rng_kind = "Wichmann-Hill",
      rng_seed = 2L
    )
  )
  expect_valid(result, "McmcOptions")
})

test_that("McmcOptions object can be created with custom RNG and NA values", {
  result <- expect_silent(
    McmcOptions(
      rng_kind = "Wichmann-Hill",
      rng_seed = NA
    )
  )
  expect_valid(result, "McmcOptions")
})
