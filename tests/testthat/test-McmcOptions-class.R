# McmcOptions-class ----

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
      RNG = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2L)
    )
  )
  expect_valid(result, "McmcOptions")
})

# McmcOptions-constructor ----

test_that("McmcOptions object can be created with user constructor", {
  result <- expect_silent(McmcOptions())
  expect_valid(result, "McmcOptions")
})

test_that("McmcOptions object can be created with custom values", {
  result <- expect_silent(
    McmcOptions(
      burnin = 1000L,
      samples = 1000L,
      RNG_kind = "base::Wichmann-Hill",
      RNG_seed = 2L
    )
  )
  expect_valid(result, "McmcOptions")
})
