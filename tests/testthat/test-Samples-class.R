# Samples ----

test_that(".Samples works as expected", {
  result <- expect_silent(.Samples())
  expect_valid(result, "Samples")
})

test_that("Samples object can be created with user constructor", {
  data <- list(param_a = 1:10, param_b = 21:30)
  options <- McmcOptions(samples = 10)

  result <- expect_silent(
    Samples(data, options)
  )
  expect_valid(result, "Samples")
  expect_identical(result@data, data)
  expect_identical(result@options, options)
})
