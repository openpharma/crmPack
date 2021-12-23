# sample_size ----

test_that("Number of samples is correctly computed", {
  object <- McmcOptions(samples = 100, step = 3)
  result <- sample_size(object)

  expect_identical(result, 100L)
})

# save_sample ----

test_that("Sample should be saved as expected for a given iteration number", {
  object <- McmcOptions(burnin = 20, step = 3)
  result <- save_sample(object, 26)

  expect_identical(result, TRUE)
})

test_that("Sample should not be saved for a given iteration number", {
  object <- McmcOptions(burnin = 20, step = 3)
  result <- save_sample(object, 2)

  expect_identical(result, FALSE)
})
