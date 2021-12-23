# sampleSize ----

test_that("Number of samples is correctly computed", {
  object <- McmcOptions(samples = 100, step = 3)
  result <- sampleSize(object)

  expect_identical(result, 100L)
})

# saveSample ----

test_that("Sample should be saved as expected for a given iteration number", {
  object <- McmcOptions(burnin = 20, step = 3)
  result <- saveSample(object, 26)

  expect_identical(result, TRUE)
})

test_that("Sample should not be saved for a given iteration number", {
  object <- McmcOptions(burnin = 20, step = 3)
  result <- saveSample(object, 2)

  expect_identical(result, FALSE)
})
