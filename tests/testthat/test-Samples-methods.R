# sampleSize ----

## Samples ----

test_that("sampleSize-Samples returns correct number of samples", {
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  samples2 <- h_as_samples(
    list(alpha0 = seq(from = 1, length.out = 50), alpha1 = seq(from = 60, length.out = 50))
  )
  expect_identical(sampleSize(samples), 4L)
  expect_identical(sampleSize(samples2), 50L)
})
