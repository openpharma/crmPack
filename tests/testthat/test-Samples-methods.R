# size ----

## Samples ----

test_that("size-Samples returns correct number of samples", {
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  samples2 <- h_as_samples(
    list(alpha0 = seq(from = 1, length.out = 50), alpha1 = seq(from = 60, length.out = 50))
  )
  expect_identical(size(samples), 4L)
  expect_identical(size(samples2), 50L)
})

# names ----

## Samples ----

test_that("names-Samples returns correct names of the parameters", {
  samples <- h_as_samples(list(alpha0 = c(0, -1), alpha1 = c(2, 1), beta = c(4, 7)))
  samples2 <- h_as_samples(
    list(a = matrix(5:8, nrow = 2), z = matrix(1:4, nrow = 2))
  )
  expect_identical(names(samples), c("alpha0", "alpha1", "beta"))
  expect_identical(names(samples2), c("a", "z"))
})
