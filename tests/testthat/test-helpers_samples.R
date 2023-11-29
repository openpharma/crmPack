test_that("h_convert_ordinal_samples works correctly", {
  ordinal_data <- .DefaultDataOrdinal()
  ordinal_model <- .DefaultLogisticLogNormalOrdinal()
  options <- McmcOptions(
    burnin = 5,
    step = 2,
    samples = 10,
    rng_kind = "Mersenne-Twister",
    rng_seed = 0
  )
  suppressWarnings({
    samples <- mcmc(ordinal_data, ordinal_model, options)
  })

  actual1 <- h_convert_ordinal_samples(samples, 1L)
  expect_class(actual1, "Samples")
  expect_equal(names(actual1@data), c("alpha0", "alpha1"))
  expect_equal(actual1@data$alpha0, samples@data$alpha1)
  expect_equal(actual1@data$alpha1, samples@data$beta)
  expect_identical(actual1@options, samples@options)

  actual2 <- h_convert_ordinal_samples(samples, 2L)
  expect_class(actual2, "Samples")
  expect_equal(names(actual2@data), c("alpha0", "alpha1"))
  expect_equal(actual2@data$alpha0, samples@data$alpha2)
  expect_equal(actual2@data$alpha1, samples@data$beta)
  expect_identical(actual2@options, samples@options)
})

test_that("h_convert_ordinal_samples fails gracefully with bad input", {
  ordinal_data <- .DefaultDataOrdinal()
  ordinal_model <- .DefaultLogisticLogNormalOrdinal()
  options <- McmcOptions(
    burnin = 5,
    step = 2,
    samples = 10,
    rng_kind = "Mersenne-Twister",
    rng_seed = 0
  )
  suppressWarnings({
    samples <- mcmc(ordinal_data, ordinal_model, options)
  })

  expect_error(
    h_convert_ordinal_samples(samples, grade = -1L),
    "Assertion on 'grade' failed: Element 1 is not >= 1."
  )
  expect_error(
    h_convert_ordinal_samples(samples, grade = 2.5),
    "Assertion on 'grade' failed: Must be of type 'integer', not 'double'"
  )
  expect_error(
    h_convert_ordinal_samples(samples, grade = 3L),
    paste0(
      "Assertion on 'c\\(paste0\\(\"alpha\", 1:grade\\), \"beta\"\\)' failed: ",
      "Must be a subset of \\{'alpha1','alpha2','beta'\\}, but has additional ",
      "elements \\{'alpha3'\\}."
    )
  )
  expect_error(
    h_convert_ordinal_samples(ordinal_data, grade = 1L),
    "Assertion on 'obj' failed: Must inherit from class 'Samples', but has class 'DataOrdinal'."
  )
})
