## nolint start

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


test_that("approximate fails gracefully with bad input", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1L:8L,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )

  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 2000,
    rng_seed = 544914,
    rng_kind = "Mersenne-Twister"
  )

  samples <- mcmc(data, model, options)

  expect_error(
    approximate(
      object = samples,
      model = model,
      data = data,
      logNormal = "NotLogical",
      control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
    ),
    "Assertion on 'logNormal' failed: Must be of type 'logical', not 'character'"
  )
  expect_error(
    approximate(
      object = samples,
      model = model,
      data = data,
      verbose = "NotLogical",
      control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
    ),
    "Assertion on 'verbose' failed: Must be of type 'logical', not 'character'"
  )
  expect_error(
    approximate(
      object = samples,
      model = model,
      data = data,
      create_plot = "NotLogical",
      control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
    ),
    "Assertion on 'create_plot' failed: Must be of type 'logical', not 'character'"
  )
  expect_error(
    approximate(
      object = samples,
      model = model,
      data = data,
      refDose = "NotNumeric",
      control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
    ),
    "Assertion on 'refDose' failed: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    approximate(
      object = samples,
      model = model,
      data = data,
      points = c(1:5, "NotNumeric"),
      control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
    ),
    "Assertion on 'points' failed: Must be of type 'numeric', not 'character'"
  )
})

test_that("Approximate works correctly", {
  data <- Data(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
    y = c(0, 0, 0, 0, 0, 0, 1, 0),
    ID = 1L:8L,
    cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
  )

  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )

  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 2000,
    rng_seed = 544914,
    rng_kind = "Mersenne-Twister"
  )

  samples <- mcmc(data, model, options)

  actual <- approximate(
    object = samples,
    model = model,
    data = data,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
  )

  expect_equal(length(actual), 2)
  expect_set_equal(names(actual), c("model", "plot"))
  for (slot_name in slotNames(actual$model)) {
    if (!is.function(slot(actual$model, slot_name))) {
      expect_snapshot(slot(actual$model, slot_name))
    }
  }
  vdiffr::expect_doppelganger("approximate-Samples", actual$plot)

  actual1 <- approximate(
    object = samples,
    model = model,
    data = data,
    create_plot = FALSE,
    control = list(threshold.stop = 0.1, max.time = 1, maxit = 1)
  )
  expect_equal(length(actual1), 1)
  expect_set_equal(names(actual1), c("model"))
})

## nolint end
