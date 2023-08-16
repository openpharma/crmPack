# simulate ----

test_that("Test if simulate generate the expected output.", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_normal()
  increments <- h_increments_relative()
  next_best <- h_next_best_ncrm()
  size <- CohortSizeConst(size = 3)

  # Extreme truth function, which has constant probability 1 in dose grid range.
  truth <- probFunction(model, alpha0 = 175, alpha1 = 5)
  stop_rule <- StoppingMinPatients(nPatients = 5)
  design <- Design(
    model = model,
    stopping = stop_rule,
    increments = increments,
    nextBest = next_best,
    cohort_size = size,
    data = data,
    startingDose = 25
  )

  my_options <- McmcOptions(burnin = 100, step = 2, samples = 5, rng_kind = "Mersenne-Twister", rng_seed = 3)

  sim <- simulate(
    design,
    nsim = 1,
    truth = truth,
    seed = 819,
    mcmcOptions = my_options
  )

  expect_snapshot(sim)
})


## NextBestInfTheory ----

test_that("NextBestInfTheory produces consistent results for empty data", {
  emptydata <- Data(doseGrid = seq(from = 40, to = 200, by = 10))

  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.056373, 0.847935, 0.847935, 2.722500), nrow = 2)
  )

  stop_rule <- StoppingMinPatients(nPatients = 30)
  increments <- IncrementsRelative(interval = 0, increments = 1)
  new_my_next_best <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)
  cohort <- CohortSizeConst(size = 3)
  my_truth <- probFunction(model, alpha0 = 175, alpha1 = 5)

  design <- Design(
    model = model,
    stopping = stop_rule,
    increments = increments,
    nextBest = new_my_next_best,
    cohort_size = cohort,
    data = emptydata,
    startingDose = 40
  )

  sim <- simulate(
    design,
    nsim = 5,
    truth = my_truth,
    mcmcOptions = h_get_mcmc_options()
  )

  result <- summary(sim, truth = my_truth, target = new_my_next_best@target)

  expect_equal(
    result@fitAtDoseMostSelected,
    c(0.985602, 0.985602, 0.985602, 0.985602, 0.985602),
    tolerance = 1e-07
  )
  expect_equal(result@propDLTs, rep(1L, 5))
  expect_equal(result@meanToxRisk, rep(1L, 5))
  expect_equal(result@doseSelected, rep(40, 5))
  expect_equal(result@toxAtDosesSelected, rep(1L, 5))
  expect_snapshot(result@meanFit)
})

test_that("NextBestInfTheory produces consistent results with a dataset", {
  my_data <- h_get_data(placebo = FALSE)

  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.056373, 0.847935, 0.847935, 2.722500), nrow = 2)
  )

  stop_rule <- StoppingMinPatients(nPatients = 5)
  increments <- IncrementsRelative(interval = 0, increments = 1)
  new_my_next_best <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)
  cohort <- CohortSizeConst(size = 3)
  my_truth <- probFunction(model, alpha0 = 175, alpha1 = 5)

  design <- Design(
    model = model,
    stopping = stop_rule,
    increments = increments,
    nextBest = new_my_next_best,
    cohort_size = cohort,
    data = my_data,
    startingDose = 25
  )

  sim <- simulate(
    design,
    nsim = 5,
    truth = my_truth,
    mcmcOptions = h_get_mcmc_options()
  )

  result <- summary(sim, truth = my_truth, target = new_my_next_best@target)
  expect_equal(
    result@fitAtDoseMostSelected,
    c(0.222, 0.222, 0.222, 0.222, 0.222),
    tolerance = 1e-02
  )
  expect_equal(result@propDLTs, rep(0.267, 5), tolerance = 1e-02)
  expect_equal(result@meanToxRisk, rep(1L, 5))
  expect_equal(result@doseSelected, rep(50, 5))
  expect_equal(result@toxAtDosesSelected, rep(1L, 5))
  expect_snapshot(result@meanFit)
})

## stop_reasons integration test ----

test_that("stop_reasons can be NA with certain stopping rule settings", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_normal()
  increments <- h_increments_relative()
  next_best <- h_next_best_ncrm()
  size <- CohortSizeConst(size = 3)
  # Extreme truth function, which has constant probability 1 in dose grid range.
  truth <- probFunction(model, alpha0 = 175, alpha1 = 5)
  stopping <- StoppingMissingDose()
  design <- Design(
    model = model,
    stopping = stopping,
    increments = increments,
    nextBest = next_best,
    cohort_size = size,
    data = data,
    startingDose = 25
  )
  sim <- simulate(
    design,
    nsim = 5,
    truth = truth,
    seed = 819,
    mcmcOptions = h_get_mcmc_options()
  )
  result <- sim@stop_reasons
  # In this case the trial always stops because no dose is deemed safe enough
  # to continue the trial. This is the default behavior of the
  # stopTrial() method.
  expected <- list(
    "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule.",
    "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule.",
    "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule.",
    "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule.",
    "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
  )
  expect_identical(result, expected)
})

# examine ----

## DADesign ----

test_that("examine for DADesign works as expected", {
  t_max <- 60

  emptydata <- DataDA(
    doseGrid = c(
      0.1, 0.5, 1, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    ),
    Tmax = t_max
  )

  n_pieces <- 10
  lambda_prior <- function(k) {
    n_pieces / (t_max * (n_pieces - k + 0.5))
  }
  l_vector <- as.numeric(t(apply(
    as.matrix(c(1:n_pieces), 1, n_pieces),
    2,
    lambda_prior
  )))

  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = n_pieces,
    l = l_vector
  )

  my_increments <- IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )
  my_next_best <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
  my_size_1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
  )
  my_size_2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
  )
  my_size <- maxSize(my_size_1, my_size_2)
  my_stopping_1 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  )
  my_stopping_2 <- StoppingMinPatients(nPatients = 50)
  my_stopping <- (my_stopping_1 | my_stopping_2)
  my_safety <- SafetyWindowConst(c(6, 2), 7, 7)

  design <- DADesign(
    model = model,
    increments = my_increments,
    nextBest = my_next_best,
    stopping = my_stopping,
    cohort_size = my_size,
    data = emptydata,
    safetyWindow = my_safety,
    startingDose = 3
  )

  options <- McmcOptions(
    burnin = 10,
    step = 1,
    samples = 100,
    rng_kind = "Mersenne-Twister",
    rng_seed = 12
  )
  expect_warning(
    result <- examine(design, mcmcOptions = options, maxNoIncrement = 2),
    "Stopping because 2 times no increment"
  )
  expect_data_frame(result)
  expect_named(result, c("DLTsearly_1", "dose", "DLTs", "nextDose", "stop", "increment"))
})
