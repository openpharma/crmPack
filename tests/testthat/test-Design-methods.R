# simulate ----

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

## DesignGrouped ----

test_that("simulate for DesignGrouped works as expected", {
  object <- DesignGrouped(
    model = LogisticLogNormalGrouped(mean = rep(-1, 4), cov = diag(5, 4), ref_dose = 1),
    mono = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 9),
      increments = IncrementsDoseLevels(levels = 5),
      cohort_size = CohortSizeConst(3),
      data = Data(doseGrid = 1:100),
      startingDose = 1
    ),
    same_dose = TRUE,
    first_cohort_mono_only = TRUE
  )
  my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
  my_combo_truth <- function(x) plogis(-4 + 0.5 * log(x / 0.1))

  result <- expect_silent(simulate(
    object,
    nsim = 2,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))

  expect_list(result)
  expect_names(names(result), identical.to = c("mono", "combo"))
  expect_valid(result$mono, "Simulations")
  expect_valid(result$combo, "Simulations")

  mono_trial <- result$mono@data[[2L]]
  combo_trial <- result$combo@data[[2L]]

  # First cohort is only mono at the starting dose (lowest in dose grid).
  expect_true(all(mono_trial@xLevel[1:3] == 1))

  # We have the same dose for subsequent cohorts.
  expect_true(all(mono_trial@xLevel[4:6] == combo_trial@xLevel[1:3]))
  expect_true(all(mono_trial@xLevel[7:9] == combo_trial@xLevel[4:6]))
})

test_that("simulate for DesignGrouped works as expected with different doses, parallel first cohort", {
  object <- DesignGrouped(
    model = LogisticLogNormalGrouped(mean = rep(-1, 4), cov = diag(5, 4), ref_dose = 1),
    mono = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 20),
      increments = IncrementsDoseLevels(levels = 5),
      cohort_size = CohortSizeConst(3),
      data = Data(doseGrid = 1:100),
      startingDose = 1
    ),
    same_dose = FALSE,
    first_cohort_mono_only = FALSE
  )
  my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
  my_combo_truth <- function(x) plogis(-4 + 0.9 * log(x / 0.1))

  result <- expect_silent(simulate(
    object,
    nsim = 1,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))

  expect_list(result)
  expect_names(names(result), identical.to = c("mono", "combo"))
  expect_valid(result$mono, "Simulations")
  expect_valid(result$combo, "Simulations")

  mono_trial <- result$mono@data[[1L]]
  combo_trial <- result$combo@data[[1L]]

  # First cohort is joint at starting dose.
  expect_true(all(mono_trial@xLevel[1:3] == combo_trial@xLevel[1:3]))

  # We have different doses in subsequent cohorts.
  expect_false(all(mono_trial@xLevel[4:20] == combo_trial@xLevel[4:20]))
})

test_that("simulate for DesignGrouped works when first patient is dosed separately, different combo design", {
  object <- DesignGrouped(
    model = LogisticLogNormalGrouped(mean = rep(-1, 4), cov = diag(5, 4), ref_dose = 1),
    mono = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 10),
      increments = IncrementsDoseLevels(levels = 3),
      cohort_size = CohortSizeConst(2),
      data = Data(doseGrid = 1:100),
      startingDose = 10
    ),
    combo = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 20),
      increments = IncrementsDoseLevels(levels = 5),
      cohort_size = CohortSizeConst(3),
      data = Data(doseGrid = 1:100),
      startingDose = 1
    ),
    same_dose = FALSE,
    first_cohort_mono_only = FALSE
  )
  my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
  my_combo_truth <- function(x) plogis(-2 + 0.9 * log(x / 0.1))

  result <- expect_silent(simulate(
    object,
    nsim = 1,
    seed = 123,
    truth = my_truth,
    firstSeparate = TRUE,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))

  expect_list(result)
  expect_names(names(result), identical.to = c("mono", "combo"))
  expect_valid(result$mono, "Simulations")
  expect_valid(result$combo, "Simulations")

  mono_trial <- result$mono@data[[1L]]
  combo_trial <- result$combo@data[[1L]]

  # We expect at least one cohort with just one patient in the combo arm here
  # because of the high toxicity.
  expect_true(any(table(combo_trial@cohort) == 1))

  # Check that we had the different cohort sizes between the two arms.
  expect_true(max(table(mono_trial@cohort)) == 2)
  expect_true(max(table(combo_trial@cohort)) == 3)

  # Check that we had different starting doses in the two arms.
  expect_true(mono_trial@x[1] == 10)
  expect_true(combo_trial@x[1] == 1)
})

test_that("simulate for DesignGrouped works with different starting doses and first mono", {
  object <- DesignGrouped(
    model = LogisticLogNormalGrouped(mean = rep(-1, 4), cov = diag(5, 4), ref_dose = 1),
    mono = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 9),
      increments = IncrementsDoseLevels(levels = 5),
      cohort_size = CohortSizeConst(3),
      data = Data(doseGrid = 1:100),
      startingDose = 10
    ),
    combo = Design(
      model = .LogisticNormal(),
      nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
      stopping = StoppingMinPatients(nPatients = 9),
      increments = IncrementsRelative(c(0, 100), c(2, 1)),
      cohort_size = CohortSizeConst(3),
      data = Data(doseGrid = 1:100),
      startingDose = 5
    ),
    same_dose = FALSE,
    first_cohort_mono_only = TRUE
  )
  my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
  my_combo_truth <- function(x) plogis(-4 + 0.5 * log(x / 0.1))

  result <- expect_silent(simulate(
    object,
    nsim = 2,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))

  expect_list(result)
  expect_names(names(result), identical.to = c("mono", "combo"))
  expect_valid(result$mono, "Simulations")
  expect_valid(result$combo, "Simulations")

  mono_trial <- result$mono@data[[2L]]
  combo_trial <- result$combo@data[[2L]]

  # First cohort is only mono at the starting dose.
  expect_true(all(mono_trial@xLevel[1:3] == 10))

  # In first combo cohort we have the expected starting dose.
  expect_true(all(combo_trial@xLevel[1:3] == 5))
})

test_that("simulate for DesignGrouped allows to stop mono when combo stops", {
  mono_arm <- Design(
    model = .LogisticNormal(),
    nextBest = NextBestNCRM(target = c(0.3, 0.6), overdose = c(0.6, 1), max_overdose_prob = 0.7),
    stopping = StoppingMinPatients(nPatients = 20),
    increments = IncrementsDoseLevels(levels = 5),
    cohort_size = CohortSizeConst(3),
    data = Data(doseGrid = 1:100),
    startingDose = 10
  )
  combo_arm <- .Design(
    mono_arm,
    stopping = StoppingMinPatients(nPatients = 1) # Such that we stop after the first cohort.
  )
  object <- DesignGrouped(
    model = LogisticLogNormalGrouped(mean = rep(-1, 4), cov = diag(5, 4), ref_dose = 1),
    mono = mono_arm,
    combo = combo_arm,
    same_dose = FALSE,
    first_cohort_mono_only = FALSE,
    stop_mono_with_combo = TRUE
  )
  my_truth <- function(x) plogis(-4 + 0.2 * log(x / 0.1))
  my_combo_truth <- function(x) plogis(-4 + 0.5 * log(x / 0.1))

  result <- expect_silent(simulate(
    object,
    nsim = 2,
    seed = 123,
    truth = my_truth,
    combo_truth = my_combo_truth,
    mcmcOptions = h_get_mcmc_options()
  ))

  expect_list(result)
  expect_names(names(result), identical.to = c("mono", "combo"))
  expect_valid(result$mono, "Simulations")
  expect_valid(result$combo, "Simulations")

  # We see the expected stop reasons.
  expect_identical(
    result$mono@stop_reasons,
    rep(list("mono stopped because combo stopped"), 2)
  )
  expect_identical(
    result$combo@stop_reasons,
    rep(list("Number of patients is 3 and thus reached the prespecified minimum number 1"), 2)
  )

  # But mono still had the initial 3 patients in both simulations.
  expect_identical(
    lapply(result$mono@data, slot, "nObs"),
    rep(list(3L), 2)
  )
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
    result <- examine(design, mcmcOptions = options, maxNoIncrement = 2L),
    "Stopping because 2 times no increment"
  )
  expect_data_frame(result)
  expect_named(result, c("DLTsearly_1", "dose", "DLTs", "nextDose", "stop", "increment"))
})
