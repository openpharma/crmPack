skip_on_cran_but_not_ci()

# nextBest ----

## NextBestEWOC ----

test_that("recommend-NextBestEWOC returns expected next dose based on overdose control", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1, 0, 0.5, 1), alpha1 = c(0.1, 0.2, 0.3, 0.4))
  )

  nb_ewoc <- NextBestEWOC(
    target = 0.30,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  doselimit <- 45
  expected <- nextBest(
    nb_ewoc,
    doselimit,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  actual <- recommend(
    nb_ewoc,
    doselimit,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )

  expect_identical(actual@value, expected$value)
})

test_that("recommend-NextBestEWOC returns expected next dose when no doselimit", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1, 0, 0.5, 1), alpha1 = c(0.1, 0.2, 0.3, 0.4))
  )

  nb_ewoc <- NextBestEWOC(
    target = 0.30,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  expected <- nextBest(nb_ewoc, Inf, samples, model, data)
  actual <- recommend(nb_ewoc, Inf, samples, model, data)

  expect_identical(actual@value, expected$value)
})

## NextBestMTD ----

test_that("recommend-NextBestMTD returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  expected <- nextBest(
    nextBest = nb_mtd,
    doselimit = 90,
    samples = samples,
    model = model,
    data = data
  )
  actual <- recommend(
    nextBest = nb_mtd,
    doselimit = 90,
    samples = samples,
    model = model,
    data = data
  )
  expect_identical(actual@value, expected$value)
})

test_that("recommend-NextBestMTD returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  expected <- nextBest(nb_mtd, Inf, samples, model, data)
  actual <- recommend(nb_mtd, Inf, samples, model, data)
  expect_identical(actual@value, expected$value)
})

test_that("recommend-NextBestMTD returns correct next dose and plot when doselimit=0", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  expected <- nextBest(
    nextBest = nb_mtd,
    doselimit = 0,
    samples = samples,
    model = model,
    data = data
  )
  actual <- recommend(
    nextBest = nb_mtd,
    doselimit = 0,
    samples = samples,
    model = model,
    data = data
  )
  expect_identical(actual@value, expected$value)
})


## NextBestNCRM ----

test_that("recommend-NextBestNCRM returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  expected <- nextBest(
    nb_ncrm,
    45,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  actual <- recommend(
    nb_ncrm,
    45,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  expect_identical(actual@value, expected$value)
})

test_that("recommend-NextBestNCRM returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  expected <- nextBest(
    nb_ncrm,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  actual <- recommend(
    nb_ncrm,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  expect_identical(expected$value, actual@value)
})

test_that("recommend-NextBestNCRM can accept additional arguments and pass them to prob inside", {
  my_data <- h_get_data_grouped()
  my_model <- .DefaultLogisticLogNormalGrouped()
  my_samples <- mcmc(
    my_data,
    my_model,
    h_get_mcmc_options(samples = 10, burnin = 10)
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
  result <- recommend(
    nb_ncrm,
    Inf,
    my_samples,
    my_model,
    my_data,
    group = "mono"
  )
  expect_identical(result@value, NA_real_)
})

test_that("recommend-NextBestNCRM-DataCombo works as expected", {
  set.seed(123)
  data <- h_get_data_combo()
  model <-   TwoDrugsCombo(
    single_models = list(
      drug1 = LogisticLogNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
        ref_dose = 10
      ),
      drug2 = LogisticLogNormal(
        mean = c(-0.7, 0.8),
        cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
        ref_dose = 20
      )
    ),
    gamma = 0,
    tau = 1,
    log_normal_eta = FALSE
  )
  samples <- mcmc(
    data,
    model,
    h_get_mcmc_options(samples = 10, burnin = 10)
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )
  increments <- IncrementsComboCartesian(
    drug1 = IncrementsRelative(0, 0.5),
    drug2 = IncrementsRelative(0, 1)
  )
  doselimit <- maxDose(increments, data)
  doselimit[3, 2] <- 20
  expected <- nextBest(
    nb_ncrm,
    doselimit,
    samples,
    model,
    data
  )
  actual <- recommend(
    nb_ncrm,
    doselimit,
    samples,
    model,
    data
  )
  expect_equal(actual@value, expected$value)
})

test_that("recommend-NextBestNCRM-DataCombo uses configured drug names in probs", {
  set.seed(123)
  data <- DataCombo(
    doseGrid = list(compound_a = c(10, 20, 30), compound_b = c(20, 40)),
    x = cbind(
      compound_a = c(10, 10, 10, 20, 20, 20),
      compound_b = c(20, 20, 20, 20, 20, 20)
    ),
    y = c(0L, 0L, 1L, 0L, 0L, 0L),
    ID = 1L:6L,
    cohort = c(1L, 1L, 1L, 2L, 2L, 2L)
  )
  model <- TwoDrugsCombo(list(
    compound_a = .DefaultLogisticLogNormal(),
    compound_b = .DefaultLogisticLogNormal()
  ))
  samples <- mcmc(
    data,
    model,
    h_get_mcmc_options(samples = 10, burnin = 10)
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(
    nb_ncrm,
    maxDose(
      IncrementsComboCartesian(
        drug1 = IncrementsRelative(0, 0.5),
        drug2 = IncrementsRelative(0, 1)
      ),
      data
    ),
    samples,
    model,
    data
  )
})

## NextBestNCRM-DataParts ----

test_that("recommend-NextBestNCRM-DataParts returns expected values of the objects", {
  data <- h_get_data_parts(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(
    nb_ncrm,
    45,
    samples,
    model,
    data
  )
  expect_identical(result@value, 25)
})

test_that("recommend-NextBestNCRM-DataParts returns expected values of the objects (no doselimit)", {
  data <- h_get_data_parts(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(
    nb_ncrm,
    Inf,
    samples,
    model,
    data
  )
  expect_identical(result@value, 75)
})

test_that("recommend-NextBestNCRM-DataParts returns expected value for all parts 1", {
  data <- h_get_data_parts_1(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(nb_ncrm, 45, samples, model, data)
  expect_identical(result@value, 45)
})

test_that("recommend-NextBestNCRM-DataParts throws the error for all parts 1 and no doselimit", {
  data <- h_get_data_parts_1(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  expect_error(
    recommend(nb_ncrm, Inf, samples, model, data),
    "A finite doselimit needs to be specified for Part I."
  )
})

## NextBestNCRMLoss ----

test_that("nextBest-NextBestNCRMLoss returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.999,
    losses = c(1, 0, 2)
  )

  result <- recommend(
    nb_ncrm_loss,
    60,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  expect_identical(result@value, 25)
})

test_that("recommend-NextBestNCRMLoss returns expected values of the objects (loss function of 4 elements)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 0.6),
    unacceptable = c(0.6, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 1, 2)
  )

  result <- recommend(
    nb_ncrm_loss,
    Inf,
    samples,
    model,
    data
  )
  expect_identical(result@value, 25)
})

test_that("recommend-NextBestNCRMLoss returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25,
    losses = c(1, 0, 2)
  )

  result <- recommend(
    nb_ncrm_loss,
    Inf,
    samples,
    model,
    data,
    prob_plot_type = "bar"
  )
  expect_identical(result@value, 25)
})

## NextBestThreePlusThree ----

test_that("recommend-NextBestThreePlusThree returns expected values (< 33% and escalated)", {
  data <- h_get_data(placebo = FALSE)

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 125)
  # expect_identical(result$stopHere, setNames(FALSE, 125))
})

test_that("recommend-NextBestThreePlusThree returns expected values (< 33%, max dose, no escalation)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = data@doseGrid[data@nGrid], y = c(0L, 1L, 0L, 0L))

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 300)
  # expect_identical(result$stopHere, setNames(TRUE, 300))
})

test_that("recommend-NextBestThreePlusThree returns expected values (< 33% and no escalation)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(
    data,
    x = data@doseGrid[tail(data@xLevel, 1) - 1],
    y = c(0L, 1L, 0L, 0L)
  )

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 75)
  # expect_identical(result$stopHere, setNames(TRUE, 75))
})

test_that("recommend-NextBestThreePlusThree returns expected values (> 33%)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = 175, y = 1L)

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 150)
  # expect_identical(result$stopHere, setNames(FALSE, 150))
})

test_that("recommend-NextBestThreePlusThree returns expected values (== 33%, 3 patients at last_lev)", {
  data <- h_get_data()
  data <- update(data, x = 200, y = c(1L, 0L, 0L))

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 200)
  # expect_identical(result$stopHere, setNames(FALSE, 200))
})

test_that("recommend-NextBestThreePlusThree returns expected values (== 33%, 6 patients at last_lev)", {
  data <- h_get_data()
  data <- update(data, x = 200, y = c(0L, 0L, 1L, 0L, 1L, 0L))

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, 175)
  # expect_identical(result$stopHere, setNames(FALSE, 175))
})

test_that("recommend-NextBestThreePlusThree returns expected values (next_level == 0)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = data@doseGrid[1], y = c(1L, 1L))

  result <- recommend(NextBestThreePlusThree(), data = data)
  expect_identical(result@value, NA_real_)
  # expect_identical(result$stopHere, TRUE)
})

## NextBestMinDist ----

test_that("recommend-NextBestMinDist returns expected values and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- recommend(nb_md, 50, samples, model, data)
  expect_identical(result@value, 50)
  # expect_snap(result$probs)
})

test_that("recommend-NextBestMinDist returns expected values and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-0.38, -0.13, 1.43, 2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_md <- NextBestMinDist(target = 0.1)

  result <- recommend(nb_md, 40, samples, model, data)
  expect_identical(result@value, 25)
  # expect_snap(result$probs)
})

test_that("recommend-NextBestMinDist returns expected values and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- recommend(nb_md, Inf, samples, model, data)
  expect_identical(result@value, 75)
  # expect_snap(result$probs)
})

## NextBestInfTheory ----

test_that("recommend-NextBestInfTheory returns correct next dose", {
  data <- h_get_data(placebo = FALSE)
  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.06, 0.85, 0.85, 2.72), nrow = 2)
  )
  samples <- h_as_samples(list(
    alpha0 = c(0, -1, 1, 2),
    alpha1 = c(0, 2, 1, -1)
  ))
  nb_it <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)

  result <- recommend(nb_it, 75, samples, model, data)
  expect_identical(result@value, 25)
})

test_that("recommend-NextBestInfTheory returns correct next dose (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.06, 0.85, 0.85, 2.72), nrow = 2)
  )
  samples <- h_as_samples(list(
    alpha0 = c(0, -1, 1, 2),
    alpha1 = c(0, 2, 1, -1)
  ))
  nb_it <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)

  result <- recommend(nb_it, Inf, samples, model, data)
  # expect_identical(result, list(value = 25))
  expect_identical(result@value, 25)
})

## NextBestTDsamples ----

test_that("recommend-NextBestTDsamples returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(
        -6.99,
        -6.99,
        -8.58,
        -8.62,
        -8.62,
        -8.62,
        -8.62,
        -8.23,
        -8.71,
        -8.71
      ),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples()

  result <- recommend(nb_tds, 90, samples, model, data)
  expect_identical(result@value, c(75, 75))
})

test_that("recommend-NextBestTDsamples returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(
        -6.99,
        -6.99,
        -8.58,
        -8.62,
        -8.62,
        -8.62,
        -8.62,
        -8.23,
        -8.71,
        -8.71
      ),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples()

  result <- recommend(nb_tds, Inf, samples, model, data)
  expect_identical(result@value, c(100, 100))
})

test_that("recommend-NextBestTDsamples returns expected values of the objects (other targets)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(
        -6.99,
        -6.99,
        -8.58,
        -8.62,
        -8.62,
        -8.62,
        -8.62,
        -8.23,
        -8.71,
        -8.71
      ),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples(0.6, 0.55, 0.45)

  result <- recommend(nb_tds, 150, samples, model, data)
  expect_identical(result@value, c(150, 150))
})

## NextBestTD ----

test_that("recommend-NextBestTD returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- recommend(nb_td, 70, model = model, data = data)
  expect_identical(result@value, c(50, 50))
})

test_that("recommend-NextBestTD returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- recommend(nb_td, Inf, model = model, data = data)
  # expected <- list(
  #   next_dose_drt = 75,
  #   prob_target_drt = 0.45,
  #   dose_target_drt = 75.82941,
  #   next_dose_eot = 50,
  #   prob_target_eot = 0.4,
  #   dose_target_eot = 63.21009,
  #   ci_dose_target_eot = c(20.38729, 195.98072),
  #   ci_ratio_dose_target_eot = 9.612886
  # )
  expect_identical(result@value, c(75, 50))
})

test_that("recommend-NextBestTD returns expected values of the objects (other targets)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.25, prob_target_eot = 0.2)

  result <- recommend(nb_td, 70, model = model, data = data)
  expect_identical(result@value, c(25, 25))
})

## NextBestMaxGain ----

test_that("recommend-NextBestMaxGain returns expected values of the objects", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

  result <- recommend(
    nb_mg,
    49,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )
  expect_identical(result@value, c(25, 25, 83.964693), tolerance = 1e-06)
})

test_that("recommend-NextBestDualEndpoint selects among eligible doses", {

  data <- DataDual(
    x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
    y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
    ID = 1:17,
    cohort = c(1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.55, 0.6, 0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(10, 80, by = 10))
  )

  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = diag(2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )

  options <- McmcOptions(
    burnin = 5,
    step = 1,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 123
  )
  samples <- mcmc(data, model, options)

  next_best <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25,
    target_thresh = 0.9999999999999998
  )

  result <- recommend(next_best, Inf, samples, model, data)

  expect_identical(names(result@details), c("Dose", "Target", "Overdose"))

  eligible_doses <- result@details$Dose[result@details$Overdose < 0.25]
  expect_gt(length(eligible_doses), 0)
  expect_identical(result@value, max(eligible_doses))

  no_eligible <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 1e-06
  )

  result_none <- recommend(no_eligible, Inf, samples, model, data)
  expect_true(is.na(result_none@value))

  absolute_target <- NextBestDualEndpoint(
    target = c(0.2, 0.8),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25,
    target_relative = FALSE
  )

  result_absolute <- recommend(
    absolute_target,
    Inf,
    samples,
    model,
    data
  )

  expect_identical(
    names(result_absolute@details),
    c("Dose", "Target", "Overdose")
  )
  expect_true(all(
    result_absolute@details$Target >= 0 &
      result_absolute@details$Target <= 1
  ))
})

test_that("recommend-NextBestMaxGain returns expected values of the objects (no doselimit)", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

  result <- recommend(
    nb_mg,
    Inf,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )

  expect_identical(result@value, c(50, 25, 83.964693), tolerance = 1e-06)
})

test_that("recommend-NextBestMaxGain returns expected values of the objects (other targets, placebo)", {
  data <- h_get_data_dual(placebo = TRUE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- recommend(
    nb_mg,
    150,
    model = model_dlt,
    data = data,
    model_eff = model_eff
  )
  # expected <- list(
  #   next_dose = 75,
  #   prob_target_drt = 0.45,
  #   dose_target_drt = 75.82941,
  #   next_dose_drt = 75,
  #   prob_target_eot = 0.4,
  #   dose_target_eot = 63.21009,
  #   next_dose_eot = 50,
  #   dose_max_gain = 83.96469,
  #   next_dose_max_gain = 75,
  #   ci_dose_target_eot = c(20.38729, 195.98072),
  #   ci_ratio_dose_target_eot = 9.612886,
  #   ci_dose_max_gain = c(26.95037, 293.67744),
  #   ci_ratio_dose_max_gain = 10.89697
  # )
  expect_identical(result@value, c(75, 50, 83.964693), tolerance = 1e-06)
})

## NextBestMaxGainSamples ----

test_that("recommend-NextBestMaxGainSamples returns expected values of the objects", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  samples_dlt <- h_as_samples(
    list(phi1 = c(-4.03, -4.48, -4.07, -4.37), phi2 = c(1.45, 0.86, 0.56, 0.42))
  )
  samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42),
      theta2 = c(3.41, 0.61, 0.58, 1.35),
      nu = c(2.14, 4.63, 0.83, 2.98)
    )
  )
  nb_mgs <- h_next_best_mgsamples()

  result <- recommend(
    nb_mgs,
    49,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
  # expected <- list(
  #   next_dose = 25,
  #   prob_target_drt = 0.45,
  #   dose_target_drt = 131.8022,
  #   next_dose_drt = 25,
  #   prob_target_eot = 0.4,
  #   dose_target_eot = 103.9855,
  #   next_dose_eot = 25,
  #   dose_max_gain = 125,
  #   next_dose_max_gain = 25,
  #   ci_dose_target_eot = c(103.9855, 103.9855),
  #   ci_ratio_dose_target_eot = 1,
  #   ci_dose_max_gain = c(30.625, 288.750),
  #   ci_ratio_dose_max_gain = 9.428571
  # )
  expect_identical(result@value, c(25, 25, 125))
})

test_that("recommend-NextBestMaxGainSamples returns expected values of the objects (no doselimit)", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  samples_dlt <- h_as_samples(
    list(phi1 = c(-4.03, -4.48, -4.07, -4.37), phi2 = c(1.45, 0.86, 0.56, 0.42))
  )
  samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42),
      theta2 = c(3.41, 0.61, 0.58, 1.35),
      nu = c(2.14, 4.63, 0.83, 2.98)
    )
  )
  nb_mgs <- h_next_best_mgsamples()

  result <- recommend(
    nb_mgs,
    Inf,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
  # expected <- list(
  #   next_dose = 125,
  #   prob_target_drt = 0.45,
  #   dose_target_drt = 131.8022,
  #   next_dose_drt = 125,
  #   prob_target_eot = 0.4,
  #   dose_target_eot = 103.9855,
  #   next_dose_eot = 100,
  #   dose_max_gain = 125,
  #   next_dose_max_gain = 125,
  #   ci_dose_target_eot = c(103.9855, 103.9855),
  #   ci_ratio_dose_target_eot = 1,
  #   ci_dose_max_gain = c(30.625, 288.750),
  #   ci_ratio_dose_max_gain = 9.428571
  # )
  expect_identical(result@value, c(125, 100, 125))
})

test_that("recommend-NextBestMaxGainSamples returns expected values of the objects (other targets, placebo)", {
  data <- h_get_data_dual(placebo = TRUE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  samples_dlt <- h_as_samples(
    list(
      phi1 = c(-4.03, -4.48, -4.07, -4.37, -4.5),
      phi2 = c(1.45, 0.86, 0.56, 0.42, 0.6)
    )
  )
  samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42, -1.5),
      theta2 = c(3.41, 0.61, 0.58, 1.35, 2),
      nu = c(2.14, 4.63, 0.83, 2.98, 1.6)
    )
  )
  nb_mgs <- h_next_best_mgsamples(td = 0.5, te = 0.45, p = 0.25, p_gstar = 0.3)

  result <- recommend(
    nb_mgs,
    60,
    samples_dlt,
    model_dlt,
    data,
    model_eff,
    samples_eff
  )
  # expected <- list(
  #   next_dose = 50,
  #   prob_target_drt = 0.5,
  #   dose_target_drt = 182.9664,
  #   next_dose_drt = 50,
  #   prob_target_eot = 0.45,
  #   dose_target_eot = 144.8885,
  #   next_dose_eot = 50,
  #   dose_max_gain = 110,
  #   next_dose_max_gain = 50,
  #   ci_dose_target_eot = c(144.8885, 144.8885),
  #   ci_ratio_dose_target_eot = 1,
  #   ci_dose_max_gain = c(32.5, 300.0),
  #   ci_ratio_dose_max_gain = 9.230769
  # )
  expect_identical(result@value, c(50, 50, 110), tolerance = 1e-06)
})

## NextBestProbMTDLTE ----

test_that("recommend-NextBestProbMTDLTE returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- recommend(nb_prob_mtd, 90, samples, model, data)
  expect_identical(result@value, 75)
  # expect_snap(result$allocation)
})

test_that("recommend-NextBestProbMTDLTE returns correct next dose and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-0.38, -0.13, 1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- recommend(nb_prob_mtd, 40, samples, model, data)
  expect_identical(result@value, 25)
  # expect_snap(result$allocation)  expect_doppel("Plot of nextBest-NextBestProbMTDLTE with placebo", result$plot)
})

test_that("recommend-NextBestProbMTDLTE returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- recommend(nb_prob_mtd, Inf, samples, model, data)
  expect_identical(result@value, 125)
  # expect_snap(result$allocation)
})

## NextBestProbMTDMinDist ----

test_that("recommend-NextBestProbMTDMinDist returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- recommend(nb_prob_mtd, 90, samples, model, data)
  expect_identical(result@value, 75)
  # expect_snap(result$allocation)
})

test_that("recommend-NextBestProbMTDMinDist returns correct next dose and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(-0.38, -0.13, 1.43, 2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- recommend(nb_prob_mtd, 40, samples, model, data)
  expect_identical(result@value, 25)
  # expect_snap(result$allocation)
})

test_that("recommend-NextBestProbMTDMinDist returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = c(2.38, -2.13, -1.43, -2.57),
      alpha1 = c(1.67, 1.3, 1.77, 2.51)
    )
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- recommend(nb_prob_mtd, Inf, samples, model, data)
  expect_identical(result@value, 25)
  # expect_snap(result$allocation)
})

## NextBestOrdinal ----
test_that("recommend-NextBestOrdinal throws exception when passed a GeneralModel object", {
  ordinal_data <- .DefaultDataOrdinal()
  ordinal_model <- .DefaultLogisticLogNormalOrdinal()
  samples <- mcmc(ordinal_data, ordinal_model, .DefaultMcmcOptions())

  next_best <- .DefaultNextBestOrdinal()
  bad_data <- .DefaultData()
  expect_error(
    recommend(next_best, Inf, samples, ordinal_model, bad_data),
    paste0(
      "NextBestOrdinal objects can only be used with LogisticLogNormalOrdinal ",
      "models and DataOrdinal data objects. In this case, the model is a ",
      "'LogisticLogNormalOrdinal' object and the data is in a Data object."
    )
  )
})

test_that("recommend-NextBestOrdinal works correctly", {
  ordinal_data <- .DefaultDataOrdinal()
  ordinal_model <- .DefaultLogisticLogNormalOrdinal()
  samples <- mcmc(
    ordinal_data,
    ordinal_model,
    McmcOptions(
      rng_kind = "Mersenne-Twister",
      rng_seed = 215614
    )
  )
  next_best <- .DefaultNextBestOrdinal()
  bad_data <- .DefaultData()

  actual <- recommend(next_best, Inf, samples, ordinal_model, ordinal_data)
  expect_equal(actual@value, 50)
})

# Tests wriotten by ChatGPT 07Nov26

test_that("recommend validates matrix dose limits", {
  expect_error(
    recommend(
      nextBest = NULL,
      doselimit = matrix(1:3, ncol = 3),
      samples = NULL,
      model = NULL,
      data = NULL
    ),
    "but has 3 cols"
  )

  expect_error(
    recommend(
      nextBest = NULL,
      doselimit = matrix(c("a", "b"), ncol = 2),
      samples = NULL,
      model = NULL,
      data = NULL
    ),
    "numeric"
  )
})

test_that("recommend rejects unsupported dose limit objects", {
  expect_error(
    recommend(
      nextBest = NULL,
      doselimit = data.frame(x = 1, y = 2),
      samples = NULL,
      model = NULL,
      data = NULL
    ),
    "doselimit must be either a number or a matrix."
  )
})

test_that("recommend-NextBestEWOC chooses the highest eligible dose", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = rep(-10, 4),
      alpha1 = rep(0.1, 4)
    )
  )

  nb_ewoc <- NextBestEWOC(
    target = 0.30,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(nb_ewoc, Inf, samples, model, data)

  expect_identical(result@value, max(data@doseGrid))
})

test_that("tidy returns the value of a Recommendation", {
  result <- tidy(Recommendation(value = 75))

  expect_identical(result$Value, 75)
})

test_that("tidy marks the recommended dose in RecommendationProbs", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(
      alpha0 = rep(-10, 4),
      alpha1 = rep(0.1, 4)
    )
  )
  nb_ewoc <- NextBestEWOC(
    target = 0.30,
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  recommendation <- recommend(nb_ewoc, Inf, samples, model, data)
  result <- tidy(recommendation)

  expect_identical(
    result$Recommended,
    recommendation@details$Dose == recommendation@value
  )
})

test_that("recommend-NextBestDualEndpoint handles Emax samples", {
  data <- h_get_data_dual(placebo = FALSE)

  model <- DualEndpointEmax(
    mean = c(0, 1),
    cov = diag(2),
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    E0 = c(0, 100),
    Emax = c(0, 500),
    ED50 = c(10, 200),
    ref_dose_emax = 1000
  )

  samples <- mcmc(
    data,
    model,
    McmcOptions(
      burnin = 5,
      step = 1,
      samples = 1000,
      rng_kind = "Mersenne-Twister",
      rng_seed = 123
    )
  )

  expect_true("Emax" %in% names(samples))

  next_best <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  result <- recommend(next_best, Inf, samples, model, data)

  expect_identical(names(result@details), c("Dose", "Target", "Overdose"))
  expect_true(all(result@details$Target >= 0 & result@details$Target <= 1))
})
