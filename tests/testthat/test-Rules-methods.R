# nextBest ----

## NextBestMTD ----

test_that("nextBest-NextBestMTD returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  result <- nextBest(nb_mtd, 90, samples, model, data)
  expect_identical(result$value, 75)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMTD", result$plot)
})

test_that("nextBest-NextBestMTD returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_mtd <- NextBestMTD(
    target = 0.33,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )

  result <- nextBest(nb_mtd, Inf, samples, model, data)
  expect_identical(result$value, 100)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMTD without doselimit", result$plot)
})

## NextBestNCRM ----

test_that("nextBest-NextBestNCRM returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, 45, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM", result$plot)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM_p1", result$singlePlots$plot1)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM_p2", result$singlePlots$plot2)
})

test_that("nextBest-NextBestNCRM returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, Inf, samples, model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM without doselimit", result$plot)
})

## NextBestNCRM-DataParts ----

test_that("nextBest-NextBestNCRM-DataParts returns expected values of the objects", {
  data <- h_get_data_parts(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, 45, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM-DataParts", result$plot)
})

test_that("nextBest-NextBestNCRM-DataParts returns expected values of the objects (no doselimit)", {
  data <- h_get_data_parts(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, Inf, samples, model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRM-DataParts nodlim", result$plot)
})

test_that("nextBest-NextBestNCRM-DataParts returns expected value for all parts 1", {
  data <- h_get_data_parts_1(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  result <- nextBest(nb_ncrm, 45, samples, model, data)
  expect_identical(result$value, 45)
  expect_null(result$plot)
})

test_that("nextBest-NextBestNCRM-DataParts throws the error for all parts 1 and no doselimit", {
  data <- h_get_data_parts_1(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm <- NextBestNCRM(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25
  )

  expect_error(
    nextBest(nb_ncrm, Inf, samples, model, data),
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

  result <- nextBest(nb_ncrm_loss, 60, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss", result$plot_joint)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss_p1", result$plots_single$plot1)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss_p2", result$plots_single$plot2)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss_ploss", result$plots_single$plot_loss)
})

test_that("nextBest-NextBestNCRMLoss returns expected values of the objects (loss function of 4 elements)", {
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

  result <- nextBest(nb_ncrm_loss, Inf, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss with losses of 4", result$plot_joint)
})

test_that("nextBest-NextBestNCRMLoss returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_ncrm_loss <- NextBestNCRMLoss(
    target = c(0.2, 0.35), overdose = c(0.35, 1), max_overdose_prob = 0.25, losses = c(1, 0, 2)
  )

  result <- nextBest(nb_ncrm_loss, Inf, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestNCRMLoss without doselimit", result$plot_joint)
})

## NextBestThreePlusThree ----

test_that("nextBest-NextBestThreePlusThree returns expected values (< 33% and escalated)", {
  data <- h_get_data(placebo = FALSE)

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 125)
  expect_identical(result$stopHere, setNames(FALSE, 125))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (< 33%, max dose, no escalation)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = data@doseGrid[data@nGrid], y = c(0L, 1L, 0L, 0L))

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 300)
  expect_identical(result$stopHere, setNames(TRUE, 300))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (< 33% and no escalation)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = data@doseGrid[tail(data@xLevel, 1) - 1], y = c(0L, 1L, 0L, 0L))

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 75)
  expect_identical(result$stopHere, setNames(TRUE, 75))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (> 33%)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = 175, y = 1L)

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 150)
  expect_identical(result$stopHere, setNames(FALSE, 150))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (== 33%, 3 patients at last_lev)", {
  data <- h_get_data()
  data <- update(data, x = 200, y = c(1L, 0L, 0L))

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 200)
  expect_identical(result$stopHere, setNames(FALSE, 200))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (== 33%, 6 patients at last_lev)", {
  data <- h_get_data()
  data <- update(data, x = 200, y = c(0L, 0L, 1L, 0L, 1L, 0L))

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, 175)
  expect_identical(result$stopHere, setNames(FALSE, 175))
})

test_that("nextBest-NextBestThreePlusThree returns expected values (next_level == 0)", {
  data <- h_get_data(placebo = FALSE)
  data <- update(data, x = data@doseGrid[1], y = c(1L, 1L))

  result <- nextBest(NextBestThreePlusThree(), data = data)
  expect_identical(result$value, NA)
  expect_identical(result$stopHere, TRUE)
})

## NextBestDualEndpoint ----

test_that("nextBest-NextBestDualEndpoint returns expected elements", {
  data <- h_get_data_dual(placebo = FALSE)
  model <- h_get_dual_endpoint_rw()
  samples <- h_samples_dual_endpoint_rw()
  nb_de <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.45, 1),
    max_overdose_prob = 0.25
  )

  result <- nextBest(nb_de, 133, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint", result$plot)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_p1", result$singlePlots$plot1)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_p2", result$singlePlots$plot2)
})

test_that("nextBest-NextBestDualEndpoint returns expected elements (with Emax param)", {
  data <- h_get_data_dual(placebo = FALSE)
  model <- h_get_dual_endpoint_beta(fixed = FALSE)
  samples <- h_samples_dual_endpoint_beta(fixed = FALSE)
  nb_de <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.45, 1),
    max_overdose_prob = 0.25
  )

  result <- nextBest(nb_de, 133, samples, model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_Emax", result$plot)
})

test_that("nextBest-NextBestDualEndpoint returns expected elements (absolute target)", {
  data <- h_get_data_dual(placebo = FALSE)
  model <- h_get_dual_endpoint_rw()
  samples <- h_samples_dual_endpoint_rw()
  nb_de <- NextBestDualEndpoint(
    target = c(0.9, 1),
    target_relative = FALSE,
    overdose = c(0.65, 1),
    max_overdose_prob = 0.55
  )

  result <- nextBest(nb_de, 90, samples, model, data)
  expect_identical(result$value, 75)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_abstarget", result$plot)
})

test_that("nextBest-NextBestDualEndpoint returns expected elements (absolute target, no doselimit)", {
  data <- h_get_data_dual(placebo = FALSE)
  model <- h_get_dual_endpoint_rw()
  samples <- h_samples_dual_endpoint_rw()
  nb_de <- NextBestDualEndpoint(
    target = c(0.9, 1),
    target_relative = FALSE,
    overdose = c(0.65, 1),
    max_overdose_prob = 0.55
  )

  result <- nextBest(nb_de, Inf, samples, model, data)
  expect_identical(result$value, 100)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_atgt_nodlim", result$plot)
})

## NextBestMinDist ----

test_that("nextBest-NextBestMinDist returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- nextBest(nb_md, 50, samples, model, data)
  expect_identical(result, list(value = 50))
})

test_that("nextBest-NextBestMinDist returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- nextBest(nb_md, Inf, samples, model, data)
  expect_identical(result, list(value = 75))
})

test_that("nextBest-NextBestMinDist returns expected values of the objects (no doselimit, target = 0.7)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.7)

  result <- nextBest(nb_md, Inf, samples, model, data)
  expect_identical(result, list(value = 175))
})

## NextBestInfTheory ----

test_that("nextBest-NextBestInfTheory returns correct next dose", {
  data <- h_get_data(placebo = FALSE)
  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.06, 0.85, 0.85, 2.72), nrow = 2)
  )
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  nb_it <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)

  result <- nextBest(nb_it, 75, samples, model, data)
  expect_identical(result, list(value = 25))
})

test_that("nextBest-NextBestInfTheory returns correct next dose (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  # Set up the model; sigma0 = 1.0278, sigma1 = 1.65, rho = 0.5.
  model <- LogisticLogNormal(
    mean = c(-4.47, 0.0033),
    cov = matrix(c(1.06, 0.85, 0.85, 2.72), nrow = 2)
  )
  samples <- h_as_samples(list(alpha0 = c(0, -1, 1, 2), alpha1 = c(0, 2, 1, -1)))
  nb_it <- NextBestInfTheory(target = 0.25, asymmetry = 0.1)

  result <- nextBest(nb_it, Inf, samples, model, data)
  expect_identical(result, list(value = 25))
})

## NextBestTDsamples ----

test_that("nextBest-NextBestTDsamples returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(-6.99, -6.99, -8.58, -8.62, -8.62, -8.62, -8.62, -8.23, -8.71, -8.71),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples()

  result <- nextBest(nb_tds, 90, samples, model, data)
  expected <- list(
    next_dose_drt = 75,
    prob_target_drt = 0.45,
    dose_target_drt = 120.4065,
    next_dose_eot = 75,
    prob_target_eot = 0.4,
    dose_target_eot = 107.1014,
    ci_dose_target_eot = c(49.21382, 535.88506),
    ci_ratio_dose_target_eot = 10.88891
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestTDsamples", result$plot)
})

test_that("nextBest-NextBestTDsamples returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(-6.99, -6.99, -8.58, -8.62, -8.62, -8.62, -8.62, -8.23, -8.71, -8.71),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples()

  result <- nextBest(nb_tds, Inf, samples, model, data)
  expected <- list(
    next_dose_drt = 100,
    prob_target_drt = 0.45,
    dose_target_drt = 120.4065,
    next_dose_eot = 100,
    prob_target_eot = 0.4,
    dose_target_eot = 107.1014,
    ci_dose_target_eot = c(49.21382, 535.88506),
    ci_ratio_dose_target_eot = 10.88891
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestTDsamples_nodoselim", result$plot)
})

test_that("nextBest-NextBestTDsamples returns expected values of the objects (other targets)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  samples <- h_as_samples(
    list(
      phi1 = c(-6.99, -6.99, -8.58, -8.62, -8.62, -8.62, -8.62, -8.23, -8.71, -8.71),
      phi2 = c(1.69, 1.69, 1.26, 1.72, 1.72, 1.72, 1.72, 1.78, 1.74, 1.74)
    )
  )
  nb_tds <- h_next_best_tdsamples(0.6, 0.55, 0.45)

  result <- nextBest(nb_tds, 150, samples, model, data)
  expected <- list(
    next_dose_drt = 150,
    prob_target_drt = 0.6,
    dose_target_drt = 188.52,
    next_dose_eot = 150,
    prob_target_eot = 0.55,
    dose_target_eot = 167.5761,
    ci_dose_target_eot = c(70.44517, 861.73632),
    ci_ratio_dose_target_eot = 12.23272
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
})

## NextBestTD ----

test_that("nextBest-NextBestTD returns expected values of the objects", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- nextBest(nb_td, 70, model = model, data = data)
  expected <- list(
    next_dose_drt = 50,
    prob_target_drt = 0.45,
    dose_target_drt = 75.82941,
    next_dose_eot = 50,
    prob_target_eot = 0.4,
    dose_target_eot = 63.21009,
    ci_dose_target_eot = c(20.38729, 195.98072),
    ci_ratio_dose_target_eot = 9.612886
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestTD", result$plot)
})

test_that("nextBest-NextBestTD returns expected values of the objects (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- nextBest(nb_td, Inf, model = model, data = data)
  expected <- list(
    next_dose_drt = 75,
    prob_target_drt = 0.45,
    dose_target_drt = 75.82941,
    next_dose_eot = 50,
    prob_target_eot = 0.4,
    dose_target_eot = 63.21009,
    ci_dose_target_eot = c(20.38729, 195.98072),
    ci_ratio_dose_target_eot = 9.612886
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestTD_nodoselim", result$plot)
})

test_that("nextBest-NextBestTD returns expected values of the objects (other targets)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_indep_beta()
  nb_td <- NextBestTD(prob_target_drt = 0.25, prob_target_eot = 0.2)

  result <- nextBest(nb_td, 70, model = model, data = data)
  expected <- list(
    next_dose_drt = 25,
    prob_target_drt = 0.25,
    dose_target_drt = 34.13734,
    next_dose_eot = 25,
    prob_target_eot = 0.2,
    dose_target_eot = 26.43526,
    ci_dose_target_eot = c(4.628141, 150.994299),
    ci_ratio_dose_target_eot = 32.62526
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
})

## NextBestMaxGain ----

test_that("nextBest-NextBestMaxGain returns expected values of the objects", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

  result <- nextBest(nb_mg, 49, model = model_dlt, data = data, model_eff = model_eff)
  expected <- list(
    next_dose = 25,
    prob_target_drt = 0.35,
    dose_target_drt = 52.28128,
    next_dose_drt = 25,
    prob_target_eot = 0.3,
    dose_target_eot = 42.68131,
    next_dose_eot = 25,
    dose_max_gain = 83.96469,
    next_dose_max_gain = 25,
    ci_dose_target_eot = c(11.06619, 164.61798),
    ci_ratio_dose_target_eot = 14.87575,
    ci_dose_max_gain = c(23.09875, 305.21431),
    ci_ratio_dose_max_gain = 13.21345
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMaxGain", result$plot)
})

test_that("nextBest-NextBestMaxGain returns expected values of the objects (no doselimit)", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

  result <- nextBest(nb_mg, Inf, model = model_dlt, data = data, model_eff = model_eff)
  expected <- list(
    next_dose = 50,
    prob_target_drt = 0.35,
    dose_target_drt = 52.28128,
    next_dose_drt = 50,
    prob_target_eot = 0.3,
    dose_target_eot = 42.68131,
    next_dose_eot = 25,
    dose_max_gain = 83.96469,
    next_dose_max_gain = 75,
    ci_dose_target_eot = c(11.06619, 164.61798),
    ci_ratio_dose_target_eot = 14.87575,
    ci_dose_max_gain = c(23.09875, 305.21431),
    ci_ratio_dose_max_gain = 13.21345
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMaxGain_nodoselim", result$plot)
})

test_that("nextBest-NextBestMaxGain returns expected values of the objects (other targets, placebo)", {
  data <- h_get_data_dual(placebo = TRUE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  nb_mg <- NextBestMaxGain(prob_target_drt = 0.45, prob_target_eot = 0.4)

  result <- nextBest(nb_mg, 150, model = model_dlt, data = data, model_eff = model_eff)
  expected <- list(
    next_dose = 75,
    prob_target_drt = 0.45,
    dose_target_drt = 75.82941,
    next_dose_drt = 75,
    prob_target_eot = 0.4,
    dose_target_eot = 63.21009,
    next_dose_eot = 50,
    dose_max_gain = 83.96469,
    next_dose_max_gain = 75,
    ci_dose_target_eot = c(20.38729, 195.98072),
    ci_ratio_dose_target_eot = 9.612886,
    ci_dose_max_gain = c(26.95037, 293.67744),
    ci_ratio_dose_max_gain = 10.89697
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
})

## NextBestMaxGainSamples ----

test_that("nextBest-NextBestMaxGainSamples returns expected values of the objects", {
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

  result <- nextBest(nb_mgs, 49, samples_dlt, model_dlt, data, model_eff, samples_eff)
  expected <- list(
    next_dose = 25,
    prob_target_drt = 0.45,
    dose_target_drt = 131.8022,
    next_dose_drt = 25,
    prob_target_eot = 0.4,
    dose_target_eot = 103.9855,
    next_dose_eot = 25,
    dose_max_gain = 125,
    next_dose_max_gain = 25,
    ci_dose_target_eot = c(103.9855, 103.9855),
    ci_ratio_dose_target_eot = 1,
    ci_dose_max_gain = c(30.625, 288.750),
    ci_ratio_dose_max_gain = 9.428571
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMaxGainSamples", result$plot)
})

test_that("nextBest-NextBestMaxGainSamples returns expected values of the objects (no doselimit)", {
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

  result <- nextBest(nb_mgs, Inf, samples_dlt, model_dlt, data, model_eff, samples_eff)
  expected <- list(
    next_dose = 125,
    prob_target_drt = 0.45,
    dose_target_drt = 131.8022,
    next_dose_drt = 125,
    prob_target_eot = 0.4,
    dose_target_eot = 103.9855,
    next_dose_eot = 100,
    dose_max_gain = 125,
    next_dose_max_gain = 125,
    ci_dose_target_eot = c(103.9855, 103.9855),
    ci_ratio_dose_target_eot = 1,
    ci_dose_max_gain = c(30.625, 288.750),
    ci_ratio_dose_max_gain = 9.428571
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMaxGainSamples_nodoselim", result$plot)
})

test_that("nextBest-NextBestMaxGainSamples returns expected values of the objects (other targets, placebo)", {
  data <- h_get_data_dual(placebo = TRUE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)
  samples_dlt <- h_as_samples(
    list(phi1 = c(-4.03, -4.48, -4.07, -4.37, -4.5), phi2 = c(1.45, 0.86, 0.56, 0.42, 0.6))
  )
  samples_eff <- h_as_samples(
    list(
      theta1 = c(-2.93, -0.54, 0.01, -2.42, -1.5),
      theta2 = c(3.41, 0.61, 0.58, 1.35, 2),
      nu = c(2.14, 4.63, 0.83, 2.98, 1.6)
    )
  )
  nb_mgs <- h_next_best_mgsamples(td = 0.5, te = 0.45, p = 0.25, p_gstar = 0.3)

  result <- nextBest(nb_mgs, 60, samples_dlt, model_dlt, data, model_eff, samples_eff)
  expected <- list(
    next_dose = 50,
    prob_target_drt = 0.5,
    dose_target_drt = 182.9664,
    next_dose_drt = 50,
    prob_target_eot = 0.45,
    dose_target_eot = 144.8885,
    next_dose_eot = 50,
    dose_max_gain = 110,
    next_dose_max_gain = 50,
    ci_dose_target_eot = c(144.8885, 144.8885),
    ci_ratio_dose_target_eot = 1,
    ci_dose_max_gain = c(32.5, 300.0),
    ci_ratio_dose_max_gain = 9.230769
  )
  expect_identical(result[names(expected)], expected, tolerance = 10e-7)
})

# maxDose-IncrementsNumDoseLevels ----

test_that("IncrementsNumDoseLevels works correctly if basislevel 'last' is defined", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2,
    basisLevel = "last"
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel='last'.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel is not defined and default is used", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel not defined, then reference value is used.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel 'max' is defined", {
  increments <- IncrementsNumDoseLevels(
    maxLevels = 2,
    basisLevel = "max"
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 20) # maxDose is 20 if basislevel='max'.
})

# maxDose-IncrementsRelativeDLTCurrent ----

test_that("IncrementsRelativeDLTCurrent works correctly", {
  increments <- IncrementsRelativeDLTCurrent(
    DLTintervals = c(0, 1, 3),
    increments = c(1, 0.33, 0.2)
  )
  result <- maxDose(
    increments,
    data = h_get_data_1()
  )
  expect_equal(result, 13.3) # maxDose is 13.3 because last dose was 10 with 1 DLT.
})

# maxDose-IncrementsHSRBeta ----

test_that("IncrementsHSRBeta works correctly if toxcicity probability is below threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 300) # maxDose is 300 as toxicity probability of no dose is above 0.95.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.9)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability"
), {
  my_data <- h_get_data()
  my_data@y[my_data@cohort == 1L] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability of placebo is above threshold probability", {
  my_data <- h_get_data()
  my_data@y[my_data@x == 0.001] <- c(1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 300) # maxDose is 300 as placebo is ignored.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability and placebo == T, but not appplied"
), {
  my_data <- h_get_data()
  my_data@x <- c(rep(25, 4), rep(50, 4), rep(100, 4))
  my_data@y[my_data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability (no placebo)"
), {
  my_data <- h_get_data(placebo = FALSE)
  my_data@y[my_data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.90.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability (no placebo)", {
  my_data <- h_get_data(placebo = FALSE)
  my_data@y[my_data@cohort == 3] <- c(0L, 1L, 1L, 1L)
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  result <- maxDose(
    increments,
    data = my_data
  )
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})


# stopTrial-StoppingMTDCV ----

test_that("StoppingMTDCV works correctly if CV is below threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 50)
  result <- stopTrial(
    stopping = stopping,
    dose = 7,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = "CV of MTD is 40 % and thus below the required precision threshold of 50 %"
  )
  expect_identical(result, expected) # CV is 23% < 30%.
})

test_that("StoppingMTDCV works correctly if CV is above threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 20)
  result <- stopTrial(
    stopping = stopping,
    dose = 7,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "CV of MTD is 40 % and thus above the required precision threshold of 20 %"
  )
  expect_identical(result, expected) # CV is 23% > 20%.
})

# stopTrial-StoppingLowestDoseHSRBeta ----

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.9)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic",
      "based on posterior Beta distribution using a Beta(1,1) prior",
      "is 24% and thus below the required 90% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is toxic", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic",
      "based on posterior Beta distribution using a Beta(1,1) prior",
      "is 24% and thus above the required 10% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
  my_data <- h_get_data()
  my_data@x[my_data@cohort == 1] <- c(0.001, 75, 75, 75)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Lowest active dose not tested, stopping rule not applied."
  )
  expect_identical(result, expected) # First active dose not applied.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not toxic", {
  my_data <- h_get_data(placebo = FALSE)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.9)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic based on",
      "posterior Beta distribution using a Beta(1,1) prior is 17% and thus",
      "below the required 90% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% < 90%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is toxic", {
  my_data <- h_get_data(placebo = FALSE)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = paste(
      "Probability that the lowest active dose of 25 being toxic based on",
      "posterior Beta distribution using a Beta(1,1) prior is 17% and thus",
      "above the required 10% threshold."
    )
  )
  expect_identical(result, expected) # Prob being toxic is 24% > 10%.
})

test_that("StoppingLowestDoseHSRBeta works correctly if first active dose is not applied", {
  my_data <- h_get_data(placebo = FALSE)
  my_data@x[my_data@cohort == 1] <- c(75, 75, 75, 75)
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.1)
  result <- stopTrial(
    stopping = stopping,
    dose = 300,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Lowest active dose not tested, stopping rule not applied."
  )
  expect_identical(result, expected) # First active dose not applied.
})
