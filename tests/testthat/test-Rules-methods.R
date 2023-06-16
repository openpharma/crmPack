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

  result <- nextBest(
    nextBest = nb_mtd,
    doselimit = 90,
    samples = samples,
    model = model,
    data = data
  )
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
  expect_identical(result$value, 75)
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
  expect_identical(result$value, 75)
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
    overdose = c(0.65, 1),
    max_overdose_prob = 0.55,
    target_relative = FALSE
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
    overdose = c(0.65, 1),
    max_overdose_prob = 0.55,
    target_relative = FALSE
  )

  result <- nextBest(nb_de, Inf, samples, model, data)
  expect_identical(result$value, 100)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestDualEndpoint_atgt_nodlim", result$plot)
})

## NextBestMinDist ----

test_that("nextBest-NextBestMinDist returns expected values and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- nextBest(nb_md, 50, samples, model, data)
  expect_identical(result$value, 50)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMinDist", result$plot)
})

test_that("nextBest-NextBestMinDist returns expected values and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-0.38, -0.13, 1.43, 2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_md <- NextBestMinDist(target = 0.1)

  result <- nextBest(nb_md, 40, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestMinDist with placebo", result$plot)
})

test_that("nextBest-NextBestMinDist returns expected values and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-1.8, -3.8, -2.2, -1.6), alpha1 = c(1.7, 3.3, 5.1, 2.2))
  )
  nb_md <- NextBestMinDist(target = 0.3)

  result <- nextBest(nb_md, Inf, samples, model, data)
  expect_identical(result$value, 75)
  expect_snapshot(result$probs)
  vdiffr::expect_doppelganger("Plot nextBest-NextBestMinDist w/o doselimit", result$plot)
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

## NextBestProbMTDLTE ----

test_that("nextBest-NextBestProbMTDLTE returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- nextBest(nb_prob_mtd, 90, samples, model, data)
  expect_identical(result$value, 75)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestProbMTDLTE", result$plot)
})

test_that("nextBest-NextBestProbMTDLTE returns correct next dose and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-0.38, -0.13, 1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- nextBest(nb_prob_mtd, 40, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestProbMTDLTE with placebo", result$plot)
})


test_that("nextBest-NextBestProbMTDLTE returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDLTE(target = 0.3)

  result <- nextBest(nb_prob_mtd, Inf, samples, model, data)
  expect_identical(result$value, 125)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot nextBest-NextBestProbMTDLTE w/o doselimit", result$plot)
})

## NextBestProbMTDMinDist ----

test_that("nextBest-NextBestProbMTDMinDist returns correct next dose and plot", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- nextBest(nb_prob_mtd, 90, samples, model, data)
  expect_identical(result$value, 75)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestProbMTDMinDist", result$plot)
})

test_that("nextBest-NextBestProbMTDMinDist returns correct next dose and plot (with placebo)", {
  data <- h_get_data(placebo = TRUE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(-0.38, -0.13, 1.43, 2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- nextBest(nb_prob_mtd, 40, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot of nextBest-NextBestProbMTDMinDist with placebo", result$plot)
})

test_that("nextBest-NextBestProbMTDMinDist returns correct next dose and plot (no doselimit)", {
  data <- h_get_data(placebo = FALSE)
  model <- h_get_logistic_log_normal()
  samples <- h_as_samples(
    list(alpha0 = c(2.38, -2.13, -1.43, -2.57), alpha1 = c(1.67, 1.3, 1.77, 2.51))
  )
  nb_prob_mtd <- NextBestProbMTDMinDist(target = 0.3)

  result <- nextBest(nb_prob_mtd, Inf, samples, model, data)
  expect_identical(result$value, 25)
  expect_snapshot(result$allocation)
  vdiffr::expect_doppelganger("Plot nextBest-NextBestProbMTDMinDist w/o doselimit", result$plot)
})


# maxDose ----

## IncrementsRelative ----

test_that("maxDose-IncrementsRelative works correctly for last dose in 1st interval", {
  increments <- IncrementsRelative(intervals = c(0, 110), increments = c(1, 0.5))
  data <- Data(
    x = c(5, 100), y = c(1L, 0L), doseGrid = c(5, 100, 270), ID = 1:2, cohort = 1:2
  )
  result <- maxDose(increments, data)
  expect_equal(result, 200)
})

test_that("maxDose-IncrementsRelative works correctly for last dose in 2nd interval", {
  increments <- IncrementsRelative(intervals = c(0, 90), increments = c(1, 0.5))
  data <- Data(
    x = c(5, 100), y = c(1L, 0L), doseGrid = c(5, 100, 270), ID = 1:2, cohort = 1:2
  )
  result <- maxDose(increments, data)
  expect_equal(result, 150)

  # Edge case: interval bound is equal to the last dose.
  increments <- IncrementsRelative(intervals = c(0, 100), increments = c(1, 0.5))
  result <- maxDose(increments, data)
  expect_equal(result, 150)
})

test_that("maxDose-IncrementsRelative throws error when last dose is below the first interval", {
  increments <- IncrementsRelative(intervals = c(200, 300), increments = c(1, 0.5))
  data <- Data(
    x = c(5, 100), y = c(1L, 0L), doseGrid = c(5, 100, 270), ID = 1:2, cohort = 1:2
  )
  expect_error(
    maxDose(increments, data),
    "Assertion on 'last_dose.*intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelative throws error when IncrementsRelative is empty", {
  increments <- IncrementsRelative(intervals = numeric(0), increments = numeric(0))
  data <- h_get_data()
  expect_error(
    maxDose(increments, data),
    "Assertion on 'last_dose.*intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelative throws error when Data is empty", {
  increments <- IncrementsRelative(intervals = c(0, 100), increments = c(1, 0.5))
  expect_error(
    maxDose(increments, Data()),
    "Assertion on 'last_dose.*intervals.*failed: Must be TRUE."
  )
})

## IncrementsRelativeDLT ----

test_that("maxDose-IncrementsRelativeDLT works correctly for no of DLTs in 1st interval", {
  increments <- IncrementsRelativeDLT(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  data <- Data(
    x = c(5, 100), y = c(0L, 0L), doseGrid = c(5, 100), ID = 1:2, cohort = 1:2
  )
  result <- maxDose(increments, data)
  expect_equal(result, 200)

  # 1 DLT in total.
  data@y <- c(1L, 0L)
  result <- maxDose(increments, data)
  expect_equal(result, 200)
})

test_that("maxDose-IncrementsRelativeDLT works correctly for no of DLTs in 2nd interval", {
  dgrid <- c(5, 100, 150, 200)
  increments <- IncrementsRelativeDLT(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  data <- Data(x = c(5, 100), y = c(1L, 1L), doseGrid = dgrid, ID = 1:2, cohort = 1:2)
  result <- maxDose(increments, data)
  expect_equal(result, 150)

  # 3 DLTs in total.
  data <- Data(
    x = c(5, 100, 150, 200), y = c(1L, 1L, 1L, 0L), doseGrid = dgrid, ID = 1:4, cohort = 1:4
  )
  result <- maxDose(increments, data)
  expect_equal(result, 300)
})

test_that("maxDose-IncrementsRelativeDLT throws error when no of DLTs is below the first interval", {
  increments <- IncrementsRelativeDLT(dlt_intervals = c(2, 5), increments = c(1, 0.5))
  data <- Data(x = c(5, 100), y = c(0L, 1L), doseGrid = c(5, 100), ID = 1:2, cohort = 1:2)
  expect_error(
    maxDose(increments, data),
    "Assertion on 'dlt_count.*dlt_intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeDLT throws error when IncrementsRelativeDLT is empty", {
  increments <- IncrementsRelativeDLT(dlt_intervals = numeric(0), increments = numeric(0))
  data <- h_get_data()
  expect_error(
    maxDose(increments, data),
    "Assertion on 'dlt_count.*dlt_intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeDLT throws error when Data is empty", {
  increments <- IncrementsRelativeDLT(dlt_intervals = c(1, 4), increments = c(1, 0.5))
  expect_error(
    maxDose(increments, Data()),
    "Assertion on 'dlt_count.*dlt_intervals.*failed: Must be TRUE."
  )
})

## IncrementsRelativeDLTCurrent ----

test_that("IncrementsRelativeDLTCurrent works correctly", {
  increments <- IncrementsRelativeDLTCurrent(
    dlt_intervals = c(0, 1, 3),
    increments = c(1, 0.33, 0.2)
  )
  data <- h_get_data_1()
  result <- maxDose(increments, data)
  expect_equal(result, 13.3) # maxDose is 13.3 because last dose was 10 with 1 DLT.
})

test_that("maxDose-IncrementsRelativeDLTCurrent works correctly when DLTs in 1st interval, no DLTs in cohorts", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  # no DLTs in 1st interval.
  data <- Data(
    x = c(5, 100, 100), y = c(0L, 0L, 0L), doseGrid = c(5, 100), ID = 1:3, cohort = c(1, 2, 2)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 200)

  # 1 DLT in 1st interval.
  data@y <- c(0L, 1L, 0L)
  result <- maxDose(increments, data)
  expect_equal(result, 200)
})

test_that("maxDose-IncrementsRelativeDLTCurrent works correctly when DLTs in 1st interval, DLTs in cohorts", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  # no DLTs in 1st interval.
  data <- Data(
    x = c(5, 5, 20, 20, 20, 100, 100),
    y = c(0L, 1L, 0L, 1L, 1L, 0L, 0L),
    doseGrid = c(5, 15, 20, 100),
    ID = 1:7,
    cohort = c(1, 1, 2, 2, 2, 3, 3)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 200)

  # 1 DLT in 1st interval.
  data@y <- c(0L, 1L, 0L, 1L, 1L, 1L, 0L)
  result <- maxDose(increments, data)
  expect_equal(result, 200)
})

test_that("maxDose-IncrementsRelativeDLTCurrent works correctly when DLTs in 2nd interval, no DLTs in cohorts", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  # 2 DLTs in 2nd interval.
  data <- Data(
    x = c(5, 100, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100), ID = 1:3, cohort = c(1, 2, 2)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 150)

  # 3 DLT in 1st interval.
  data <- Data(
    x = c(5, 100, 100, 100), y = c(0L, 1L, 1L, 1L), doseGrid = c(5, 100), ID = 1:4, cohort = c(1, 2, 2, 2)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 150)
})

test_that("maxDose-IncrementsRelativeDLTCurrent works correctly when DLTs in 2nd interval, DLTs in cohorts", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(0, 2), increments = c(1, 0.5))
  # 2 DLTs in 2nd interval.
  data <- Data(
    x = c(5, 5, 20, 20, 20, 100, 100, 100),
    y = c(0L, 1L, 0L, 1L, 1L, 1L, 1L, 0L),
    doseGrid = c(5, 15, 20, 100),
    ID = 1:8,
    cohort = c(1, 1, 2, 2, 2, 3, 3, 3)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 150)

  # 3 DLT in 1st interval.
  y <- c(0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 150)
})

test_that("maxDose-IncrementsRelativeDLTCurrent throws error when no of DLTs below the first interval", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(2, 5), increments = c(1, 0.5))
  data <- Data(x = c(5, 100), y = c(0L, 1L), doseGrid = c(5, 100), ID = 1:2, cohort = 1:2)
  expect_error(
    maxDose(increments, data),
    "Assertion on 'dlt_count_lcohort.*dlt_intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeDLTCurrent throws error when IncrementsRelativeDLTCurrent is empty", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = numeric(0), increments = numeric(0))
  data <- h_get_data()
  expect_error(
    maxDose(increments, data),
    "Assertion on 'dlt_count_lcohort.*dlt_intervals.*failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeDLTCurrent throws error when Data is empty", {
  increments <- IncrementsRelativeDLTCurrent(dlt_intervals = c(1, 4), increments = c(1, 0.5))
  expect_error(
    maxDose(increments, Data()),
    "Assertion on 'dlt_count_lcohort.*dlt_intervals.*failed: Must be TRUE."
  )
})

## IncrementsRelativeParts ----

test_that("maxDose-IncrementsRelativeParts works correctly when in part 1 and part 2 not started", {
  increments <- IncrementsRelativeParts(
    dlt_start = 5, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10),
    part = c(1L, 1L, 1L),
    nextPart = 1L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 3)
})

test_that("maxDose-IncrementsRelativeParts works correctly when in part 1, part 2 started, DLT", {
  increments <- IncrementsRelativeParts(
    dlt_start = 3, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 1, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10, 15, 20, 30),
    part = c(1L, 1L, 1L),
    nextPart = 2L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10, 20)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 10)
})

test_that("maxDose-IncrementsRelativeParts works correctly when in part 1, part 2 started, no DLT, clean_start > 0", {
  increments <- IncrementsRelativeParts(
    dlt_start = 3, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10, 15, 20, 30),
    part = c(1L, 1L, 1L),
    nextPart = 2L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10, 20)
  )
  result <- maxDose(increments, data)
  expected_increments <- IncrementsRelative(intervals = c(0, 1), increments = c(4, 3))
  expected <- maxDose(expected_increments, data) # expected = 2.5 # nolintr
  expect_equal(result, expected)
})

test_that("maxDose-IncrementsRelativeParts works correctly when in part 1, part 2 started, no DLT, clean_start <= 0", {
  increments <- IncrementsRelativeParts(
    dlt_start = -9, clean_start = -2, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.4, 0.5, 1.5, 3, 6, 10, 15, 20, 30),
    part = c(1L, 1L, 1L),
    nextPart = 2L,
    part1Ladder = c(0.1, 0.4, 0.5, 1.5, 3, 6, 10, 20)
  )
  result <- maxDose(increments, data)
  expect_equal(result, 0.4)
})

test_that("maxDose-IncrementsRelativeParts works correctly when already in part 2", {
  increments <- IncrementsRelativeParts(
    dlt_start = 5, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 0.5, 1.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10),
    part = c(1L, 1L, 2L),
    nextPart = 1L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6)
  )
  result <- maxDose(increments, data)
  expected_increments <- IncrementsRelative(intervals = c(0, 1), increments = c(4, 3))
  expected <- maxDose(expected_increments, data) # expected = 6 # nolintr
  expect_equal(result, expected)
})

test_that("maxDose-IncrementsRelativeParts throws error when part1Ladder is exceeded (in p1, no p2)", {
  increments <- IncrementsRelativeParts(
    dlt_start = 5, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 6, 0.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10),
    part = c(1L, 1L, 1L),
    nextPart = 1L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6)
  )
  expect_error(
    maxDose(increments, data),
    "Assertion on 'new_max_dose_level <= length\\(data@part1Ladder\\)' failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeParts throws error when part1Ladder is exceeded (in p1, p2, DLT)", {
  increments <- IncrementsRelativeParts(
    dlt_start = 5, clean_start = 9, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 1, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10, 15, 20, 30),
    part = c(1L, 1L, 1L),
    nextPart = 2L,
    part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10, 20)
  )
  expect_error(
    maxDose(increments, data),
    "Assertion on 'new_max_dose_level <= length\\(data@part1Ladder\\)' failed: Must be TRUE."
  )
  increments@dlt_start <- -4L
  expect_error(
    maxDose(increments, data),
    "Assertion on 'new_max_dose_level >= 0L' failed: Must be TRUE."
  )
})

test_that("maxDose-IncrementsRelativeParts throws error when part1Ladder is exceeded (in p1, p2, DLT, cstart <= 0)", {
  increments <- IncrementsRelativeParts(
    dlt_start = -9, clean_start = -5, intervals = c(0, 1), increments = c(4, 3)
  )
  data <- DataParts(
    x = c(0.1, 1.5, 0.5),
    y = c(0, 0, 0),
    ID = 1:3,
    cohort = 1:3,
    doseGrid = c(0.1, 0.4, 0.5, 1.5, 3, 6, 10, 15, 20, 30),
    part = c(1L, 1L, 1L),
    nextPart = 2L,
    part1Ladder = c(0.1, 0.4, 0.5, 1.5, 3, 6, 10, 20)
  )
  expect_error(
    maxDose(increments, data),
    "Assertion on 'new_max_dose_level >= 0L' failed: Must be TRUE."
  )
})

## IncrementsDoseLevels ----

test_that("maxDose-IncrementsDoseLevels works correctly for 'last' basis_level and 1 level increase", {
  increments <- IncrementsDoseLevels(levels = 1)
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 250)
})

test_that("maxDose-IncrementsDoseLevels works correctly for 'last' basis_level and 2 levels increase", {
  increments <- IncrementsDoseLevels(levels = 2)
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 300)
})

test_that("maxDose-IncrementsDoseLevels works correctly for 'max' basis_level and 1 level increase", {
  increments <- IncrementsDoseLevels(levels = 1, basis_level = "max")
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 300)
})

test_that("maxDose-IncrementsDoseLevels works correctly for 'max' basis_level and 2 levels increase", {
  increments <- IncrementsDoseLevels(levels = 2, basis_level = "max")
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 400)
})

test_that("maxDose-IncrementsDoseLevels works correctly for 'last' basis_level and over-grid increase", {
  increments <- IncrementsDoseLevels(levels = 4)
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 400)
})

test_that("maxDose-IncrementsDoseLevels works correctly for 'max' basis_level and over-grid increase", {
  increments <- IncrementsDoseLevels(levels = 3, basis_level = "max")
  data <- Data(
    x = c(5, 250, 100), y = c(0L, 1L, 1L), doseGrid = c(5, 100, 250, 300, 400), ID = 1:3, cohort = 1:3
  )
  result <- maxDose(increments, data = data)
  expect_equal(result, 400)
})

## IncrementsHSRBeta ----

test_that("IncrementsHSRBeta works correctly if toxcicity probability is below threshold probability", {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  data <- h_get_data()
  data@y[data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 300) # maxDose is 300 as toxicity probability of no dose is above 0.95.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability", {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.9)
  data <- h_get_data()
  data@y[data@cohort == 3L] <- c(0L, 0L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability"
), {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  data <- h_get_data()
  data@y[data@cohort == 1L] <- c(0L, 1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability of placebo is above threshold probability", {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  data <- h_get_data()
  data@y[data@x == 0.001] <- c(1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 300) # maxDose is 300 as placebo is ignored.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability and placebo == T, but not appplied"
), {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)
  data <- h_get_data()
  data@x <- c(rep(25, 4), rep(50, 4), rep(100, 4))
  data@y[data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.95 and placebo used.
})

test_that(paste(
  "IncrementsHSRBeta works correctly if toxcicity probability of first",
  "active dose is above threshold probability (no placebo)"
), {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  data <- h_get_data(placebo = FALSE)
  data@y[data@cohort == 1] <- c(0L, 1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 25) # maxDose is 25 as toxicity probability of dose 25 is above 0.90.
})

test_that("IncrementsHSRBeta works correctly if toxcicity probability is above threshold probability (no placebo)", {
  increments <- IncrementsHSRBeta(target = 0.3, prob = 0.90)
  data <- h_get_data(placebo = FALSE)
  data@y[data@cohort == 3] <- c(0L, 1L, 1L, 1L)
  result <- maxDose(increments, data)
  expect_equal(result, 75) # maxDose is 75 as toxicity probability of dose 100 is above 0.90.
})

## IncrementsMin ----

test_that("maxDose-IncrementsMin works correctly when incr1 is minimum", {
  incr1 <- IncrementsRelative(intervals = c(0, 20), increments = c(4, 0.1))
  incr2 <- IncrementsRelativeDLT(dlt_intervals = c(0, 1, 3), increments = c(2, 0.5, 0.4))
  increments <- IncrementsMin(increments_list = list(incr1, incr2))
  data <- Data(
    x = c(5, 100), y = c(1L, 0L), doseGrid = c(5, 100), ID = 1:2, cohort = 1:2
  )
  result <- maxDose(increments, data)
  expect_equal(result, 110)
})

test_that("maxDose-IncrementsMin works correctly when incr2 is minimum", {
  incr1 <- IncrementsRelative(intervals = c(0, 20), increments = c(4, 0.7))
  incr2 <- IncrementsRelativeDLT(dlt_intervals = c(0, 1, 3), increments = c(2, 0.5, 0.4))
  increments <- IncrementsMin(increments_list = list(incr1, incr2))
  data <- Data(
    x = c(5, 100), y = c(1L, 0L), doseGrid = c(5, 100), ID = 1:2, cohort = 1:2
  )
  result <- maxDose(increments, data)
  expect_equal(result, 150)
})

# stopTrial ----

## StoppingMissingDose ----

test_that("StoppingMissingDose works correctly", {
  stopping <- StoppingMissingDose()

  result <- stopTrial(
    stopping,
    dose = NA_real_,
    data = Data(doseGrid = c(0, 1), placebo = TRUE)
  )
  expect_true(result)
  expect_equal(
    attributes(result),
    list(
      message = "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
    )
  )

  result <- stopTrial(
    stopping,
    dose = 0,
    data = Data(doseGrid = c(0, 1), placebo = TRUE)
  )
  expect_true(result)
  expect_equal(
    attributes(result),
    list(
      message = "Next dose is placebo dose , i.e., no active dose is safe enough according to the NextBest rule."
    )
  )

  result <- stopTrial(
    stopping,
    dose = 1,
    data = Data(doseGrid = c(0, 1), placebo = TRUE)
  )
  expect_false(result)
  expect_equal(
    attributes(result),
    list(message = "Next dose is available at the dose grid.")
  )
})

## StoppingCohortsNearDose ----

test_that("StoppingCohortsNearDose can handle when dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingCohortsNearDose(nCohorts = 2, percentage = 0)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "0 cohorts lie within 0% of the next best dose NA. This is below the required 2 cohorts"
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingCohortsNearDose", {
  # Exactly n cohorts at dose
  stopRule <- StoppingCohortsNearDose(nCohorts = 2, percentage = 0)
  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )
  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3),
    new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(2, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "2 cohorts lie within 0% of the next best dose 2. This reached the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(2, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3),
    new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "2 cohorts lie within 0% of the next best dose 2. This reached the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(2, 2), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(2, 2), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3),
    new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 1, 2, 2, 2, 2), y = rep(0, 6), cohort = c(1L, 1L, 2L, 2L, 3L, 3L), ID = 1:6, doseGrid = 1:3),
    new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "2 cohorts lie within 0% of the next best dose 2. This reached the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 1, 2, 2, 2, 2), y = rep(0, 6), cohort = c(1L, 1L, 2L, 2L, 3L, 3L), ID = 1:6, doseGrid = 1:3)
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "2 cohorts lie within 0% of the next best dose 2. This reached the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 1, 2, 2, 2, 2), y = rep(0, 6), cohort = c(1L, 1L, 2L, 2L, 2L, 2L), ID = 1:6, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 1, 2, 2, 2, 2), y = rep(0, 6), cohort = c(1L, 1L, 2L, 2L, 2L, 2L), ID = 1:6, doseGrid = 1:3),
    new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 0% of the next best dose 2. This is below the required 2 cohorts")
  )

  # n cohorts around dose
  stopRule <- StoppingCohortsNearDose(nCohorts = 2, percentage = 35)
  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 35% of the next best dose 2. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "1 cohorts lie within 35% of the next best dose 3. This is below the required 2 cohorts")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(2, 3), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "2 cohorts lie within 35% of the next best dose 3. This reached the required 2 cohorts")
  )
})

## StoppingPatientsNearDose ----

test_that("StoppingPatientsNearDose can handle when dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingPatientsNearDose(nPatients = 9, percentage = 0)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "0 patients lie within 0% of the next best dose NA. This is below the required 9 patients"
  )
  expect_identical(result, expected)
})

## StoppingMinCohorts ----

test_that("StoppingMinCohorts works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingMinCohorts(nCohorts = 4)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Number of cohorts is 3 and thus below the prespecified minimum number 4"
    )
  )
  expect_identical(result, expected)
})

test_that("StoppingMinCohorts works correctly in edge cases", {
  s1 <- StoppingMinCohorts(nCohorts = 2)

  rv <- stopTrial(s1, dose = 0, data = Data(doseGrid = c(0, 1), placebo = TRUE))
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "Number of cohorts is 0 and thus below the prespecified minimum number 2")
  )

  s1 <- StoppingMinCohorts(nCohorts = 1)

  rv <- stopTrial(s1, dose = 0.01, data = h_get_data())
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "Number of cohorts is 3 and thus reached the prespecified minimum number 1")
  )
})

## StoppingMinPatients ----

test_that("StoppingMinPatients works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingMinPatients(nPatients = 18)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Number of patients is 12 and thus below the prespecified minimum number 18"
    )
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingMinPatients", {
  stopRule <- StoppingMinPatients(nPatients = 3)

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3),
    new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "Number of patients is 2 and thus below the prespecified minimum number 3")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(message = "Number of patients is 2 and thus below the prespecified minimum number 3")
  )

  rv <- stopTrial(
    stopping = stopRule,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = Data(x = c(1, 2, 2), y = c(0, 0, 0), cohort = c(1L, 2L, 2L), ID = 1:3, doseGrid = 1:3)
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(message = "Number of patients is 3 and thus reached the prespecified minimum number 3")
  )
})

## StoppingTargetProb ----

test_that("StoppingTargetProb can handle when dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingTargetProb(target = c(0.15, 0.2), prob = 0.3)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Probability for target toxicity is 0 % for dose NA and thus below the required 30 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingTargetProb works correctly when below threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingTargetProb(target = c(0.15, 0.2), prob = 0.3)
  result <- stopTrial(
    stopping = stopping,
    dose = 100,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Probability for target toxicity is 14 % for dose 100 and thus below the required 30 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingTargetProb works correctly when above threshold", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingTargetProb(target = c(0.1, 0.4), prob = 0.3)
  result <- stopTrial(
    stopping = stopping,
    dose = 100,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = "Probability for target toxicity is 82 % for dose 100 and thus above the required 30 %"
  )
  expect_identical(result, expected)
})

## StoppingMTDdistribution ----

test_that("StoppingMTDdistribution can handle when dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingMTDdistribution(target = 0.25, thresh = 0.3, prob = 0.3)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "Probability of MTD above 30 % of current dose NA is 0 % and thus strictly less than the required 30 %"
  )
  expect_identical(result, expected) # CV is 23% < 30%.
})

test_that("stopTrial works correctly for StoppingMTDdistribution", {
  # Observed data is irrelevant in this case.  provide an empty Data object
  emptyData <- Data(doseGrid = 1:5)
  # Define a model
  model <- LogisticLogNormal(mean = c(-3, 2), cov = diag(2))
  # Generate some samples from the model
  n_samples <- 100
  samples <- mcmc(
    emptyData,
    model,
    McmcOptions(
      samples = n_samples,
      rng_kind = "Mersenne-Twister",
      rng_seed = 460017
    )
  )
  for (targetRate in seq(0.05, 0.95, 0.1)) {
    for (threshold in seq(0.1, 0.9, 0.2)) {
      for (confidence in seq(0.5, 0.9, 0.2)) {
        for (d in emptyData@doseGrid) {
          sampledMTD <- dose(targetRate, model, samples)
          thresholdDose <- d * threshold
          sampledConfidence <- mean(sampledMTD > thresholdDose)
          result <- stopTrial(
            StoppingMTDdistribution(targetRate, threshold, confidence),
            d,
            samples,
            model,
            data = emptyData
          )
          direction <- ifelse(as.logical(result), "greater than or equal to", "strictly less than")
          expected <- sampledConfidence >= confidence
          if (expected != as.logical(result)) {
            print(
              paste0(
                "targetRate: ", targetRate, "; threshold: ", threshold,
                "; confidence: ", confidence, "; d: ", d, "; expected: ",
                expected, "[", sampledConfidence, "]; actual: ",
                as.logical(result), " [", attr(result, "message"), "]"
              )
            )
          }
          attr(expected, "message") <- paste0(
            "Probability of MTD above ",
            threshold * n_samples,
            " % of current dose ",
            d,
            " is ",
            sampledConfidence * n_samples,
            " % and thus ",
            direction,
            " the required ",
            n_samples * confidence,
            " %"
          )
          expect_equal(result, expected)
        }
      }
    }
  }
})

## StoppingMTDCV ----

test_that("StoppingMTDCV can handle when dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(samples = 1000, burnin = 1000))
  stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 30)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = "CV of MTD is 40 % and thus above the required precision threshold of 30 %"
  )
  expect_identical(result, expected) # CV is 23% < 30%.
})

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

## StoppingLowestDoseHSRBeta ----

test_that("StoppingLowestDoseHSRBeta works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingLowestDoseHSRBeta(target = 0.3, prob = 0.9)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
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

## StoppingTargetBiomarker ----

test_that("StoppingTargetBiomarker can handle when dose is NA", {
  data <- h_get_data_dual()
  model <- h_get_dual_endpoint_rw()
  options <- h_get_mcmc_options()
  samples <- mcmc(data, model, options)
  stopping <- StoppingTargetBiomarker(
    target = c(0.9, 1),
    prob = 0.5
  )
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = samples,
    model = model,
    data = data
  )
  expected <- structure(
    FALSE,
    message = "Probability for target biomarker is 0 % for dose NA and thus below the required 50 %"
  )
  expect_identical(result, expected)
})

test_that("stopTrial works for StoppingTargetBiomarker", {
  # Simply copying example code.  probably needs more thoughtful testing
  data <- DataDual(
    ID = 1:17,
    cohort = 1:17,
    x = c(
      0.1, 0.5, 1.5, 3, 6, 10, 10, 10,
      20, 20, 20, 40, 40, 40, 50, 50, 50
    ),
    y = c(
      0, 0, 0, 0, 0, 0, 1, 0,
      0, 1, 1, 0, 0, 1, 0, 1, 1
    ),
    w = c(
      0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.55, 0.6,
      0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21
    ),
    doseGrid = c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    )
  )

  # Initialize the Dual-Endpoint model (in this case RW1)
  model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )

  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 500,
    rng_kind = "Mersenne-Twister",
    rng_seed = 94
  )
  samples <- mcmc(data, model, options)

  # Set-up some MCMC parameters and generate samples from the posterior
  samples <- mcmc(data, model, options)

  # Define the rule for dose increments and calculate the maximum dose allowed
  myIncrements <- IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )
  nextMaxDose <- maxDose(myIncrements, data = data)

  # Define the rule which will be used to select the next best dose
  # In this case target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
  # and with a probability below 0.25 that prob(DLT)>0.35 (safety)

  myNextBest <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  # Define the stopping rule such that the study would be stopped if if there is at
  # least 0.5 posterior probability that the biomarker (efficacy) is within the
  # biomarker target range of [0.9, 1.0] (relative to the maximum for the biomarker).

  myStopping <- StoppingTargetBiomarker(
    target = c(0.9, 1),
    prob = 0.5
  )

  expectedAttributes <- list(
    "0.1" = "Probability for target biomarker is 2 % for dose 0.1 and thus below the required 50 %",
    "0.5" = "Probability for target biomarker is 1 % for dose 0.5 and thus below the required 50 %",
    "1.5" = "Probability for target biomarker is 2 % for dose 1.5 and thus below the required 50 %",
    "3" = "Probability for target biomarker is 3 % for dose 3 and thus below the required 50 %",
    "6" = "Probability for target biomarker is 14 % for dose 6 and thus below the required 50 %",
    "10" = "Probability for target biomarker is 11 % for dose 10 and thus below the required 50 %",
    "12" = "Probability for target biomarker is 7 % for dose 12 and thus below the required 50 %",
    "14" = "Probability for target biomarker is 9 % for dose 14 and thus below the required 50 %",
    "16" = "Probability for target biomarker is 4 % for dose 16 and thus below the required 50 %",
    "18" = "Probability for target biomarker is 3 % for dose 18 and thus below the required 50 %",
    "20" = "Probability for target biomarker is 1 % for dose 20 and thus below the required 50 %",
    "22" = "Probability for target biomarker is 3 % for dose 22 and thus below the required 50 %",
    "24" = "Probability for target biomarker is 3 % for dose 24 and thus below the required 50 %",
    "26" = "Probability for target biomarker is 4 % for dose 26 and thus below the required 50 %",
    "28" = "Probability for target biomarker is 2 % for dose 28 and thus below the required 50 %",
    "30" = "Probability for target biomarker is 3 % for dose 30 and thus below the required 50 %",
    "32" = "Probability for target biomarker is 1 % for dose 32 and thus below the required 50 %",
    "34" = "Probability for target biomarker is 0 % for dose 34 and thus below the required 50 %",
    "36" = "Probability for target biomarker is 0 % for dose 36 and thus below the required 50 %",
    "38" = "Probability for target biomarker is 0 % for dose 38 and thus below the required 50 %",
    "40" = "Probability for target biomarker is 0 % for dose 40 and thus below the required 50 %",
    "42" = "Probability for target biomarker is 0 % for dose 42 and thus below the required 50 %",
    "44" = "Probability for target biomarker is 0 % for dose 44 and thus below the required 50 %",
    "46" = "Probability for target biomarker is 0 % for dose 46 and thus below the required 50 %",
    "48" = "Probability for target biomarker is 0 % for dose 48 and thus below the required 50 %",
    "50" = "Probability for target biomarker is 0 % for dose 50 and thus below the required 50 %",
    "52" = "Probability for target biomarker is 0 % for dose 52 and thus below the required 50 %",
    "54" = "Probability for target biomarker is 0 % for dose 54 and thus below the required 50 %",
    "56" = "Probability for target biomarker is 1 % for dose 56 and thus below the required 50 %",
    "58" = "Probability for target biomarker is 1 % for dose 58 and thus below the required 50 %",
    "60" = "Probability for target biomarker is 1 % for dose 60 and thus below the required 50 %",
    "62" = "Probability for target biomarker is 1 % for dose 62 and thus below the required 50 %",
    "64" = "Probability for target biomarker is 2 % for dose 64 and thus below the required 50 %",
    "66" = "Probability for target biomarker is 1 % for dose 66 and thus below the required 50 %",
    "68" = "Probability for target biomarker is 1 % for dose 68 and thus below the required 50 %",
    "70" = "Probability for target biomarker is 3 % for dose 70 and thus below the required 50 %",
    "72" = "Probability for target biomarker is 2 % for dose 72 and thus below the required 50 %",
    "74" = "Probability for target biomarker is 2 % for dose 74 and thus below the required 50 %",
    "76" = "Probability for target biomarker is 4 % for dose 76 and thus below the required 50 %",
    "78" = "Probability for target biomarker is 3 % for dose 78 and thus below the required 50 %",
    "80" = "Probability for target biomarker is 4 % for dose 80 and thus below the required 50 %"
  )

  sapply(
    data@doseGrid,
    function(d) {
      actual <- stopTrial(
        stopping = myStopping,
        dose = d,
        samples = samples,
        model = model,
        data = data
      )
      expected <- FALSE
      attr(expected, "message") <- expectedAttributes[[as.character(d)]]
      expect_equal(actual, expected)
    }
  )
})


## StoppingSpecificDose ----

test_that("StoppingSpecificDose works correctly if next dose is NA", {
  my_samples <- h_as_samples(
    list(alpha0 = c(1.2, 0, -0.4, -0.1, 0.9), alpha1 = c(0.7, 1.7, 1.9, 0.6, 2.8))
  )
  result <- stopTrial(
    stopping = h_stopping_specific_dose(),
    dose = NA_real_,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_1()
  )
  expected <- structure(
    FALSE,
    message = "Probability for target toxicity is 0 % for dose 80 and thus below the required 80 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingSpecificDose works correctly if dose rec. differs from specific and stop crit. not met", {
  # StoppingSpecificDose works correctly if dose recommendation is not the same
  # as the specific dose and stop is not met.
  my_samples <- h_as_samples(
    list(alpha0 = c(1.2, 0, -0.4, -0.1, 0.9), alpha1 = c(0.7, 1.7, 1.9, 0.6, 2.8))
  )
  result <- stopTrial(
    stopping = h_stopping_specific_dose(),
    dose = 20,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_1()
  )
  expected <- structure(
    FALSE,
    message = "Probability for target toxicity is 0 % for dose 80 and thus below the required 80 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingSpecificDose works correctly if dose rec. differs from specific and stop crit. is met", {
  # StoppingSpecificDose works correctly if dose recommendation is not the same
  # as the specific dose and stop is met.
  my_samples <- h_as_samples(
    list(
      alpha0 = c(-1.88, -1.58, -2.43, -3.61, -2.15, -2.28, -3.32, -2.16, -2.79, -2.90),
      alpha1 = c(1.08, 0.86, 0.67, 2.38, 5.99, 2.94, 0.74, 2.39, 1.74, 0.84)
    )
  )
  result <- stopTrial(
    stopping = h_stopping_specific_dose(),
    dose = 20,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_1()
  )
  expected <- structure(
    TRUE,
    message = "Probability for target toxicity is 90 % for dose 80 and thus above the required 80 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingSpecificDose works correctly if dose rec = specific and stop crit. not met", {
  # StoppingSpecificDose works correctly if dose recommendation is the same
  # as the specific dose and stop is not met.
  my_samples <- h_as_samples(
    list(alpha0 = c(1.2, 0, -0.4, -0.1, 0.9), alpha1 = c(0.7, 1.7, 1.9, 0.6, 2.8))
  )
  result <- stopTrial(
    stopping = h_stopping_specific_dose(),
    dose = 80,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_1()
  )
  expected <- structure(
    FALSE,
    message = "Probability for target toxicity is 0 % for dose 80 and thus below the required 80 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingSpecificDose works correctly if dose rec. = specific and stop crit. is met", {
  # StoppingSpecificDose works correctly if dose recommendation is the same
  # as the specific dose and stop is met.
  my_samples <- h_as_samples(
    list(
      alpha0 = c(-1.88, -1.58, -2.43, -3.61, -2.15, -2.28, -3.32, -2.16, -2.79, -2.90),
      alpha1 = c(1.08, 0.86, 0.67, 2.38, 5.99, 2.94, 0.74, 2.39, 1.74, 0.84)
    )
  )
  result <- stopTrial(
    stopping = h_stopping_specific_dose(),
    dose = 80,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_1()
  )
  expected <- structure(
    TRUE,
    message = "Probability for target toxicity is 90 % for dose 80 and thus above the required 80 %"
  )
  expect_identical(result, expected)
})

test_that("StoppingSpecificDose correctly replaces next best string with specific string", {
  my_stopping <- StoppingSpecificDose(
    rule = StoppingPatientsNearDose(nPatients = 9, percentage = 5),
    dose = 80
  )
  my_samples <- h_as_samples(
    list(
      alpha0 = c(-1.88, -1.58, -2.43, -3.61, -2.15, -2.28, -3.32, -2.16, -2.79, -2.90),
      alpha1 = c(1.08, 0.86, 0.67, 2.38, 5.99, 2.94, 0.74, 2.39, 1.74, 0.84)
    )
  )
  result <- stopTrial(
    stopping = my_stopping,
    dose = 20,
    samples = my_samples,
    model = h_get_logistic_log_normal(),
    data = h_get_data_sr_2()
  )
  expected <- structure(
    TRUE,
    message = "12 patients lie within 5% of the specific dose 80. This reached the required 9 patients"
  )
  expect_identical(result, expected)
})

## StoppingHighestDose ----

test_that("StoppingHighestDose works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  stopping <- StoppingHighestDose()
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = paste(
      "Next best dose is NA and thus not the highest dose"
    )
  )
  expect_identical(result, expected)
})

## StoppingList ----

test_that("StoppingList with any works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  stopping <- StoppingList(stop_list = list(s1, s2), summary = any)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = list(
      "Number of cohorts is 3 and thus reached the prespecified minimum number 2",
      "Next best dose is NA and thus not the highest dose"
    )
  )
  expect_identical(result, expected)
})

test_that("StoppingList with all works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  stopping <- StoppingList(stop_list = list(s1, s2), summary = all)
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = list(
      "Number of cohorts is 3 and thus reached the prespecified minimum number 2",
      "Next best dose is NA and thus not the highest dose"
    )
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingList", {
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  any1 <- StoppingList(stop_list = list(s1, s2), summary = any)
  all1 <- StoppingList(stop_list = list(s1, s2), summary = all)

  data_none <- Data(x = c(1, 1), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any1 <- Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any2 <- Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  data_all <- Data(x = c(1, 3), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )
  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )
  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 2 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_all
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  data_any1 <- Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  data_any2 <- Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  rv <- stopTrial(
    stopping = any1,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 2 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_all,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )
})

## StoppingAll ----

test_that("StoppingAll works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  stopping <- StoppingAll(stop_list = list(s1, s2))
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    FALSE,
    message = list(
      "Number of cohorts is 3 and thus reached the prespecified minimum number 2",
      "Next best dose is NA and thus not the highest dose"
    )
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingAll", {
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  all1 <- StoppingAll(stop_list = list(s1, s2))

  data_none <- Data(x = c(1, 1), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any1 <- Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any2 <- Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  data_all <- Data(x = c(1, 3), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)

  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )
  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_all
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  data_any2 <- Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  rv <- stopTrial(
    stopping = all1,
    dose = 2,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 2 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = all1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_all,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )
})

test_that("Logical operators for combining Stopping rules work correctly", {
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  s3 <- StoppingPatientsNearDose(nPatients = 9, percentage = 25)
  all1 <- StoppingAll(stop_list = list(s1, s2))

  expect_identical(s1 & s2, StoppingAll(stop_list = list(s1, s2)))

  expect_identical(all1 & s3, StoppingAll(stop_list = list(s1, s2, s3)))
  expect_identical(s3 & all1, StoppingAll(stop_list = list(s3, s1, s2)))
})

## StoppingAny ----

test_that("StoppingAny works correctly if next dose is NA", {
  my_data <- h_get_data()
  my_model <- h_get_logistic_kadane()
  my_samples <- mcmc(my_data, my_model, h_get_mcmc_options(fixed = FALSE))
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  stopping <- StoppingAny(stop_list = list(s1, s2))
  result <- stopTrial(
    stopping = stopping,
    dose = NA_real_,
    samples = my_samples,
    model = my_model,
    data = my_data
  )
  expected <- structure(
    TRUE,
    message = list(
      "Number of cohorts is 3 and thus reached the prespecified minimum number 2",
      "Next best dose is NA and thus not the highest dose"
    )
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingAny", {
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  any1 <- StoppingAny(stop_list = list(s1, s2))

  data_none <- Data(x = c(1, 1), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any1 <- Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 1L), ID = 1:2, doseGrid = 1:3)
  data_any2 <- Data(x = c(1, 2), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)
  data_any3 <- Data(x = c(3, 3), y = c(0, 0), cohort = c(1L, 2L), ID = 1:2, doseGrid = 1:3)

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_none,
    samples = new("Samples")
  )
  expect_false(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any1,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 1 and thus below the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 1,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 1 and thus not the highest dose"
      )
    )
  )

  rv <- stopTrial(
    stopping = any1,
    dose = 3,
    model = LogisticLogNormal(mean = c(0, 1), cov = diag(2)),
    data = data_any2,
    samples = new("Samples")
  )
  expect_true(rv)
  expect_equal(
    attributes(rv),
    list(
      message = list(
        "Number of cohorts is 2 and thus reached the prespecified minimum number 2",
        "Next best dose is 3 and thus the highest dose"
      )
    )
  )
})

test_that("Logical operators for combining Stopping rules work correctly", {
  s1 <- StoppingMinCohorts(nCohorts = 2)
  s2 <- StoppingHighestDose()
  s3 <- StoppingPatientsNearDose(nPatients = 9, percentage = 25)
  any1 <- StoppingAny(stop_list = list(s1, s2))

  expect_identical(s1 | s2, StoppingAny(stop_list = list(s1, s2)))

  expect_identical(any1 | s3, StoppingAny(stop_list = list(s1, s2, s3)))
  expect_identical(s3 | any1, StoppingAny(stop_list = list(s3, s1, s2)))
})

## StoppingTDCIRatio ----

# Numerically not stable. Need to investigate why.
test_that("StoppingTDCIRatio works correctly when dose is NA", {
  data <- h_get_data_dual()
  model <- h_get_logistic_indep_beta()
  options <- h_get_mcmc_options()
  samples <- mcmc(data, model, options)
  # This is necessary as rng do not work with model
  samples@data$phi1 <- c(0.04748928, -3.69616243, -7.38656113,  0.04428348)
  samples@data$phi2 <- c(-0.009012972,  0.737940430,  1.245383234,  0.053978501)
  stopping <- StoppingTDCIRatio(target_ratio = 5, prob_target = 0.3)
  result <- stopTrial(
    stopping,
    NA_real_,
    samples,
    model,
    data = data
  )
  expected <- structure(
    FALSE,
    message = "95% CI is (3.56190161486129, 1.20753437767844e+43), Ratio = 3.39013961710862e+42 is greater than target_ratio = 5"
  )
  expect_identical(result, expected)
})

test_that("stopTrial works correctly for StoppingTDCIRatio when samples are provided", {
  # Observed data is irrelevant in this case.  provide an empty Data object
  emptyData <- Data(doseGrid = seq(25, 300, 25))
  # Define a model
  model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEdose = c(25, 300),
    DLEweights = c(3, 3),
    data = emptyData
  )
  # Generate some samples from the model
  n_samples <- 100
  samples <- mcmc(
    emptyData,
    model,
    McmcOptions(
      samples = n_samples,
      rng_kind = "Mersenne-Twister",
      rng_seed = 12911
    )
  )
  for (targetRate in seq(0.05, 0.95, 0.1)) {
    for (targetRatio in c(3, 6, 10, 20)) {
      for (d in emptyData@doseGrid) {
        sampledMTD <- dose(targetRate, model, samples)

        sampledLimits <- quantile(sampledMTD, probs = c(0.025, 0.975))
        sampledRatio <- sampledLimits[[2]] / sampledLimits[[1]]
        expected <- sampledRatio < targetRatio
        result <- stopTrial(
          StoppingTDCIRatio(targetRatio, targetRate),
          d,
          samples,
          model,
          data = emptyData
        )
        direction <- ifelse(expected, "less", "greater")
        attr(expected, "message") <- paste0(
          "95% CI is (",
          sampledLimits[[1]],
          ", ",
          sampledLimits[[2]],
          "), Ratio = ",
          round(sampledRatio, 4),
          " is ",
          direction,
          " than target_ratio = ",
          targetRatio
        )
        if (expected != as.logical(result)) {
          print(
            paste0(
              "targetRate: ", targetRate, "; targetRatio: ", targetRatio,
              "; d: ", d, "; expected: ",
              expected, "; actual: ",
              as.logical(result), " [", attr(result, "message"), "]"
            )
          )
        }
        expect_equal(result, expected)
      }
    }
  }
})

test_that("stopTrial works correctly for StoppingTDCIRatio when samples are not provided", {
  # Observed data is irrelevant in this case.  provide an empty Data object
  emptyData <- Data(doseGrid = seq(25, 300, 25))
  # Define a model
  model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEdose = c(25, 300),
    DLEweights = c(3, 3),
    data = emptyData
  )
  for (targetRate in seq(0.05, 0.95, 0.1)) {
    for (targetRatio in c(3, 6, 10, 20)) {
      for (d in emptyData@doseGrid) {
        result <- stopTrial(
          stopping = StoppingTDCIRatio(targetRatio, targetRate),
          dose = d,
          model = model,
          data = emptyData
        )
        # TODO: message attribute not checked
        expect_false(result, expected)
      }
    }
  }
})


# CohortSize ----

## CohortSizeDLT ----

test_that("size works as expected for CohortSizeDLT", {
  cohortSize <- CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
  expect_equal(size(cohortSize, NA, Data(doseGrid = 1:3)), 0)
  for (dose in 1:3) {
    expect_equal(
      size(
        object = cohortSize,
        dose = dose,
        data = Data(x = 1:2, y = c(0, 0), ID = 1:2, cohort = 1:2, doseGrid = 1:3)
      ),
      1
    )
    expect_equal(
      size(
        object = cohortSize,
        dose = dose,
        data = Data(x = 1:2, y = c(0, 1), ID = 1:2, cohort = 1:2, doseGrid = 1:3)
      ),
      3
    )
    expect_equal(
      size(
        object = cohortSize,
        dose = dose,
        data = Data(x = 1:2, y = c(1, 1), ID = 1:2, cohort = 1:2, doseGrid = 1:3)
      ),
      3
    )
  }
})

## CohortSizeConst ----

test_that("size works as expected for CohortSizeConst", {
  cohortSize <- CohortSizeConst(size = 4)
  emptyData <- Data(doseGrid = 1:5)
  expect_equal(size(cohortSize, NA, Data(doseGrid = 1:5)), 0)
  for (dose in 1:5) {
    expect_equal(size(object = cohortSize, dose = dose, data = emptyData), 4)
  }
})

## CohortSizeRange ----

test_that("size works as expected for CohortSizeRange", {
  doseGrid <- 1:10
  cohortSize <- CohortSizeRange(intervals = c(0, 5), cohort_size = c(1, 2))
  emptyData <- Data(doseGrid = 1:10)
  expect_equal(size(cohortSize, NA, Data(doseGrid = doseGrid)), 0)
  for (dose in doseGrid) {
    expect_equal(size(object = cohortSize, dose = dose, data = emptyData), ifelse(dose < 5, 1, 2))
  }
})

## CohortSizeMax ----

test_that("size works as expected for CohortSizeMax", {
  doseGrid <- 1:5
  cohortSize <- CohortSizeMax(
    cohort_size_list = list(
      CohortSizeRange(intervals = c(0, 3), cohort_size = 1:2),
      CohortSizeDLT(dlt_intervals = 0:2, cohort_size = c(1, 3, 6))
    )
  )
  emptyData <- Data(doseGrid = doseGrid)
  noDLT <- Data(x = 1, y = 0, ID = 1, cohort = 1, doseGrid = doseGrid)
  oneDLT <- Data(x = 1, y = 1, ID = 1, cohort = 1, doseGrid = doseGrid)
  twoDLTs <- Data(x = 1:2, y = c(1, 1), ID = 1:2, cohort = 1:2, doseGrid = doseGrid)
  expect_equal(size(cohortSize, NA, Data(doseGrid = doseGrid)), 0)
  for (dose in doseGrid) {
    expect_equal(size(object = cohortSize, dose = dose, data = emptyData), ifelse(dose < 3, 1, 2))
    expect_equal(size(object = cohortSize, dose = dose, data = noDLT), ifelse(dose < 3, 1, 2))
    expect_equal(size(object = cohortSize, dose = dose, data = oneDLT), 3)
    expect_equal(size(object = cohortSize, dose = dose, data = twoDLTs), 6)
  }
})

test_that("maxSize works as expected", {
  size1 <- CohortSizeRange(intervals = c(0, 3), cohort_size = 1:2)
  size2 <- CohortSizeDLT(dlt_intervals = 0:2, cohort_size = c(1, 3, 6))
  cohortSize <- CohortSizeMax(cohort_size_list = list(size1, size2))
  expect_equal(maxSize(size1, size2), cohortSize)
})

## CohortSizeMin ----

test_that("size works as expected for CohortSizeMin", {
  doseGrid <- 1:5
  cohortSize <- CohortSizeMin(
    cohort_size_list = list(
      CohortSizeRange(intervals = c(0, 3), cohort_size = 1:2),
      CohortSizeDLT(dlt_intervals = 0:2, cohort_size = c(1, 3, 6))
    )
  )
  emptyData <- Data(doseGrid = doseGrid)
  noDLT <- Data(x = 1, y = 0, ID = 1, cohort = 1, doseGrid = doseGrid)
  oneDLT <- Data(x = 1, y = 1, ID = 1, cohort = 1, doseGrid = doseGrid)
  twoDLTs <- Data(x = 1:2, y = c(1, 1), ID = 1:2, cohort = 1:2, doseGrid = doseGrid)
  expect_equal(size(cohortSize, NA, Data(doseGrid = doseGrid)), 0)
  for (dose in doseGrid) {
    expect_equal(size(object = cohortSize, dose = dose, data = emptyData), 1)
    expect_equal(size(object = cohortSize, dose = dose, data = noDLT), 1)
    expect_equal(size(object = cohortSize, dose = dose, data = oneDLT), ifelse(dose < 3, 1, 2))
    expect_equal(size(object = cohortSize, dose = dose, data = twoDLTs), ifelse(dose < 3, 1, 2))
  }
})

test_that("size works as expected for CohortSizeMin", {
  doseGrid <- 1:5
  cohortSize <- CohortSizeParts(sizes = c(1, 3))
  expect_equal(size(cohortSize, NA, DataParts(nextPart = 1L)), 0)
  expect_equal(size(cohortSize, NA, DataParts(nextPart = 2L)), 0)
  for (dose in doseGrid) {
    expect_equal(size(object = cohortSize, dose = dose, data = DataParts(nextPart = 1L)), 1)
    expect_equal(size(object = cohortSize, dose = dose, data = DataParts(nextPart = 2L)), 3)
  }
})

test_that("minSize works as expected", {
  size1 <- CohortSizeRange(intervals = c(0, 3), cohort_size = 1:2)
  size2 <- CohortSizeDLT(dlt_intervals = 0:2, cohort_size = c(1, 3, 6))
  cohortSize <- CohortSizeMin(cohort_size_list = list(size1, size2))
  expect_equal(minSize(size1, size2), cohortSize)
})

# SafetyWindow ----

test_that("windowLength works correctly", {
  # Window length depends only on cohort size, so use an empty Data object and
  # an arbitrary dose grid
  emptyData <- Data(doseGrid = 1:5)

  windowLengthVariable <- SafetyWindowSize(
    gap = list(c(7, 3), c(9, 7, 5)),
    size = c(1, 4),
    follow = 7,
    follow_min = 14
  )
  windowLengthConst <- SafetyWindowConst(gap = c(7, 3), follow = 7, follow_min = 14)

  for (d in emptyData@doseGrid) {
    for (cSize in 1:6) {
      cohortSize <- CohortSizeConst(size = cSize)
      sizeRecommendation <- size(cohortSize, dose = d, data = emptyData)

      actual <- windowLength(windowLengthVariable, size = sizeRecommendation)
      expect_equal(names(actual), c("patientGap", "patientFollow", "patientFollowMin"))
      expect_equal(length(actual$patientGap), cSize)
      expect_equal(actual$patientFollow, 7)
      expect_equal(actual$patientFollowMin, 14)
      if (cSize == 1) {
        expectedGaps <- c(0)
      } else if (cSize == 2) {
        expectedGaps <- c(0, 7)
      } else if (cSize == 3) {
        expectedGaps <- c(0, 7, 3)
      } else if (cSize > 3) {
        expectedGaps <- c(0, 9, 7, rep(5, cSize - 3))
      }
      expect_equal(actual$patientGap, expectedGaps)

      actual <- windowLength(windowLengthConst, size = sizeRecommendation)
      expect_equal(names(actual), c("patientGap", "patientFollow", "patientFollowMin"))
      expect_equal(length(actual$patientGap), cSize)
      expect_equal(actual$patientFollow, 7)
      expect_equal(actual$patientFollowMin, 14)
      if (cSize == 1) {
        expectedGaps <- c(0)
      } else if (cSize == 2) {
        expectedGaps <- c(0, 7)
      } else if (cSize > 3) {
        expectedGaps <- c(0, 7, rep(3, cSize - 2))
      }
      expect_equal(actual$patientGap, expectedGaps)
    }
  }
})
