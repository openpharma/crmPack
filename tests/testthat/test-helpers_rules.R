# for nextBest methods ----

## some specific helpers ----

### h_info_theory_dist ----

test_that("h_info_theory_dist works as expected for scalars", {
  result <- h_info_theory_dist(0.2, 0.4, 1.2)
  expected <- 0.3298
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("h_info_theory_dist works as expected for vectors", {
  result <- h_info_theory_dist(c(0.5, 0.2), 0.4, 1.2)
  expected <- c(0.04, 0.3298)
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("h_info_theory_dist works as expected for matrices", {
  prob <- matrix(c(0.5, 0.3, 0.7, 0.2), nrow = 2)
  result <- h_info_theory_dist(prob, 0.4, 1.2)
  expected <- matrix(c(0.04, 0.3617, 0.0564, 0.3298), nrow = 2, byrow = TRUE)
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("h_info_theory_dist throws the error for non-conformable args", {
  expect_error(
    h_info_theory_dist(0.2, c(0.4, 0.5), 1.2),
    "Assertion on 'target' failed: Must have length 1."
  )
  expect_error(
    h_info_theory_dist(0.2, 0.4, c(1.2, 1.6)),
    "Assertion on 'asymmetry' failed: Must have length 1."
  )
})

test_that("h_info_theory_dist throws the error for wrong asymmetry", {
  expect_error(
    h_info_theory_dist(0.2, 0.4, 4),
    "Assertion on 'asymmetry' failed: Element 1 is not <= 2."
  )
})

### h_delta_g_yeung ----

test_that("h_delta_g_yeung works as expected", {
  model <- h_get_logistic_indep_beta()
  eff_model <- h_get_eff_log_log()

  result <- h_delta_g_yeung(10, model, eff_model)
  expected <- matrix(c(-0.7573726, -8.2992172, -0.2311984, -247.2734106), ncol = 1)
  expect_equal(result, expected)
})

## next best at grid ----

### h_next_best_mg_doses_at_grid ----

test_that("h_next_best_mg_doses_at_grid works as expected", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)

  result <- h_next_best_mg_doses_at_grid(
    52.3, 42.7, 84, 0.3, seq(25, 200, 25), FALSE, model_dlt, model_eff
  )
  expected <- list(
    next_dose = 50,
    next_dose_drt = 50,
    next_dose_eot = 25,
    next_dose_mg = 75,
    ci_td_eot = c(11.071039, 164.690056),
    ci_ratio_td_eot = 14.8757545,
    ci_dose_mg = c(23.127108, 305.096515),
    ci_ratio_dose_mg = 13.1921604
  )
  expect_equal(result, expected)
})

## plot ----

### h_next_best_tdsamples_plot ----

test_that("h_next_best_tdsamples_plot works as expected", {
  result <- h_next_best_tdsamples_plot(
    target_in_trial_samples = 1:100,
    target_trial_end_samples = 50:150,
    target_in_trial_est = 100,
    target_trial_end_est = 120,
    dose_grid_range = c(25, 300),
    nextBest = h_next_best_tdsamples(),
    doselimit = 75,
    next_best_dose = 60
  )
  vdiffr::expect_doppelganger("h_next_best_tdsamples_plot", result)
})

test_that("h_next_best_tdsamples_plot works as expected (no doselimit)", {
  result <- h_next_best_tdsamples_plot(
    target_in_trial_samples = 1:100,
    target_trial_end_samples = 50:150,
    target_in_trial_est = 100,
    target_trial_end_est = 120,
    dose_grid_range = c(25, 300),
    nextBest = h_next_best_tdsamples(),
    doselimit = Inf,
    next_best_dose = 60
  )
  vdiffr::expect_doppelganger("h_next_best_tdsamples_plot_nodoselim", result)
})

### h_next_best_td_plot ----

test_that("h_next_best_td_plot works as expected", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  prob_dlt <- c(0.11, 0.22, 0.31, 0.37, 0.43, 0.47, 0.5, 0.53, 0.55, 0.57, 0.59, 0.6)
  result <- h_next_best_td_plot(0.33, 80, 0.27, 70, data, prob_dlt, 200, 75)
  vdiffr::expect_doppelganger("h_next_best_td_plot", result)
})

test_that("h_next_best_td_plot works as expected (no doselimit)", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  prob_dlt <- c(0.11, 0.22, 0.31, 0.37, 0.43, 0.47, 0.5, 0.53, 0.55, 0.57, 0.59, 0.6)
  result <- h_next_best_td_plot(0.33, 80, 0.27, 70, data, prob_dlt, Inf, 75)
  vdiffr::expect_doppelganger("h_next_best_td_plot_nodoselim", result)
})

### h_next_best_mg_plot ----

test_that("h_next_best_mg_plot works as expected", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 0)

  result <- h_next_best_mg_plot(
    0.35, 52.3, 0.3, 42.7, 79.8, 0.63, 50, 70, data, model_dlt, model_eff
  )
  vdiffr::expect_doppelganger("h_next_best_mg_plot", result)
})

test_that("h_next_best_mg_plot works as expected (no doselimit)", {
  data <- h_get_data_dual(placebo = FALSE)
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 0)

  result <- h_next_best_mg_plot(
    0.35, 52.3, 0.3, 42.7, 79.8, 0.63, 50, Inf, data, model_dlt, model_eff
  )
  vdiffr::expect_doppelganger("h_next_best_mg_plot_nodoselim", result)
})

### h_next_best_mgsamples_plot ----

test_that("h_next_best_mgsamples_plot works as expected", {
  result <- h_next_best_mgsamples_plot(
    prob_target_drt = 0.45,
    dose_target_drt = 104,
    prob_target_eot = 0.4,
    dose_target_eot = 83,
    dose_mg = 200,
    dose_mg_samples = c(100, 300, 250, 125, 100, 175, 50, 300, 300, 150),
    next_dose = 75,
    doselimit = 80,
    dose_grid_range = c(50, 320)
  )
  vdiffr::expect_doppelganger("h_next_best_mgsamples_plot", result)
})

test_that("h_next_best_mgsamples_plot works as expected (no doselimit)", {
  result <- h_next_best_mgsamples_plot(
    prob_target_drt = 0.35,
    dose_target_drt = 67.5,
    prob_target_eot = 0.3,
    dose_target_eot = 53,
    dose_mg = 125,
    dose_mg_samples = c(300, 225, 50, 175, 75, 125, 25, 125, 125, 250),
    next_dose = 50,
    doselimit = Inf,
    dose_grid_range = c(25, 300)
  )
  vdiffr::expect_doppelganger("h_next_best_mgsamples_plot_nodoselim", result)
})
