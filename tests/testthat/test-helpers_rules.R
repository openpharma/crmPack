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

### h_next_best_mg_ci ----

test_that("h_next_best_mg_ci works as expected", {
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)

  result <- h_next_best_mg_ci(42.7, 84, 0.3, FALSE, model_dlt, model_eff)
  expected <- list(
    ci_dose_target = c(11.071039, 164.690056),
    ci_ratio_dose_target = 14.8757545,
    ci_dose_mg = c(23.127108, 305.096515),
    ci_ratio_dose_mg = 13.1921604
  )
  expect_equal(result, expected)
})

test_that("h_next_best_mg_ci works as expected (with placebo)", {
  model_dlt <- h_get_logistic_indep_beta()
  model_eff <- h_get_eff_log_log(const = 5)

  result <- h_next_best_mg_ci(42.7, 84, 0.3, TRUE, model_dlt, model_eff)
  expected <- list(
    ci_dose_target = c(11.071039, 164.690056),
    ci_ratio_dose_target = 14.8757545,
    ci_dose_mg = c(26.97598, 293.63161),
    ci_ratio_dose_mg = 10.88493
  )
  expect_equal(result, expected, tolerance = 1e-6)
})

## next best at grid ----

### h_next_best_mg_doses_at_grid ----

test_that("h_next_best_mg_doses_at_grid works as expected", {
  result <- h_next_best_mg_doses_at_grid(52.3, 42.7, 84, seq(25, 200, 25), 100, FALSE)
  expected <- list(
    next_dose = 50,
    next_dose_drt = 50,
    next_dose_eot = 25,
    next_dose_mg = 75
  )
  expect_equal(result, expected)
})

test_that("h_next_best_mg_doses_at_grid works as expected (small doselimit)", {
  result <- h_next_best_mg_doses_at_grid(52.3, 42.7, 84, seq(25, 200, 25), 49, FALSE)
  expected <- list(
    next_dose = 25,
    next_dose_drt = 25,
    next_dose_eot = 25,
    next_dose_mg = 25
  )
  expect_equal(result, expected)
})

test_that("h_next_best_mg_doses_at_grid works as expected (small doselimit, placebo)", {
  result <- h_next_best_mg_doses_at_grid(24, 42.7, 84, c(0.001, seq(25, 200, 25)), 49, TRUE)
  expected <- list(
    next_dose = NA_real_,
    next_dose_drt = NA_real_,
    next_dose_eot = 25,
    next_dose_mg = 25
  )
  expect_equal(result, expected)
})

test_that("h_next_best_mg_doses_at_grid works as expected (td > mg)", {
  result <- h_next_best_mg_doses_at_grid(94, 42.7, 84, seq(25, 200, 25), 100, FALSE)
  expected <- list(
    next_dose = 75,
    next_dose_drt = 75,
    next_dose_eot = 25,
    next_dose_mg = 75
  )
  expect_equal(result, expected)
})

## eligible doses ----

### h_next_best_eligible_doses ----

test_that("h_next_best_eligible_doses works as expected", {
  dose_grid <- c(0.001, seq(25, 200, 25))

  # doses
  expect_identical(h_next_best_eligible_doses(dose_grid, 79, TRUE), dose_grid[2:4])
  expect_identical(h_next_best_eligible_doses(dose_grid, 250, TRUE), dose_grid[-1])
  expect_identical(h_next_best_eligible_doses(dose_grid, 200, TRUE), dose_grid[-1])
  expect_identical(h_next_best_eligible_doses(dose_grid, 75, TRUE), dose_grid[2:4])
  expect_identical(h_next_best_eligible_doses(dose_grid, 0.001, TRUE), dose_grid[1])
  expect_identical(h_next_best_eligible_doses(dose_grid, 0.00001, TRUE), numeric(0))

  # levels
  ftttf <- c(FALSE, rep(TRUE, 3), rep(FALSE, 5))
  ft <- c(FALSE, rep(TRUE, 8))
  expect_identical(h_next_best_eligible_doses(dose_grid, 79, TRUE, TRUE), ftttf)
  expect_identical(h_next_best_eligible_doses(dose_grid, 250, TRUE, TRUE), ft)
  expect_identical(h_next_best_eligible_doses(dose_grid, 200, TRUE, TRUE), ft)
  expect_identical(h_next_best_eligible_doses(dose_grid, 75, TRUE, TRUE), ftttf)
  expect_identical(h_next_best_eligible_doses(dose_grid, 0.001, TRUE, TRUE), !ft)
  expect_identical(h_next_best_eligible_doses(dose_grid, 0.00001, TRUE, TRUE), rep(FALSE, 9))
})

test_that("h_next_best_eligible_doses works as expected (no placebo)", {
  dose_grid <- seq(25, 200, 25)

  # doses
  expect_identical(h_next_best_eligible_doses(dose_grid, 79, FALSE), dose_grid[1:3])
  expect_identical(h_next_best_eligible_doses(dose_grid, 250, FALSE), dose_grid)
  expect_identical(h_next_best_eligible_doses(dose_grid, 200, FALSE), dose_grid)
  expect_identical(h_next_best_eligible_doses(dose_grid, 75, FALSE), dose_grid[1:3])
  expect_identical(h_next_best_eligible_doses(dose_grid, 1, FALSE), numeric(0))

  # levels
  tttf <- c(rep(TRUE, 3), rep(FALSE, 5))
  all_true <- rep(TRUE, 8)
  expect_identical(h_next_best_eligible_doses(dose_grid, 79, FALSE, TRUE), tttf)
  expect_identical(h_next_best_eligible_doses(dose_grid, 250, FALSE, TRUE), all_true)
  expect_identical(h_next_best_eligible_doses(dose_grid, 200, FALSE, TRUE), all_true)
  expect_identical(h_next_best_eligible_doses(dose_grid, 75, FALSE, TRUE), tttf)
  expect_identical(h_next_best_eligible_doses(dose_grid, 1, FALSE, TRUE), !all_true)
})

test_that("h_next_best_eligible_doses throws the error for empty dose grid or not sorted", {
  expect_error(
    h_next_best_eligible_doses(numeric(0), 80, FALSE),
    "Assertion on 'dose_grid' failed: Must have length >= 1, but has length 0."
  )
  expect_error(
    h_next_best_eligible_doses(c(2, 1), 80, FALSE),
    "Assertion on 'dose_grid' failed: Must be sorted."
  )
})

## plot ----

### h_next_best_tdsamples_plot ----

test_that("h_next_best_tdsamples_plot works as expected", {
  result <- h_next_best_tdsamples_plot(
    1:100, 50:150, 100, 120, c(25, 300), h_next_best_tdsamples(), 75, 60
  )
  vdiffr::expect_doppelganger("h_next_best_tdsamples_plot", result)
})

test_that("h_next_best_tdsamples_plot works as expected (no doselimit)", {
  result <- h_next_best_tdsamples_plot(
    1:100, 50:150, 100, 120, c(25, 300), h_next_best_tdsamples(), Inf, 60
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
    0.45, 104, 0.4, 83, 200, c(100, 300, 250, 125, 100, 175, 50, 300, 300, 150), 75, 80, c(50, 320)
  )
  vdiffr::expect_doppelganger("h_next_best_mgsamples_plot", result)
})

test_that("h_next_best_mgsamples_plot works as expected (no doselimit)", {
  result <- h_next_best_mgsamples_plot(
    0.35, 67.5, 0.3, 53, 125, c(300, 225, 50, 175, 75, 125, 25, 125, 125, 250), 50, Inf, c(25, 300)
  )
  vdiffr::expect_doppelganger("h_next_best_mgsamples_plot_nodoselim", result)
})
