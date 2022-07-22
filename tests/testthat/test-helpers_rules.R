# h_info_theory_dist ----

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

# h_next_best_tdsamples_plot ----

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

# h_next_best_td_plot ----

test_that("h_next_best_td_plot works as expected", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  result <- h_next_best_td_plot(
    prob_target_drt = 0.33,
    dose_target_drt = 80,
    prob_target_eot = 0.27,
    dose_target_eot = 70,
    data = data,
    prob_dlt = c(0.11, 0.22, 0.31, 0.37, 0.43, 0.47, 0.5, 0.53, 0.55, 0.57, 0.59, 0.6),
    doselimit = 200,
    next_dose = 75
  )
  vdiffr::expect_doppelganger("h_next_best_td_plot", result)
})

test_that("h_next_best_td_plot works as expected (no doselimit)", {
  data <- h_get_data(empty = TRUE, placebo = FALSE)
  result <- h_next_best_td_plot(
    prob_target_drt = 0.33,
    dose_target_drt = 80,
    prob_target_eot = 0.27,
    dose_target_eot = 70,
    data = data,
    prob_dlt = c(0.11, 0.22, 0.31, 0.37, 0.43, 0.47, 0.5, 0.53, 0.55, 0.57, 0.59, 0.6),
    doselimit = Inf,
    next_dose = 75
  )
  vdiffr::expect_doppelganger("h_next_best_td_plot_nodoselim", result)
})
