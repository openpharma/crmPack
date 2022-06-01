# h_info_theory_dist ----

test_that("h_info_theory_dist works as expected", {
  # Values calculated using a different program.
  p <- c(0.01, 0.2, 0.7)
  gamma <- c(0.5, 0, 0.3)
  a <- c(1, 1.8, 0.5)
  expected <- c(24.25, 0.76, 1.16)
  calculated <- h_info_theory_dist(p, gamma, a)
  expect_equal(calculated, expected, tolerance = 0.01)
})
