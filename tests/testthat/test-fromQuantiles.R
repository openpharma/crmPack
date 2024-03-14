# h_get_min_inf_beta ----

test_that("h_get_min_inf_beta works as expected with p < q", {
  result <- expect_silent(h_get_min_inf_beta(0.2, 0.5))
  expected <- list(a = 2.322, b = 1)
  expect_equal(result, expected, tolerance = 1e-4)
  expect_identical(result$b, 1)
})

test_that("h_get_min_inf_beta works as expected with p > q", {
  result <- expect_silent(h_get_min_inf_beta(0.7, 0.1))
  expected <- list(a = 1, b = 11.4272)
  expect_equal(result, expected, tolerance = 1e-4)
  expect_identical(result$a, 1)
})

test_that("h_get_min_inf_beta works as expected with p = q", {
  result <- expect_silent(h_get_min_inf_beta(0.1, 0.1))
  expected <- list(a = 1, b = 1)
  expect_identical(result, expected)
})
