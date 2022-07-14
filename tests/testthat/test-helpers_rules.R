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
