# h_all_equivalent ----

test_that("h_all_equivalent returns TRUE for equivalent objects", {
  target <- structure(c(1, 2, 3.1), names = letters[1:3], some_attr = "some_attr")
  current <- structure(c(1, 2, 3.1), names = letters[4:6], some_attr = "some_attr1")

  result <- h_all_equivalent(target, current)
  expect_true(result)
})

test_that("h_all_equivalent returns TRUE for equivalent objects up to some tollerance", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.3)
  # Mean relative difference: 0.2 < tolerance = 0.3
  expect_true(result)
})

test_that("h_all_equivalent returns FALSE for non-equivalent objects", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.1)
  # Mean relative difference: 0.2 > tolerance = 0.1
  expect_false(result)
})
