# check_probability ----

test_that("check_probability returns TRUE as expected", {
  expect_true(
    check_probability(c(0.2, 0.1, 0.3))
  )
  expect_true(
    check_probability(c(0, 0.2, 0.1, 0.3))
  )
  expect_true(
    check_probability(c(0.2, 0.1, 0.3, 1))
  )
  expect_true(
    check_probability(c(0, 0.2, 0.1, 0.3, 1))
  )
  expect_true(
    check_probability(c(0.2, 0.1, 0.3), bounds_closed = FALSE)
  )
  expect_true(
    check_probability(c(0.2, 0.1, 0.3, 0.9), bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_probability(c(0.2, 0.1, 0.3, 1), bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_probability(c(0.6, 0.2, 0.1, 0.3), bounds_closed = c(TRUE, FALSE))
  )
  expect_true(
    check_probability(c(0, 0.2, 0.1, 0.3), bounds_closed = c(TRUE, FALSE))
  )
  expect_true(
    check_probability(c(0.1, 0.2, 0.3), len = 3, sorted = TRUE)
  )
})

test_that("check_probability returns error message as expected", {
  expect_identical(
    check_probability(c(-0.1, 0.2, 0.1)),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability(c(-0.1, 0.2, 0.1), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0.2, 0.1, 0.3, 1), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0, 0.2, 0.1, 0.3), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0, 0.2, 0.1, 0.3, 1), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0, 0.2, 0.1, 0.3, 1), bounds_closed = c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability(c(0, 0.2, 0.1, 0.3), bounds_closed = c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability(c(0, 0.2, 0.1, 0.3, 1), bounds_closed = c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0.2, 0.1, 0.3, 1), bounds_closed = c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0.2, 0.1, 0.3), len = 2),
    "Must have length 2, but has length 3"
  )
  expect_identical(
    check_probability(c(0.2, 0.1, 0.3), sorted = TRUE),
    "Must be sorted"
  )
})

# check_prob ----

test_that("check_prob returns TRUE as expected", {
  expect_true(
    check_prob(0.2)
  )
  expect_true(
    check_prob(0)
  )
  expect_true(
    check_prob(1)
  )
  expect_true(
    check_prob(0.2, bounds_closed = FALSE)
  )
  expect_true(
    check_prob(0.2, bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_prob(1, bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_prob(0.2, bounds_closed = c(TRUE, FALSE))
  )
  expect_true(
    check_prob(0, bounds_closed = c(TRUE, FALSE))
  )
})

test_that("check_prob returns error message as expected", {
  expect_identical(
    check_prob(-0.1),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_prob(-0.1, bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob(0, bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob(1, bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob(0, bounds_closed = c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_prob(1, bounds_closed = c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob(c(0.2, 0.1, 0.3)),
    "Must have length 1, but has length 3"
  )
})

# check_prob_range ----

test_that("check_prob_range returns TRUE as expected", {
  expect_true(
    check_prob_range(c(0.1, 0.3))
  )
  expect_true(
    check_prob_range(c(0, 0.3))
  )
  expect_true(
    check_prob_range(c(0.3, 1))
  )
  expect_true(
    check_prob_range(c(0, 1))
  )
  expect_true(
    check_prob_range(c(0.1, 0.3), bounds_closed = FALSE)
  )
  expect_true(
    check_prob_range(c(0.2, 0.9), bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_prob_range(c(0.2, 1), bounds_closed = c(FALSE, TRUE))
  )
  expect_true(
    check_prob_range(c(0.2, 0.9), bounds_closed = c(TRUE, FALSE))
  )
  expect_true(
    check_prob_range(c(0, 0.9), bounds_closed = c(TRUE, FALSE))
  )
})

test_that("check_prob_range returns error message as expected", {
  expect_identical(
    check_prob_range(c(-0.1, 0.9)),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(-0.1, 0.9), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(0, 0.9), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(0.1, 1), bounds_closed = FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(0, 1), bounds_closed = c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(0, 1), bounds_closed = c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_prob_range(c(0.2, 0.1, 0.3)),
    "Must have length 2, but has length 3"
  )
  expect_identical(
    check_prob_range(c(0.2, 0.1)),
    "Must be sorted"
  )
})
