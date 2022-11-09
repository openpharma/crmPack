# check_probabilities ----

test_that("check_probabilities returns TRUE as expected", {
  expect_true(
    check_probabilities(c(0.2, 0.1, 0.3))
  )
  expect_true(
    check_probabilities(c(0, 0.2, 0.1, 0.3))
  )
  expect_true(
    check_probabilities(c(0.2, 0.1, 0.3, 1))
  )
  expect_true(
    check_probabilities(c(0, 0.2, 0.1, 0.3, 1))
  )
  expect_true(
    check_probabilities(c(0.2, 0.1, 0.3), FALSE)
  )
  expect_true(
    check_probabilities(c(0.2, 0.1, 0.3, 0.9), c(FALSE, TRUE))
  )
  expect_true(
    check_probabilities(c(0.2, 0.1, 0.3, 1), c(FALSE, TRUE))
  )
  expect_true(
    check_probabilities(c(0.6, 0.2, 0.1, 0.3), c(TRUE, FALSE))
  )
  expect_true(
    check_probabilities(c(0, 0.2, 0.1, 0.3), c(TRUE, FALSE))
  )
  expect_true(
    check_probabilities(c(0.1, 0.2, 0.3), FALSE, 3, TRUE, TRUE)
  )
})

test_that("check_probabilities returns error message as expected", {
  expect_identical(
    check_probabilities(c(-0.1, 0.2, 0.1)),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(-0.1, 0.2, 0.1), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0.2, 0.1, 0.3, 1), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0, 0.2, 0.1, 0.3), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0, 0.2, 0.1, 0.3, 1), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0, 0.2, 0.1, 0.3, 1), c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0, 0.2, 0.1, 0.3), c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0, 0.2, 0.1, 0.3, 1), c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0.2, 0.1, 0.3, 1), c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probabilities(c(0.2, 0.1, 0.3), TRUE, 2),
    "Must have length 2, but has length 3"
  )
  expect_identical(
    check_probabilities(c(0.1, 0.2, 0.2), TRUE, 3, TRUE),
    "Contains duplicated values, position 3"
  )
  expect_identical(
    check_probabilities(c(0.2, 0.1, 0.3), TRUE, 3, TRUE, TRUE),
    "Must be sorted"
  )
})

# check_probability ----

test_that("check_probability returns TRUE as expected", {
  expect_true(
    check_probability(0.2)
  )
  expect_true(
    check_probability(0)
  )
  expect_true(
    check_probability(1)
  )
  expect_true(
    check_probability(0.2, FALSE)
  )
  expect_true(
    check_probability(0.2, c(FALSE, TRUE))
  )
  expect_true(
    check_probability(1, c(FALSE, TRUE))
  )
  expect_true(
    check_probability(0.2, c(TRUE, FALSE))
  )
  expect_true(
    check_probability(0, c(TRUE, FALSE))
  )
})

test_that("check_probability returns error message as expected", {
  expect_identical(
    check_probability(-0.1),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability(-0.1, FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(0, FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(1, FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(0, c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability(1, c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability(c(0.2, 0.1, 0.3)),
    "Must have length 1, but has length 3"
  )
})

# check_probability_range ----

test_that("check_probability_range returns TRUE as expected", {
  expect_true(
    check_probability_range(c(0.1, 0.3))
  )
  expect_true(
    check_probability_range(c(0, 0.3))
  )
  expect_true(
    check_probability_range(c(0.3, 1))
  )
  expect_true(
    check_probability_range(c(0, 1))
  )
  expect_true(
    check_probability_range(c(0.1, 0.3), FALSE)
  )
  expect_true(
    check_probability_range(c(0.2, 0.9), c(FALSE, TRUE))
  )
  expect_true(
    check_probability_range(c(0.2, 1), c(FALSE, TRUE))
  )
  expect_true(
    check_probability_range(c(0.2, 0.9), c(TRUE, FALSE))
  )
  expect_true(
    check_probability_range(c(0, 0.9), c(TRUE, FALSE))
  )
})

test_that("check_probability_range returns error message as expected", {
  expect_identical(
    check_probability_range(c(-0.1, 0.9)),
    "Probability must be within [0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(-0.1, 0.9), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(0, 0.9), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(0.1, 1), FALSE),
    "Probability must be within (0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(0, 1), c(FALSE, TRUE)),
    "Probability must be within (0, 1] bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(0, 1), c(TRUE, FALSE)),
    "Probability must be within [0, 1) bounds but it is not"
  )
  expect_identical(
    check_probability_range(c(0.2, 0.1, 0.3)),
    "Must have length 2, but has length 3"
  )
  expect_identical(
    check_probability_range(c(0.2, 0.1)),
    "Must be sorted"
  )
})
