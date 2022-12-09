# check_probabilities ----

test_that("check_probabilities returns TRUE as expected", {
  expect_equal(
    c("x", "bounds_closed", "len", "unique", "sorted"),
    formalArgs(check_probabilities)
  )
  expect_true(check_probabilities(c(0.2, 0.1, 0.3)))
  expect_true(check_probabilities(c(0, 0.2, 0.1, 0.3)))
  expect_true(check_probabilities(c(0.2, 0.1, 0.3, 1)))
  expect_true(check_probabilities(c(0, 0.2, 0.1, 0.3, 1)))
  expect_true(check_probabilities(c(0.2, 0.1, 0.3), FALSE))
  expect_true(check_probabilities(c(0.2, 0.1, 0.3, 0.9), c(FALSE, TRUE)))
  expect_true(check_probabilities(c(0.2, 0.1, 0.3, 1), c(FALSE, TRUE)))
  expect_true(check_probabilities(c(0.6, 0.2, 0.1, 0.3), c(TRUE, FALSE)))
  expect_true(check_probabilities(c(0, 0.2, 0.1, 0.3), c(TRUE, FALSE)))
  expect_true(check_probabilities(c(0.1, 0.2, 0.3), FALSE, 3, TRUE, TRUE))
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
  expect_equal(
    c("x", "bounds_closed"),
    formalArgs(check_probability)
  )
  expect_true(check_probability(0.2))
  expect_true(check_probability(0))
  expect_true(check_probability(1))
  expect_true(check_probability(0.2, FALSE))
  expect_true(check_probability(0.2, c(FALSE, TRUE)))
  expect_true(check_probability(1, c(FALSE, TRUE)))
  expect_true(check_probability(0.2, c(TRUE, FALSE)))
  expect_true(check_probability(0, c(TRUE, FALSE)))
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
  expect_equal(
    c("x", "bounds_closed"),
    formalArgs(check_probability_range)
  )
  expect_true(check_probability_range(c(0.1, 0.3)))
  expect_true(check_probability_range(c(0, 0.3)))
  expect_true(check_probability_range(c(0.3, 1)))
  expect_true(check_probability_range(c(0, 1)))
  expect_true(check_probability_range(c(0.1, 0.3), FALSE))
  expect_true(check_probability_range(c(0.2, 0.9), c(FALSE, TRUE)))
  expect_true(check_probability_range(c(0.2, 1), c(FALSE, TRUE)))
  expect_true(check_probability_range(c(0.2, 0.9), c(TRUE, FALSE)))
  expect_true(check_probability_range(c(0, 0.9), c(TRUE, FALSE)))
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

# check_length ----

test_that("check_length returns TRUE as expected", {
  expect_equal(
    c("x", "len"),
    formalArgs(check_length)
  )
  # x is of length 1.
  expect_true(check_length(1, 1))
  expect_true(check_length(1, 2))
  expect_true(check_length(1, 15))
  # the second vector is of length 1.
  expect_true(check_length(1:2, 1))
  expect_true(check_length(1:15, 1))
  # x and the second vector are both of length > 1.
  expect_true(check_length(1:2, 2))
  expect_true(check_length(1:15, 15))
})

test_that("check_length returns error message as expected", {
  expect_error(
    check_length(1),
    ".*len.* is missing, with no default"
  )
  expect_error(
    check_length(1, numeric(0)),
    "Assertion on 'len' failed: Must have length 1."
  )
  expect_error(
    check_length(1:5, 1:2),
    "Assertion on 'len' failed: Must have length 1."
  )
  expect_error(
    check_length(1:5, 1.5),
    "Assertion on 'len' failed: Must be of type 'count', not 'double'."
  )
  expect_error(
    check_length(numeric(0), 4),
    "Assertion on 'x_len >= 1L' failed: Must be TRUE."
  )
})
