# check_equal ----
test_that("check_equal works correctly", {
  expect_true(check_equal(1:2, 1:2))
  expect_equal(check_equal(1:2, 2:3), "Not all equal")
  expect_equal(check_equal(Inf, Inf), "Not all entries finite")
  expect_equal(check_equal(NA, 1), "Some entries NA")
  expect_equal(check_equal(0.01, 0.02), "Not all equal")
  expect_true(check_equal(0.01, 0.02, tol = 0.05))
  expect_equal(check_equal(1, c(1, 1)), "Not all of same length")
})

# assert_equal ----
test_that("assert_equal works correctly", {
  expect_invisible(assert_equal(1:2, 1:2))
  expect_error(
    assert_equal(1:2, 2:3),
    "Assertion on 'x' failed: Not all equal."
  )
  expect_error(
    assert_equal(Inf, Inf),
    "Assertion on 'x' failed: Not all entries finite."
  )
  expect_error(assert_equal(NA, 1), "Assertion on 'x' failed: Some entries NA")
  expect_error(
    assert_equal(0.01, 0.02),
    "Assertion on 'x' failed: Not all equal."
  )
  expect_invisible(assert_equal(0.01, 0.02, tol = 0.05))
  expect_error(
    assert_equal(1, c(1, 1)),
    "Assertion on 'x' failed: Not all of same length."
  )
})

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

# check_range ----

test_that("check_range returns TRUE as expected", {
  expect_equal(
    c("x", "lower", "upper", "finite", "unique"),
    formalArgs(check_range)
  )
  expect_true(check_range(c(1, 5)))
  expect_true(check_range(c(0, 5)))
  expect_true(check_range(c(-5, 5)))
  expect_true(check_range(c(-5, -1)))
  expect_true(check_range(c(1, 5), lower = 1, upper = 5))
  expect_true(check_range(c(1, Inf)))
  expect_true(check_range(c(-Inf, 5)))
  expect_true(check_range(c(-Inf, Inf)))
})

test_that("check_range returns TRUE as expected when unique is FALSE", {
  expect_true(check_range(c(1, 5), unique = FALSE))
  expect_true(check_range(c(1, 1), unique = FALSE))
  expect_true(check_range(c(-5, -5), unique = FALSE))
  expect_true(check_range(c(1, 1), 1, 1, unique = FALSE))
  expect_true(check_range(c(1, 1), 1, 1, finite = TRUE, unique = FALSE))
})

test_that("check_range throws the error as expected", {
  expect_error(
    check_range(1:2, lower = "0"),
    "Assertion on 'lower' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    check_range(1:2, upper = 3:4),
    "Assertion on 'upper' failed: Must have length 1."
  )
  expect_error(
    check_range(1:2, unique = 1),
    "Assertion on 'unique' failed: Must be of type 'logical flag', not 'double'."
  )
  expect_error(
    check_range(1:2, finite = 1),
    "Assertion on 'finite' failed: Must be of type 'logical flag', not 'double'."
  )
})

test_that("check_range returns error message as expected", {
  em <- "x must be a valid numerical range."

  expect_identical(
    check_range(c("a", "c")),
    paste(em, "Must be of type 'numeric', not 'character'")
  )
  expect_identical(
    check_range(1),
    paste(em, "Must have length 2, but has length 1")
  )
  expect_identical(
    check_range(c(1, 2, 3)),
    paste(em, "Must have length 2, but has length 3")
  )
  expect_identical(
    check_range(c(1, 1)),
    paste(em, "Contains duplicated values, position 2")
  )
  expect_identical(check_range(c(2, 1)), paste(em, "Must be sorted"))
  expect_identical(
    check_range(c(1, NA)),
    paste(em, "Contains missing values (element 2)")
  )

  # Adding lower or upper bounds.
  expect_identical(
    check_range(c(1, 5), lower = 6),
    paste(em, "Element 1 is not >= 6")
  )
  expect_identical(
    check_range(c(1, 5), upper = 0),
    paste(em, "Element 1 is not <= 0")
  )

  # Restricting to finite.
  expect_identical(
    check_range(c(1, Inf), finite = TRUE),
    paste(em, "Must be finite")
  )
  expect_identical(
    check_range(c(-Inf, 5), finite = TRUE),
    paste(em, "Must be finite")
  )
})

test_that("check_range returns error message as expected when unique is FALSE", {
  em <- "x must be a valid numerical range."

  expect_identical(
    check_range(1, unique = FALSE),
    paste(em, "Must have length 2, but has length 1")
  )
  expect_identical(
    check_range(c(1, 2, 3), unique = FALSE),
    paste(em, "Must have length 2, but has length 3")
  )
  expect_identical(
    check_range(c(2, 1), unique = FALSE),
    paste(em, "Must be sorted")
  )
  expect_identical(
    check_range(c(1, NA), unique = FALSE),
    paste(em, "Contains missing values (element 2)")
  )

  # Adding lower or upper bounds.
  expect_identical(
    check_range(c(1, 5), lower = 6, unique = FALSE),
    paste(em, "Element 1 is not >= 6")
  )
  expect_identical(
    check_range(c(1, 5), upper = 0, unique = FALSE),
    paste(em, "Element 1 is not <= 0")
  )

  # Restricting to finite.
  expect_identical(
    check_range(c(1, Inf), finite = TRUE, unique = FALSE),
    paste(em, "Must be finite")
  )
  expect_identical(
    check_range(c(-Inf, 5), finite = TRUE, unique = FALSE),
    paste(em, "Must be finite")
  )
})
