# Validate-class ----

test_that("Validate objects can be created with default constructor Validate", {
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))
})

# Validate ----

test_that("Validate returns NULL for valid object, or message for inv. obj.", {
  error_a <- "some_error_A"
  error_b <- "some_error_B"

  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, error_a))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, error_a), error_a)
  expect_identical(o$msg, error_a)

  expect_identical(o$check(FALSE, error_b), c(error_a, error_b))
  expect_null(o$check(TRUE, error_b))
})

test_that("Having a msg global variable does not confuse the constructor (1)", {
  msg <- character(0)
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))

  msg <- "some msg"
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))
})

test_that("Having a msg global variable does not confuse the constructor (2)", {
  error_a <- "some_error_A"
  error_b <- "some_error_B"

  msg <- "some msg"
  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, error_a))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, error_a), error_a)
  expect_identical(o$msg, error_a)

  expect_identical(o$check(FALSE, error_b), c(error_a, error_b))
  expect_null(o$check(TRUE, error_b))
})

test_that("Having a msg global variable does not confuse the constructor (3)", {
  error_a <- "some_error_A"
  error_b <- "some_error_B"

  o <- Validate()
  msg <- "some msg"

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, error_a))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, error_a), error_a)
  expect_identical(o$msg, error_a)

  expect_identical(o$check(FALSE, error_b), c(error_a, error_b))
  expect_null(o$check(TRUE, error_b))
})

test_that("Having a msg global variable does not confuse the constructor (4)", {
  error_a <- "some_error_A"
  error_b <- "some_error_B"

  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, error_a))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, error_a), error_a)
  expect_identical(o$msg, error_a)

  msg <- "some msg"
  expect_identical(o$check(FALSE, error_b), c(error_a, error_b))
  expect_null(o$check(TRUE, error_b))
})
