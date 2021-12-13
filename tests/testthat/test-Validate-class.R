# Validate-class ----

test_that("Validate objects can be created with default constructor Validate", {
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))
})

# Validate ----

test_that("Validate returns NULL for valid object, or message for an invalid one", {
  erra <- "some_error_A"
  errb <- "some_error_B"

  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, erra))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, erra), erra)
  expect_identical(o$msg, erra)

  expect_identical(o$check(FALSE, errb), c(erra, errb))
  expect_null(o$check(TRUE, errb))
})

test_that("Having a msg global variable does not confuse the constructor method (1)", {
  msg <- character(0)
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))

  msg <- "some msg"
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))
})

test_that("Having a msg global variable does not confuse the constructor method (2)", {
  erra <- "some_error_A"
  errb <- "some_error_B"

  msg <- "some msg"
  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, erra))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, erra), erra)
  expect_identical(o$msg, erra)

  expect_identical(o$check(FALSE, errb), c(erra, errb))
  expect_null(o$check(TRUE, errb))
})

test_that("Having a msg global variable does not confuse the constructor method (3)", {
  erra <- "some_error_A"
  errb <- "some_error_B"

  o <- Validate()
  msg <- "some msg"

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, erra))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, erra), erra)
  expect_identical(o$msg, erra)

  expect_identical(o$check(FALSE, errb), c(erra, errb))
  expect_null(o$check(TRUE, errb))
})

test_that("Having a msg global variable does not confuse the constructor method (4)", {
  erra <- "some_error_A"
  errb <- "some_error_B"

  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, erra))
  expect_identical(o$msg, character(0))

  expect_identical(o$check(FALSE, erra), erra)
  expect_identical(o$msg, erra)

  msg <- "some msg"
  expect_identical(o$check(FALSE, errb), c(erra, errb))
  expect_null(o$check(TRUE, errb))
})
