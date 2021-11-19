# Validate-class ----

test_that("Validate objects can be created with default constructor Validate", {
  result <- expect_silent(Validate())
  expect_s4_class(result, "Validate")
  expect_true(validObject(result))
})

# Validate ----

test_that("Validate returns NULL for valid object, or message for invalid object", {
  
  A <- "some_err_A"
  B <- "some_err_B"

  o <- Validate()
  
  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, A))
  expect_identical(o$msg, character(0))
  
  expect_identical(o$check(FALSE, A), A)
  expect_identical(o$msg, A)
  
  expect_identical(o$check(FALSE, B), c(A, B))
  expect_null(o$check(TRUE, B))
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
  
  A <- "some_err_A"
  B <- "some_err_B"

  msg <- "some msg"
  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, A))
  expect_identical(o$msg, character(0))
  
  expect_identical(o$check(FALSE, A), A)
  expect_identical(o$msg, A)
  
  expect_identical(o$check(FALSE, B), c(A, B))
  expect_null(o$check(TRUE, B))
})

test_that("Having a msg global variable does not confuse the constructor method (3)", {
  
  A <- "some_err_A"
  B <- "some_err_B"
  
  o <- Validate()
  msg <- "some msg"
  
  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, A))
  expect_identical(o$msg, character(0))
  
  expect_identical(o$check(FALSE, A), A)
  expect_identical(o$msg, A)
  
  expect_identical(o$check(FALSE, B), c(A, B))
  expect_null(o$check(TRUE, B))
})

test_that("Having a msg global variable does not confuse the constructor method (4)", {
  
  A <- "some_err_A"
  B <- "some_err_B"
  
  o <- Validate()

  expect_identical(o$msg, character(0))
  expect_null(o$check(TRUE, A))
  expect_identical(o$msg, character(0))
  
  expect_identical(o$check(FALSE, A), A)
  expect_identical(o$msg, A)

  msg <- "some msg"
  expect_identical(o$check(FALSE, B), c(A, B))
  expect_null(o$check(TRUE, B))
})
