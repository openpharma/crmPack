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
