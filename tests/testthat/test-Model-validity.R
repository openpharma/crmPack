# validate_general_model ----

test_that("validate_general_model passes for valid object", {
  object <- h_get_general_model()
  expect_true(validate_general_model(object))
})

test_that("validate_general_model returns error message for non-valid object", {
  object <- h_get_general_model()
  # Changing `datanames` so that the arguments of `object@init` are not a subset
  # of the `datanames`.
  object@datanames <- "y"

  expect_equal(
    validate_general_model(object),
    "Arguments of the init function must be data names"
  )
})

# validate_model ----

test_that("validate_model passes for valid object", {
  object <- h_get_model()
  expect_true(validate_model(object))
})

test_that("validate_model returns error message for wrong dose function", {
  object <- h_get_model()
  # Assigning a function with wrong parameter, i.e. not a `prob` and `param1`.
  object@dose <- function(wrong_param) {
    wrong_param
  }

  expect_equal(
    validate_model(object),
    "Arguments of dose function are incorrect"
  )
})

test_that("validate_model returns error message for wrong prob function", {
  object <- h_get_model()
  # Assigning a function with wrong parameter, i.e. not a `dose` and `param1`.
  object@prob <- function(wrong_param) {
    wrong_param
  }

  expect_equal(
    validate_model(object),
    "Arguments of prob function are incorrect"
  )
})
