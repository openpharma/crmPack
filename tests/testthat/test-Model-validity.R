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

# validate_model_normal ----

test_that("validate_model_normal passes for valid object", {
  object <- h_get_model_normal()
  expect_true(validate_model_normal(object))
})

test_that("validate_model_normal returns error for wrong mean and NA", {
  object <- h_get_model_normal()
  # Assigning mean vector of wrong length = 4 != 2, and with NA.
  object@mean <- c(1:3, NA)

  expect_equal(
    validate_model_normal(object),
    "mean must have length 2 and no missing values are allowed"
  )
})

test_that("validate_model_normal returns error for cov with NA", {
  object <- h_get_model_normal()
  # We assign a covariance matrix of wrong dimension and including NA.
  object@cov <- matrix(c(1:3, 4, 5, NA), ncol = 2)

  expect_equal(
    validate_model_normal(object),
    "cov must be 2x2 matrix without any missing values"
  )
})

test_that("validate_model_normal returns error for wrong cov", {
  object <- h_get_model_normal()
  # We assign a matrix which is not a covariance matrix.
  object@cov <- matrix(c(5, 2, 1, 5), ncol = 2)

  expect_equal(
    validate_model_normal(object),
    c(
      "cov must be positive-definite matrix",
      "prec must be inverse of cov"
    )
  )
})

test_that("validate_model_normal returns error for wrong ref_dose", {
  object <- h_get_model_normal()
  # We assign a ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    validate_model_normal(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("validate_model_normal passes for valid object with ref_dose 0", {
  object <- h_get_model_normal()
  object@ref_dose <- 0

  expect_true(validate_model_normal(object))
})
