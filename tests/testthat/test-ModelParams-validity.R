# v_model_params_normal ----

test_that("v_model_params_normal passes for valid object", {
  object <- h_get_model_params_normal()
  expect_true(v_model_params_normal(object))
})

test_that("v_model_params_normal returns error for wrong mean and NA", {
  object <- h_get_model_params_normal()
  # Assigning mean vector of wrong length = 4 != 2, and with NA.
  object@mean <- c(1:3, NA)

  expect_equal(
    v_model_params_normal(object),
    "mean must have length 2 and no missing values are allowed"
  )
})

test_that("v_model_params_normal returns error for cov with NA", {
  object <- h_get_model_params_normal()
  # We assign a covariance matrix of wrong dimension and including NA.
  object@cov <- matrix(c(1:3, 4, 5, NA), ncol = 2)

  expect_equal(
    v_model_params_normal(object),
    "cov must be 2x2 positive-definite matrix without any missing values"
  )
})

test_that("v_model_params_normal returns error for prec with NA", {
  object <- h_get_model_params_normal()
  # We assign a precision matrix of wrong dimension and including NA.
  object@prec <- matrix(c(1:3, 4, 5, NA), ncol = 2)

  expect_equal(
    v_model_params_normal(object),
    "prec must be 2x2 positive-definite matrix without any missing values"
  )
})

test_that("v_model_params_normal returns error for wrong cov", {
  object <- h_get_model_params_normal()
  # We assign a matrix which is not a covariance matrix.
  object@cov <- matrix(c(5, 2, 1, 5), ncol = 2)

  expect_equal(
    v_model_params_normal(object),
    "cov must be 2x2 positive-definite matrix without any missing values"
  )
})

test_that("v_model_params_normal returns error for wrong prec", {
  object <- h_get_model_params_normal()
  # We assign a precision matrix which is not a covariance matrix.
  object@prec <- matrix(c(5, 2, 1, 5), ncol = 2)

  expect_equal(
    v_model_params_normal(object),
    "prec must be 2x2 positive-definite matrix without any missing values"
  )
})

test_that("v_model_params_normal returns error for wrong prec (not an inverse of cov)", {
  object <- h_get_model_params_normal()
  # We assign a precision matrix which is not an inverse of cov.
  object@prec <- matrix(c(5, 1, 1, 5), ncol = 2)

  expect_equal(
    v_model_params_normal(object),
    "prec must be inverse of cov"
  )
})
