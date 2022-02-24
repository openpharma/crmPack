# v_general_model ----

test_that("v_general_model passes for valid object", {
  object <- h_get_general_model()
  expect_true(v_general_model(object))
})

test_that("v_general_model returns error message for non-valid object", {
  object <- h_get_general_model()
  # Changing `datanames` so that the arguments of `object@init` are not a subset
  # of the `datanames`.
  object@datanames <- "y"

  expect_equal(
    v_general_model(object),
    "Arguments of the init function must be data names"
  )
})

# v_model ----

test_that("v_model passes for valid object", {
  object <- h_get_model()
  expect_true(v_model(object))
})

test_that("v_model returns error message for wrong dose function", {
  object <- h_get_model()
  # Assigning a function with wrong parameter, i.e. not a `prob` and `param1`.
  object@dose <- function(wrong_param) {
    wrong_param
  }

  expect_equal(
    v_model(object),
    "Arguments of dose function are incorrect"
  )
})

test_that("v_model returns error message for wrong prob function", {
  object <- h_get_model()
  # Assigning a function with wrong parameter, i.e. not a `dose` and `param1`.
  object@prob <- function(wrong_param) {
    wrong_param
  }

  expect_equal(
    v_model(object),
    "Arguments of prob function are incorrect"
  )
})

# v_model_log_normal ----

test_that("v_model_log_normal passes for valid object", {
  object <- h_get_model_log_normal()
  expect_true(v_model_log_normal(object))
})

test_that("v_model_log_normal returns error for wrong mean and NA", {
  object <- h_get_model_log_normal()
  # Assigning mean vector of wrong length = 4 != 2, and with NA.
  object@mean <- c(1:3, NA)

  expect_equal(
    v_model_log_normal(object),
    "mean must have length 2 and no missing values are allowed"
  )
})

test_that("v_model_log_normal returns error for cov with NA", {
  object <- h_get_model_log_normal()
  # We assign a covariance matrix of wrong dimension and including NA.
  object@cov <- matrix(c(1:3, 4, 5, NA), ncol = 2)

  expect_equal(
    v_model_log_normal(object),
    "cov must be 2x2 matrix without any missing values"
  )
})

test_that("v_model_log_normal returns error for wrong cov", {
  object <- h_get_model_log_normal()
  # We assign a matrix which is not a covariance matrix.
  object@cov <- matrix(c(5, 2, 1, 5), ncol = 2)

  expect_equal(
    v_model_log_normal(object),
    c(
      "cov must be positive-definite matrix",
      "prec must be inverse of cov"
    )
  )
})

test_that("v_model_log_normal returns error for wrong prec", {
  object <- h_get_model_log_normal()
  # We assign a matrix which is not an inverse of `cov`.
  object@prec <- matrix(c(5, 4, 1, 5), ncol = 2)

  expect_equal(
    v_model_log_normal(object),
    "prec must be inverse of cov"
  )
})

test_that("v_model_log_normal returns error for wrong ref_dose", {
  object <- h_get_model_log_normal()
  # We assign a ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    v_model_log_normal(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("v_model_log_normal passes for valid object with ref_dose 0", {
  object <- h_get_model_log_normal()
  object@ref_dose <- 0

  expect_true(v_model_log_normal(object))
})

# v_model_logistic_kadane ----

test_that("v_model_logistic_kadane passes for valid object", {
  object <- h_get_logistic_kadane()
  expect_true(v_model_logistic_kadane(object))
})

test_that("v_model_logistic_kadane returns error for wrong theta probability", {
  object <- h_get_logistic_kadane()
  err_msg <- "theta must be a probability scalar > 0 and < 1"
  # Assigning wrong values for probability theta.
  object@theta <- -1
  expect_equal(v_model_logistic_kadane(object), err_msg)
  object@theta <- 5
  expect_equal(v_model_logistic_kadane(object), err_msg)
  object@theta <- 0
  expect_equal(v_model_logistic_kadane(object), err_msg)
})

test_that("v_model_logistic_kadane returns error for non-scalars", {
  object <- h_get_logistic_kadane()
  # Assigning vectors for scalar slots.
  object@theta <- c(0.4, 0.5)
  object@xmin <- 1:4
  object@xmax <- 2:5

  expect_equal(
    v_model_logistic_kadane(object),
    c(
      "theta must be a probability scalar > 0 and < 1",
      "xmin must be scalar",
      "xmax must be scalar"
    )
  )
})

test_that("v_model_logistic_kadane returns error for xmin greater than xmax", {
  object <- h_get_logistic_kadane()
  # Assigning vectors for scalar slots.
  object@xmin <- 1
  object@xmax <- 1
  expect_equal(
    v_model_logistic_kadane(object),
    "xmin must be strictly smaller than xmax"
  )
  object@xmin <- 2
  object@xmax <- 1
  expect_equal(
    v_model_logistic_kadane(object),
    "xmin must be strictly smaller than xmax"
  )
})

# v_model_logistic_normal_mixture ----

test_that("v_model_logistic_normal_mixture passes for valid object", {
  object <- h_get_logistic_normal_mixture()
  expect_true(v_model_logistic_normal_mixture(object))
})

test_that("v_model_logistic_normal_mixture returns error for wrong mean and NA", {
  object <- h_get_logistic_normal_mixture()
  # Assigning mean vector of wrong length = 4 != 2, and with NA.
  object@comp1$mean <- c(1:3, NA)
  object@comp2$mean <- c(1:4, NA)

  expect_equal(
    v_model_logistic_normal_mixture(object),
    rep("mean must have length 2 and no missing values are allowed", 2)
  )
})

test_that("v_model_logistic_normal_mixture returns error for cov with NA", {
  object <- h_get_logistic_normal_mixture()
  # We assign a covariance matrix of wrong dimension and including NA.
  object@comp1$cov <- matrix(c(1:3, 4, 5, NA), ncol = 2)
  object@comp2$cov <- matrix(c(1:3, 4, 5, NA), ncol = 2)

  expect_equal(
    v_model_logistic_normal_mixture(object),
    rep("cov must be 2x2 matrix without any missing values", 2)
  )
})

test_that("v_model_logistic_normal_mixture returns error for wrong cov", {
  object <- h_get_logistic_normal_mixture()
  # We assign a matrix which is not a covariance matrix.
  object@comp1$cov <- matrix(c(5, 2, 1, 5), ncol = 2)
  object@comp2$cov <- matrix(c(5, 2, 1, 9), ncol = 2)

  expect_equal(
    v_model_logistic_normal_mixture(object),
    rep(
      c(
        "cov must be positive-definite matrix",
        "prec must be inverse of cov"
      ),
      2
    )
  )
})

test_that("v_model_logistic_normal_mixture returns error for wrong prec", {
  object <- h_get_logistic_normal_mixture()
  # We assign a matrix which is not an inverse of `cov`.
  object@comp1$prec <- matrix(c(5, 2, 1, 5), ncol = 2)
  object@comp2$prec <- matrix(c(5, 2, 1, 9), ncol = 2)

  expect_equal(
    v_model_logistic_normal_mixture(object),
    rep("prec must be inverse of cov", 2)
  )
})

test_that("v_model_logistic_normal_mixture returns error for wrong ref_dose", {
  object <- h_get_logistic_normal_mixture()
  # We assign a ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    v_model_logistic_normal_mixture(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("v_model_logistic_normal_mixture passes for valid object with ref_dose 0", {
  object <- h_get_logistic_normal_mixture()
  object@ref_dose <- 0

  expect_true(v_model_logistic_normal_mixture(object))
})

test_that("v_model_logistic_normal_mixture returns error for wrong weightpar", {
  object <- h_get_logistic_normal_mixture()
  err_msg <- "weightpar must be a numerical vector of length two with values greater than 0"
  # Assigning wrong values for weightpar.
  object@weightpar <- c(a = -1, b = -2)
  expect_equal(v_model_logistic_normal_mixture(object), err_msg)
  object@weightpar <- c(a = -1, b = 2)
  expect_equal(v_model_logistic_normal_mixture(object), err_msg)
})

test_that("v_model_logistic_normal_mixture returns error for wrong weightpar names", {
  object <- h_get_logistic_normal_mixture()
  err_msg0 <- "weightpar must be a numerical vector of length two with values greater than 0"
  err_msg <- "weightpar should be a named vector of length two with names 'a' and 'b'"
  # Assigning wrong values for weightpar.
  object@weightpar <- c(1, 2)
  expect_equal(v_model_logistic_normal_mixture(object), c(err_msg0, err_msg))
  object@weightpar <- c(a = 1, 2)
  expect_equal(v_model_logistic_normal_mixture(object), c(err_msg0, err_msg))
  object@weightpar <- c(1, b = 2)
  expect_equal(v_model_logistic_normal_mixture(object), c(err_msg0, err_msg))
  object@weightpar <- c(a = 1, g = 2)
  expect_equal(v_model_logistic_normal_mixture(object), err_msg)
  object@weightpar <- c(h = 1, g = 2)
  expect_equal(v_model_logistic_normal_mixture(object), err_msg)
})
