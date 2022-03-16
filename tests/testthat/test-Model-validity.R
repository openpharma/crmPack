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

test_that("v_model_log_normal passes for valid object with ref_dose 0", {
  object <- h_get_model_log_normal()
  object@ref_dose <- 0

  expect_true(v_model_log_normal(object))
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

test_that("v_model_logistic_kadane returns error for wrong theta probability", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "theta must be a probability scalar > 0 and < 1"
  # Assigning wrong values for probability theta.
  object@theta <- -1
  expect_equal(v_model_logistic_kadane(object), err_msg)
  object@theta <- 5
  expect_equal(v_model_logistic_kadane(object), err_msg)
  object@theta <- 0
  expect_equal(v_model_logistic_kadane(object), err_msg)
})

test_that("v_model_logistic_kadane returns error for xmin greater than xmax", {
  object <- h_get_logistic_kadane_beta_gam()
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

test_that("v_model_logistic_kadane returns error for non-scalars", {
  object <- h_get_logistic_kadane_beta_gam()
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

# v_model_logistic_kadane_beta_gamma ----

test_that("v_model_logistic_kadane_beta_gamma passes for valid object", {
  object <- h_get_logistic_kadane_beta_gam()
  expect_true(v_model_logistic_kadane_beta_gamma(object))
})

test_that("v_model_logistic_kadane_beta_gamma returns error for non-scalars", {
  object <- h_get_logistic_kadane_beta_gam()
  # Assigning vectors for scalar slots.
  object@alpha <- 3:6
  object@beta <- 3:6
  object@shape <- 4:7
  object@rate <- 4:7

  expect_equal(
    v_model_logistic_kadane_beta_gamma(object),
    c(
      "Beta distribution shape parameter alpha must be a positive scalar",
      "Beta distribution shape parameter beta must be a positive scalar",
      "Gamma distribution shape parameter must be a positive scalar",
      "Gamma distribution rate parameter must be a positive scalar"
    )
  )
})

test_that("v_model_logistic_kadane_beta_gamma returns error for wrong Beta distribution shape parameter alpha", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Beta distribution shape parameter alpha must be a positive scalar"
  # Assigning wrong values for Beta distribution shape parameter alpha.
  object@alpha <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@alpha <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns error for wrong Beta distribution shape parameter beta", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Beta distribution shape parameter beta must be a positive scalar"
  # Assigning wrong values for Beta distribution shape parameter beta.
  object@beta <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@beta <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns error for wrong Gamma distribution shape parameter", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Gamma distribution shape parameter must be a positive scalar"
  # Assigning wrong values for Gamma distribution shape parameter.
  object@shape <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@shape <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns error for wrong Gamma distribution rate parameter", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Gamma distribution rate parameter must be a positive scalar"
  # Assigning wrong values for Gamma distribution rate parameter.
  object@rate <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@rate <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

# v_model_logistic_normal_mix ----

test_that("v_model_logistic_normal_mix passes for valid object", {
  object <- h_get_logistic_normal_mix()
  expect_true(v_model_logistic_normal_mix(object))
})

test_that("v_model_logistic_normal_mix passes for valid object with ref_dose 0", {
  object <- h_get_logistic_normal_mix()
  object@ref_dose <- 0

  expect_true(v_model_logistic_normal_mix(object))
})

test_that("v_model_logistic_normal_mix returns error for wrong ref_dose", {
  object <- h_get_logistic_normal_mix()
  # We assign a ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    v_model_logistic_normal_mix(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("v_model_logistic_normal_mix returns error for wrong weightpar", {
  object <- h_get_logistic_normal_mix()
  err_msg <- "weightpar must be a numerical vector of length two with values greater than 0"
  # Assigning wrong values for weightpar.
  object@weightpar <- c(a = -1, b = -2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
  object@weightpar <- c(a = -1, b = 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
})

test_that("v_model_logistic_normal_mix returns error for wrong weightpar names", {
  object <- h_get_logistic_normal_mix()
  err_msg0 <- "weightpar must be a numerical vector of length two with values greater than 0"
  err_msg <- "weightpar should be a named vector of length two with names 'a' and 'b'"
  # Assigning wrong values for weightpar.
  object@weightpar <- c(1, 2)
  expect_equal(v_model_logistic_normal_mix(object), c(err_msg0, err_msg))
  object@weightpar <- c(a = 1, 2)
  expect_equal(v_model_logistic_normal_mix(object), c(err_msg0, err_msg))
  object@weightpar <- c(1, b = 2)
  expect_equal(v_model_logistic_normal_mix(object), c(err_msg0, err_msg))
  object@weightpar <- c(a = 1, g = 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
  object@weightpar <- c(h = 1, g = 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
})

# v_model_logistic_normal_fixed_mix ----

test_that("v_model_logistic_normal_fixed_mix passes for valid object", {
  object <- h_get_logistic_normal_fixed_mix()
  expect_true(v_model_logistic_normal_fixed_mix(object))
})

test_that("v_model_logistic_normal_fixed_mix passes for valid object with ref_dose 0", {
  object <- h_get_logistic_normal_fixed_mix()
  object@ref_dose <- 0

  expect_true(v_model_logistic_normal_fixed_mix(object))
})

test_that("v_model_logistic_normal_fixed_mix returns error for wrong ref_dose", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for wrong components", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning wrong values for components.
  object@components <- list(mean = c(0, 1), cov = diag(2))

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "components must be a list with ModelParamsNormal S4 class objects"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for non-valid ModelParamsNormal comp", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning wrong values for ModelParamsNormal object.
  object@components[[1]]@mean <- c(0, NA)
  object@components[[1]]@cov <- matrix(letters[1:4], nrow = 2)

  err_msg <- paste(
    c(
      "components must be a list with valid ModelParamsNormal S4 class objects",
      "mean must have length 2 and no missing values are allowed",
      "cov must be 2x2 matrix without any missing values"
    ),
    collapse = ", "
  )
  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    err_msg
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for weights and comp of diff len", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning weights of different length than the components.
  object@weights <- rep(0.1, 10)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "components must have same length as weights"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for weights not sum to 1", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning weights that do not sum to 1.
  object@weights <- c(2, 4)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "weights must sum to 1"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for negative weights", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning negative weights.
  object@weights <- c(-0.5, 1.5)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "weights must be positive"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns error for non-scalar log_normal", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning non-scalar log_normal.
  object@log_normal <- c(TRUE, FALSE, TRUE)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "log_normal must be TRUE or FALSE"
  )
})

# v_model_logistic_log_normal_mix ----

test_that("v_model_logistic_log_normal_mix passes for valid object", {
  object <- h_get_logistic_log_normal_mix()
  expect_true(v_model_logistic_log_normal_mix(object))
})

test_that("v_model_logistic_log_normal_mix passes for valid object with ref_dose 0", {
  object <- h_get_logistic_log_normal_mix()
  object@ref_dose <- 0

  expect_true(v_model_logistic_log_normal_mix(object))
})

test_that("v_model_logistic_log_normal_mix returns error for wrong ref_dose", {
  object <- h_get_logistic_log_normal_mix()
  # We assign a ref_dose which is not a non-negative scalar.
  object@ref_dose <- c(-3, -5, 4)

  expect_equal(
    v_model_logistic_log_normal_mix(object),
    "ref_dose must be a non-negative scalar"
  )
})

test_that("v_model_logistic_log_normal_mix returns error for wrong share_weight", {
  object <- h_get_logistic_log_normal_mix()
  err_msg <- "share_weight does not specify a probability"
  # Assigning wrong values for weightpar.
  object@share_weight <- -1
  expect_equal(v_model_logistic_log_normal_mix(object), err_msg)
  object@share_weight <- c(-1, 0.5)
  expect_equal(v_model_logistic_log_normal_mix(object), err_msg)
})
