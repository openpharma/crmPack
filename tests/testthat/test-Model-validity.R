# v_general_model ----

test_that("v_general_model passes for valid object", {
  object <- h_get_general_model()
  expect_true(v_general_model(object))
})

test_that("v_general_model returns message for non-valid object", {
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

test_that("v_model returns message for wrong dose function", {
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

test_that("v_model returns message for wrong prob function", {
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

# v_model_logistic_kadane ----

test_that("v_model_logistic_kadane passes for valid object", {
  object <- h_get_logistic_kadane()
  expect_true(v_model_logistic_kadane(object))
})

test_that("v_model_logistic_kadane returns message for wrong theta probability", {
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

test_that("v_model_logistic_kadane returns message for non-scalars", {
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

test_that("v_model_logistic_kadane returns message for xmin greater than xmax", {
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

test_that("v_model_logistic_kadane returns message for wrong theta probability", {
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

test_that("v_model_logistic_kadane returns message for xmin greater than xmax", {
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

test_that("v_model_logistic_kadane returns message for non-scalars", {
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

test_that("v_model_logistic_kadane_beta_gamma returns message for non-scalars", {
  object <- h_get_logistic_kadane_beta_gam()
  # Assigning vectors for scalar slots.
  object@alpha <- 3:6
  object@beta <- 3:6
  object@shape <- 4:7
  object@rate <- 4:7

  expect_snapshot(v_model_logistic_kadane_beta_gamma(object))
})

test_that("v_model_logistic_kadane_beta_gamma returns message for wrong Beta distribution shape parameter alpha", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Beta distribution shape parameter alpha must be a positive scalar"
  # Assigning wrong values for Beta distribution shape parameter alpha.
  object@alpha <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@alpha <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns message for wrong Beta distribution shape parameter beta", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Beta distribution shape parameter beta must be a positive scalar"
  # Assigning wrong values for Beta distribution shape parameter beta.
  object@beta <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@beta <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns message for wrong Gamma distribution shape parameter", {
  object <- h_get_logistic_kadane_beta_gam()
  err_msg <- "Gamma distribution shape parameter must be a positive scalar"
  # Assigning wrong values for Gamma distribution shape parameter.
  object@shape <- -1
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
  object@shape <- 0
  expect_equal(v_model_logistic_kadane_beta_gamma(object), err_msg)
})

test_that("v_model_logistic_kadane_beta_gamma returns message for wrong Gamma distribution rate parameter", {
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

test_that("v_model_logistic_normal_mix returns message for wrong weightpar", {
  object <- h_get_logistic_normal_mix()
  err_msg <- "weightpar must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  # Assigning wrong values for weightpar.
  object@weightpar <- c(a = -1, b = -2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
  object@weightpar <- c(a = -1, b = 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)

  object@weightpar <- c(1, 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
  object@weightpar <- c(a = 1, 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
  object@weightpar <- c(1, b = 2)
  expect_equal(v_model_logistic_normal_mix(object), err_msg)
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

test_that("v_model_logistic_normal_fixed_mix returns message for wrong components", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning wrong values for components.
  object@components <- list(mean = c(0, 1), cov = diag(2))

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "components must be a list with ModelParamsNormal S4 class objects"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns message for non-valid ModelParamsNormal comp", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning wrong values for ModelParamsNormal object.
  object@components[[1]]@mean <- c(0, NA)
  object@components[[1]]@cov <- matrix(letters[1:4], nrow = 2)

  expect_snapshot(v_model_logistic_normal_fixed_mix(object))
})

test_that("v_model_logistic_normal_fixed_mix returns message for weights and comp of diff len", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning weights of different length than the components.
  object@weights <- rep(0.1, 10)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "components must have same length as weights"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns message for weights not sum to 1", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning weights that do not sum to 1.
  object@weights <- c(2, 4)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "weights must sum to 1"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns message for negative weights", {
  object <- h_get_logistic_normal_fixed_mix()
  # Assigning negative weights.
  object@weights <- c(-0.5, 1.5)

  expect_equal(
    v_model_logistic_normal_fixed_mix(object),
    "weights must be positive"
  )
})

test_that("v_model_logistic_normal_fixed_mix returns message for non-scalar log_normal", {
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

test_that("v_model_logistic_log_normal_mix returns message for wrong share_weight", {
  object <- h_get_logistic_log_normal_mix()
  err_msg <- "share_weight does not specify a probability"
  # Assigning wrong values for weightpar.
  object@share_weight <- -1
  expect_equal(v_model_logistic_log_normal_mix(object), err_msg)
  object@share_weight <- c(-1, 0.5)
  expect_equal(v_model_logistic_log_normal_mix(object), err_msg)
})

# v_model_dual_endpoint ----

test_that("v_model_dual_endpoint passes for valid object", {
  object <- h_get_dual_endpoint()
  object_ff <- h_get_dual_endpoint(fixed = FALSE)

  expect_true(v_model_dual_endpoint(object))
  expect_true(v_model_dual_endpoint(object_ff))
})

test_that("v_model_dual_endpoint returns message for wrong use_log_dose", {
  object <- h_get_dual_endpoint()
  # We assign a use_log_dose which is not a single flag.
  object@use_log_dose <- c(TRUE, FALSE)

  expect_equal(
    v_model_dual_endpoint(object),
    "use_log_dose must be TRUE or FALSE"
  )
})

test_that("v_model_dual_endpoint returns message for wrong use_fixed", {
  object <- h_get_dual_endpoint()
  # Assigning non-valid use_fixed.
  object@use_fixed <- TRUE

  expect_snapshot(v_model_dual_endpoint(object))
})

test_that("v_model_dual_endpoint returns message for wrong fixed sigma2W", {
  object <- h_get_dual_endpoint()
  # Assigning wrong values for sigma2W.
  object@sigma2W <- c(-5:0, Inf)

  expect_equal(
    v_model_dual_endpoint(object),
    "sigma2W must be a positive and finite numerical scalar"
  )
})

test_that("v_model_dual_endpoint returns message for wrong sigma2W", {
  object <- h_get_dual_endpoint()
  # Assigning wrong values for sigma2W.
  object@sigma2W <- c(4, -5, b = -Inf)
  object@use_fixed["sigma2W"] <- FALSE

  expect_equal(
    v_model_dual_endpoint(object),
    "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

test_that("v_model_dual_endpoint returns message for wrong fixed rho", {
  object <- h_get_dual_endpoint()
  # Assigning wrong values for rho.
  object@rho <- c(-5:0, Inf)

  expect_equal(
    v_model_dual_endpoint(object),
    "rho must be a number in (-1, 1)"
  )
})

test_that("v_model_dual_endpoint returns message for wrong rho", {
  object <- h_get_dual_endpoint()
  # Assigning wrong values for rho.
  object@rho <- c(4, -5, b = -Inf)
  object@use_fixed["rho"] <- FALSE

  expect_equal(
    v_model_dual_endpoint(object),
    "rho must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

# v_model_dual_endpoint_rw ----

test_that("v_model_dual_endpoint_rw passes for valid object", {
  object <- h_get_dual_endpoint_rw()
  object_ff <- h_get_dual_endpoint_rw(fixed = FALSE)

  expect_true(v_model_dual_endpoint_rw(object))
  expect_true(v_model_dual_endpoint_rw(object_ff))
})

test_that("v_model_dual_endpoint_rw returns message for wrong use_fixed", {
  object <- h_get_dual_endpoint_rw()
  # Assigning non-valid use_fixed.
  object@use_fixed <- TRUE

  expect_equal(
    v_model_dual_endpoint_rw(object),
    c(
      "use_fixed must be a named logical vector that contains name 'sigma2betaW'",
      "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  )
})

test_that("v_model_dual_endpoint_rw returns message for wrong fixed sigma2betaW", {
  object <- h_get_dual_endpoint_rw()
  # Assigning wrong values for sigma2betaW.
  object@sigma2betaW <- c(-5:0, Inf)

  expect_equal(
    v_model_dual_endpoint_rw(object),
    "sigma2betaW must be a positive and finite numerical scalar"
  )
})

test_that("v_model_dual_endpoint_rw returns message for wrong sigma2betaW", {
  object <- h_get_dual_endpoint_rw()
  # Assigning wrong values for sigma2betaW.
  object@sigma2betaW <- c(4, -5, b = -Inf)
  object@use_fixed["sigma2betaW"] <- FALSE

  expect_equal(
    v_model_dual_endpoint_rw(object),
    "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

# v_model_dual_endpoint_beta ----

test_that("v_model_dual_endpoint_beta passes for valid object", {
  object <- h_get_dual_endpoint_beta()
  object_ff <- h_get_dual_endpoint_beta(fixed = FALSE)

  expect_true(v_model_dual_endpoint_beta(object))
  expect_true(v_model_dual_endpoint_beta(object_ff))
})

test_that("v_model_dual_endpoint_beta returns message for wrong use_fixed", {
  object <- h_get_dual_endpoint_beta()
  # Assigning non-valid use_fixed.
  object@use_fixed <- TRUE

  expect_snapshot(v_model_dual_endpoint_beta(object))
})

test_that("v_model_dual_endpoint_beta returns message for wrong fixed parameters", {
  object <- h_get_dual_endpoint_beta()
  # Assigning wrong values for fixed delta1, mode parameters.
  object@delta1 <- c(-2, 0)
  object@mode <- c(-2, 2)

  expect_snapshot(v_model_dual_endpoint_beta(object))
})

test_that("v_model_dual_endpoint_beta returns message for wrong parameters", {
  object <- h_get_dual_endpoint_beta(fixed = FALSE)
  # Assigning wrong values for E0, Emax, delta1, mode parameters.
  object@E0 <- c(4, -5, b = -Inf)
  object@Emax <- c(4, -5, b = -Inf)
  object@delta1 <- c(4, -5, b = -Inf)
  object@mode <- c(4, -5, b = -Inf)

  expect_snapshot(v_model_dual_endpoint_beta(object))
})

# v_model_dual_endpoint_emax ----

test_that("v_model_dual_endpoint_emax passes for valid object", {
  object <- h_get_dual_endpoint_emax()
  object_ff <- h_get_dual_endpoint_emax(fixed = FALSE)

  expect_true(v_model_dual_endpoint_emax(object))
  expect_true(v_model_dual_endpoint_emax(object_ff))
})

test_that("v_model_dual_endpoint_emax returns message for wrong fixed parameters", {
  object <- h_get_dual_endpoint_emax()
  # Assigning wrong values for fixed E0, Emax, ED50 parameters.
  object@E0 <- c(-2, 0)
  object@Emax <- c(-2, 2)
  object@ED50 <- c(-2, 6)

  expect_snapshot(v_model_dual_endpoint_emax(object))
})

test_that("v_model_dual_endpoint_emax returns message for wrong parameters", {
  object <- h_get_dual_endpoint_emax(fixed = FALSE)
  # Assigning wrong values for E0, Emax, ED50 parameters.
  object@E0 <- c(4, -5, b = -Inf)
  object@Emax <- c(4, -5, b = -Inf)
  object@ED50 <- c(4, -5, b = -Inf)

  expect_snapshot(v_model_dual_endpoint_emax(object))
})
