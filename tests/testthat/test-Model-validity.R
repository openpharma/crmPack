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
    v_model_logistic_normal_fixed_mix(object), "components must have same length as weights"
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

# v_model_logistic_indep_beta ----

test_that("v_model_logistic_indep_beta passes for valid object", {
  object_edat <- h_get_logistic_indep_beta(emptydata = TRUE)
  object <- h_get_logistic_indep_beta(emptydata = FALSE)

  expect_true(v_model_logistic_indep_beta(object_edat))
  expect_true(v_model_logistic_indep_beta(object))
})

test_that("v_model_logistic_indep_beta returns message for wrong DLE parameters", {
  object <- h_get_logistic_indep_beta()
  # Assigning wrong values for binDLE, DLEdose, DLEweights.
  object@binDLE <- c(-2, NA)
  object@DLEdose <- c(3, NA)
  object@DLEweights <- c(4L, NA)
  expect_snapshot(v_model_logistic_indep_beta(object))

  object@binDLE <- -2
  object@DLEdose <- 3
  object@DLEweights <- 4L
  expect_snapshot(v_model_logistic_indep_beta(object))
})

test_that("v_model_logistic_indep_beta returns message for wrong DLE parameters (diff len)", {
  object <- h_get_logistic_indep_beta()
  # Assigning wrong-length values for binDLE, DLEdose, DLEweights.
  object@binDLE <- c(2, 6)
  object@DLEdose <- c(3, 8, 9)
  object@DLEweights <- c(4L, 12L)
  expect_snapshot(v_model_logistic_indep_beta(object))

  object@binDLE <- c(2, 6)
  object@DLEdose <- c(3, 8)
  object@DLEweights <- c(4L, 12L, 20L)
  expect_snapshot(v_model_logistic_indep_beta(object))

  object@binDLE <- c(2, 6)
  object@DLEdose <- c(3, 8, 11)
  object@DLEweights <- c(4L, 12L, 20L)
  expect_snapshot(v_model_logistic_indep_beta(object))
})

test_that("v_model_logistic_indep_beta returns message for wrong phi parameters", {
  object <- h_get_logistic_indep_beta()
  # Assigning non-scalar values for phi1 and phi2.
  object@phi1 <- c(2, 6)
  object@phi2 <- c(3, 8, 9)

  expect_equal(
    v_model_logistic_indep_beta(object),
    c(
      "phi1 must be a numerical scalar",
      "phi2 must be a numerical scalar"
    )
  )
})

test_that("v_model_logistic_indep_beta returns message for wrong Pcov", {
  err_msg <- "Pcov must be 2x2 positive-definite matrix without any missing values"
  object <- h_get_logistic_indep_beta()

  # Assigning wrong Pcov matrix.
  object@Pcov <- matrix(c(1:3, 4, 5, NA), ncol = 2)
  expect_equal(v_model_logistic_indep_beta(object), err_msg)

  object@Pcov <- matrix(c(5, 2, 1, 5), ncol = 2)
  expect_equal(v_model_logistic_indep_beta(object), err_msg)

  object@Pcov <- matrix(c(5, 2, 3, 2, 3, 2, 3, 2, 5), ncol = 3)
  expect_equal(v_model_logistic_indep_beta(object), err_msg)
})

# v_model_eff_log_log ----

test_that("v_model_eff_log_log passes for valid object", {
  object_edat <- h_get_eff_log_log(emptydata = TRUE)
  object <- h_get_eff_log_log(emptydata = FALSE)

  expect_true(v_model_eff_log_log(object_edat))
  expect_true(v_model_eff_log_log(object))
})

test_that("v_model_eff_log_log returns message for wrong eff and eff_dose parameters (NAs)", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for eff, eff_dose (no NA allowed, min len 2).
  object@eff <- c(2, NA)
  object@eff_dose <- c(3, NA)
  expect_equal(
    v_model_eff_log_log(object),
    c(
      "eff must be a finite numerical vector of minimum length 2, without missing values",
      "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
    )
  )
})

test_that("v_model_eff_log_log returns message for wrong eff and eff_dose parameters (scalars)", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for eff, eff_dose.
  object@eff <- 2
  object@eff_dose <- 3
  expect_equal(
    v_model_eff_log_log(object),
    "eff must be a finite numerical vector of minimum length 2, without missing values"
  )
})

test_that("v_model_eff_log_log returns message for wrong eff and eff_dose parameters (diff lengths)", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for eff, eff_dose.
  object@eff <- c(20, 50)
  object@eff_dose <- c(4, 6, 7)
  expect_equal(
    v_model_eff_log_log(object),
    "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
  )
})

test_that("v_model_eff_log_log returns message for wrong fixed nu", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for nu.
  object@nu <- c(-5:0, Inf)
  object@use_fixed <- TRUE
  expect_equal(
    v_model_eff_log_log(object),
    "nu must be a positive and finite numerical scalar"
  )
})

test_that("v_model_eff_log_log returns message for wrong nu", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for nu.
  object@nu <- c(4, -5, b = -Inf)
  expect_equal(
    v_model_eff_log_log(object),
    "nu must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

test_that("v_model_eff_log_log returns message for wrong use_fixed", {
  object <- h_get_eff_log_log()
  # Assigning non-valid use_fixed.
  object@use_fixed <- c(TRUE, FALSE)
  expect_equal(v_model_eff_log_log(object), "use_fixed must be a flag")
})

test_that("v_model_eff_log_log returns message for wrong const", {
  object <- h_get_eff_log_log()
  err_msg <- "const must be a non-negative number"

  # Assigning non-valid const.
  object@const <- c(5, 6)
  expect_equal(v_model_eff_log_log(object), err_msg)

  object@const <- -4
  expect_equal(v_model_eff_log_log(object), err_msg)
})

test_that("v_model_eff_log_log returns message for dose + const <= 1 (emptydata)", {
  object <- h_get_eff_log_log(emptydata = TRUE)
  # Assigning wrong combination of eff_dose and const.
  object@eff_dose <- c(1, 2)
  object@const <- 0
  expect_equal(
    v_model_eff_log_log(object),
    "For log-log model, doses and const must be such that dose + const > 1"
  )
})

test_that("v_model_eff_log_log returns message for dose + const <= 1", {
  object <- h_get_eff_log_log(emptydata = FALSE)
  # Assigning wrong combination of dose (at observed efficacy) and const.
  object@data@doseGrid[1] <- 1
  object@const <- 0
  expect_equal(
    v_model_eff_log_log(object),
    "For log-log model, doses and const must be such that dose + const > 1"
  )
})

test_that("v_model_eff_log_log returns message for wrong theta1, theta2", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for theta1, theta2.
  object@theta1 <- c(-5, Inf)
  object@theta2 <- c(4, 7)

  expect_equal(
    v_model_eff_log_log(object),
    c("theta1 must be a numerical scalar", "theta2 must be a numerical scalar")
  )
})

test_that("v_model_eff_log_log returns message for wrong Pcov", {
  object <- h_get_eff_log_log()
  err_msg <- "Pcov must be 2x2 positive-definite matrix without any missing values"

  # Assigning wrong Pcov matrix.
  object@Pcov <- matrix(c(1:3, 4, 5, NA), ncol = 2)
  expect_equal(v_model_eff_log_log(object), err_msg)

  object@Pcov <- matrix(c(5, 2, 1, 5), ncol = 2)
  expect_equal(v_model_eff_log_log(object), err_msg)

  object@Pcov <- matrix(c(5, 2, 3, 2, 3, 2, 3, 2, 5), ncol = 3)
  expect_equal(v_model_eff_log_log(object), err_msg)
})

test_that("v_model_eff_log_log returns message for wrong Pcov (data len <= 2)", {
  object <- h_get_eff_log_log(dlt_observed_only = TRUE)
  # Assigning wrong Pcov matrix.
  object@Pcov <- matrix(c(1:3, 4, 5, NA), ncol = 2)
  expect_equal(
    v_model_eff_log_log(object),
    "Pcov must be 2x2 numeric matrix with all values missing if the length of combined data is 2"
  )
})

test_that("v_model_eff_log_log returns message for wrong X (empty data)", {
  object <- h_get_eff_log_log(emptydata = TRUE)
  # Assigning wrong values for X (wrong dimension).
  object@X <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_log_log(object),
    "X must be a finite numerical matrix of size 2 x 2, without any missing values"
  )

  # Assigning wrong values for X (wrong 1st column).
  object@X <- matrix(c(1, 0, 27, 302), ncol = 2)
  expect_equal(
    v_model_eff_log_log(object),
    "X must be a design matrix, i.e. first column must be of 1s"
  )
})

test_that("v_model_eff_log_log returns message for wrong X", {
  object <- h_get_eff_log_log(emptydata = FALSE)
  # Assigning wrong values for X (wrong dimension).
  object@X <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_log_log(object),
    "X must be a finite numerical matrix of size 4 x 2, without any missing values"
  )

  # Assigning wrong values for X (wrong 1st column).
  object@X <- matrix(c(1, 1, 1, 0, 25, 50, 50, 75), ncol = 2)
  expect_equal(
    v_model_eff_log_log(object),
    "X must be a design matrix, i.e. first column must be of 1s"
  )
})

test_that("v_model_eff_log_log returns message for wrong Y (empty data)", {
  object <- h_get_eff_log_log(emptydata = TRUE)
  # Assigning wrong values for Y.
  object@Y <- c(27, 302, 27)
  expect_equal(
    v_model_eff_log_log(object),
    "Y must be a finite numerical vector of length 2 and without any missing values"
  )
})

test_that("v_model_eff_log_log returns message for wrong Y", {
  object <- h_get_eff_log_log(emptydata = FALSE)
  # Assigning wrong values for Y.
  object@Y <- c(0.31, 0.42, 0.59, 0.45, 5)
  expect_equal(
    v_model_eff_log_log(object),
    "Y must be a finite numerical vector of length 4 and without any missing values"
  )
})

test_that("v_model_eff_log_log returns message for wrong mu", {
  object <- h_get_eff_log_log()
  # Assigning wrong values for mu.
  object@mu <- c(4, -5, b = -Inf, NA)
  expect_equal(
    v_model_eff_log_log(object),
    "mu must be a finite numerical vector of length 2"
  )
})

test_that("v_model_eff_log_log returns message for wrong Q", {
  object <- h_get_eff_log_log()
  err_msg <- "Q must be 2x2 positive-definite matrix without any missing values"

  # Assigning wrong Pcov matrix.
  object@Q <- matrix(c(1:3, 4, 5, NA), ncol = 2)
  expect_equal(v_model_eff_log_log(object), err_msg)

  object@Q <- matrix(c(5, 2, 1, 5), ncol = 2)
  expect_equal(v_model_eff_log_log(object), err_msg)

  object@Q <- matrix(c(5, 2, 3, 2, 3, 2, 3, 2, 5), ncol = 3)
  expect_equal(v_model_eff_log_log(object), err_msg)
})

# v_model_eff_flexi ----

test_that("v_model_eff_flexi passes for valid object", {
  object_edat <- h_get_eff_flexi(emptydata = TRUE)
  object <- h_get_eff_flexi(emptydata = FALSE)

  expect_true(v_model_eff_flexi(object_edat))
  expect_true(v_model_eff_flexi(object))
})

test_that("v_model_eff_flexi returns message for wrong eff and eff_dose parameters (NAs)", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for eff, eff_dose (no NA allowed, min len 2).
  object@eff <- c(2, NA)
  object@eff_dose <- c(3, NA)
  expect_equal(
    v_model_eff_flexi(object),
    c(
      "eff must be a finite numerical vector of minimum length 2, without missing values",
      "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong eff and eff_dose parameters (scalars)", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for eff, eff_dose.
  object@eff <- 2
  object@eff_dose <- 3
  expect_equal(
    v_model_eff_flexi(object),
    "eff must be a finite numerical vector of minimum length 2, without missing values"
  )
})

test_that("v_model_eff_flexi returns message for wrong eff and eff_dose parameters (diff lengths)", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for eff, eff_dose.
  object@eff <- c(20, 50)
  object@eff_dose <- c(4, 6, 7)
  expect_equal(
    v_model_eff_flexi(object),
    "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
  )
})

test_that("v_model_eff_flexi returns message for wrong use_fixed", {
  object <- h_get_eff_flexi()
  # Assigning non-valid use_fixed.
  object@use_fixed <- TRUE

  expect_equal(
    v_model_eff_flexi(object),
    c(
      "use_fixed must be a named logical vector that contains name 'sigma2W'",
      "use_fixed must be a named logical vector that contains name 'sigma2betaW'"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong fixed sigma2W", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for sigma2W.
  object@sigma2W <- c(-5:0, Inf)
  object@use_fixed <- TRUE
  expect_snapshot(v_model_eff_flexi(object))
})

test_that("v_model_eff_flexi returns message for wrong sigma2W", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for sigma2W.
  object@sigma2W <- c(4, -5, b = -Inf)
  expect_equal(
    v_model_eff_flexi(object),
    "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

test_that("v_model_eff_flexi returns message for wrong fixed sigma2betaW", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for sigma2betaW.
  object@sigma2betaW <- c(-5:0, Inf)
  object@use_fixed <- TRUE
  expect_snapshot(v_model_eff_flexi(object))
})

test_that("v_model_eff_flexi returns message for wrong sigma2betaW", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for sigma2betaW.
  object@sigma2betaW <- c(4, -5, b = -Inf)
  expect_equal(
    v_model_eff_flexi(object),
    "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
})

test_that("v_model_eff_flexi returns message for wrong rw1", {
  object <- h_get_eff_flexi()
  # Assigning non-valid rw1.
  object@rw1 <- c(TRUE, FALSE)
  expect_equal(
    v_model_eff_flexi(object),
    c("rw1 must be a flag", "RW_rank must be an integer equal to data@nGrid - 2L")
  )
})

test_that("v_model_eff_flexi returns message for wrong ncol of X (empty data)", {
  object <- h_get_eff_flexi(emptydata = TRUE)
  # Assigning wrong values for X (wrong ncol).
  object@X <- matrix(c(1, 1, 1, 0, 0, 1), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    "X must be an integer matrix with 12 columns and without any missing values"
  )
})

test_that("v_model_eff_flexi returns message for wrong X (empty data)", {
  object <- h_get_eff_flexi(emptydata = TRUE)
  # Assigning wrong values for X (wrong values and ncol).
  object@X <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    c(
      "X must be an integer matrix with 12 columns and without any missing values",
      "X must be a matrix with 0-1 values only"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong ncol of X", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for X (wrong ncol).
  object@X <- matrix(c(1, 1, 1, 0, 0, 1), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    "X must be an integer matrix with 12 columns and without any missing values"
  )
})

test_that("v_model_eff_flexi returns message for wrong X", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for X (wrong values and ncol).
  object@X <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    c(
      "X must be an integer matrix with 12 columns and without any missing values",
      "X must be a matrix with 0-1 values only"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong RW", {
  object <- h_get_eff_flexi()
  # Assigning wrong values for X (wrong dimension and va).
  object@RW <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    c(
      "RW must be 12x12 matrix without any missing values"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong RW (RW2)", {
  object <- h_get_eff_flexi(rw1 = FALSE)
  # Assigning wrong values for RW (wrong dimension).
  object@RW <- matrix(c(1, 1, 1, 27, 302, 27), ncol = 2)
  expect_equal(
    v_model_eff_flexi(object),
    c(
      "RW must be 12x12 matrix without any missing values"
    )
  )
})

test_that("v_model_eff_flexi returns message for wrong RW_rank", {
  object <- h_get_eff_flexi()
  err_msg <- "RW_rank must be an integer equal to data@nGrid - 2L"

  # Assigning wrong RW_rank.
  object@RW_rank <- c(5L, 6L)
  expect_equal(v_model_eff_flexi(object), err_msg)

  object@RW_rank <- 5L
  expect_equal(v_model_eff_flexi(object), err_msg)
})

# v_model_da_logistic_log_normal ----

test_that("v_model_da_logistic_log_normal passes for valid object", {
  object <- h_get_da_logistic_log_normal()
  expect_true(v_model_da_logistic_log_normal(object))
})

test_that("v_model_da_logistic_log_normal returns message for wrong npiece", {
  object <- h_get_da_logistic_log_normal()

  # Assigning wrong npiece.
  object@npiece <- c(5L, 6L)
  expect_equal(
    v_model_da_logistic_log_normal(object),
    "npiece must be a is a single integerish value"
  )
})

test_that("v_model_da_logistic_log_normal returns message for wrong l vector", {
  object <- h_get_da_logistic_log_normal()
  err_msg <- "prior parameter vector l of lambda must be a non-negative vector of length equal to npiece"

  # Assigning wrong l.
  object@l <- c(5L, 6L, Inf)
  expect_equal(v_model_da_logistic_log_normal(object), err_msg)

  object@l <- 5L
  expect_equal(v_model_da_logistic_log_normal(object), err_msg)
})

test_that("v_model_da_logistic_log_normal returns message for wrong c_par", {
  object <- h_get_da_logistic_log_normal()

  # Assigning wrong c_par.
  object@c_par <- c(5, 6, Inf)
  expect_equal(
    v_model_da_logistic_log_normal(object),
    "c_par must be a finite numerical scalar"
  )
})

test_that("v_model_da_logistic_log_normal returns message for wrong cond_pem", {
  object <- h_get_da_logistic_log_normal()

  # Assigning non-valid cond_pem.
  object@cond_pem <- c(TRUE, FALSE)
  expect_equal(
    v_model_da_logistic_log_normal(object),
    "cond_pem must be a flag"
  )
})

# v_model_tite_logistic_log_normal ----

test_that("v_model_tite_logistic_log_normal passes for valid object", {
  object <- h_get_tite_logistic_log_normal()
  expect_true(v_model_tite_logistic_log_normal(object))
})

test_that("v_model_tite_logistic_log_normal returns message for wrong weight_method", {
  object <- h_get_tite_logistic_log_normal()
  err_msg <- "weight_method must be a string equal either to linear or adaptive"

  # Assigning wrong weight_method.
  object@weight_method <- "linearadaptive"
  expect_equal(v_model_tite_logistic_log_normal(object), err_msg)

  object@weight_method <- c("linear", "adaptive")
  expect_equal(v_model_tite_logistic_log_normal(object), err_msg)
})

# v_model_one_par_exp_normal_prior ----

test_that("v_model_one_par_exp_normal_prior passes for valid object", {
  object <- h_get_one_par_log_normal_prior()
  expect_true(v_model_one_par_exp_normal_prior(object))
})

test_that("v_model_one_par_exp_normal_prior passes for valid object (finit art. prec. interpolation)", {
  object <- OneParLogNormalPrior(
    skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
    dose_grid = 1:5,
    sigma2 = 2
  )
  expect_true(v_model_one_par_exp_normal_prior(object))
})

test_that("v_model_one_par_exp_normal_prior returns message for wrong skel_fun - skel_fun_inv", {
  object <- h_get_one_par_log_normal_prior()

  # Assigning wrong skel_fun/skel_fun_inv
  object@skel_fun <- function(x) 2 * x
  object@skel_fun_inv <- function(x) x^2
  expect_equal(
    v_model_one_par_exp_normal_prior(object),
    c(
      "skel_fun_inv must be an inverse funtion of skel_fun function on within the range of sekeleton probs",
      "skel_fun_inv must be an inverse funtion of skel_fun function on outside the range of sekeleton probs"
    )
  )
})

test_that("v_model_one_par_exp_normal_prior returns message for wrong skel_probs", {
  object <- h_get_one_par_log_normal_prior()
  err_msg <- "skel_probs must be a unique sorted probability values between 0 and 1"

  # Assigning wrong skel_probs.
  object@skel_probs <- c(-1, 0.5, Inf)
  expect_equal(v_model_one_par_exp_normal_prior(object), err_msg)

  # Assigning non-unique skel_probs.
  object@skel_probs <- c(0.2, 0.2)
  expect_equal(v_model_one_par_exp_normal_prior(object), err_msg)

  # Assigning not sorted skel_probs.
  object@skel_probs <- c(0.2, 0.1)
  expect_equal(v_model_one_par_exp_normal_prior(object), err_msg)
})

test_that("v_model_one_par_exp_normal_prior returns message for wrong sigma2", {
  object <- h_get_one_par_log_normal_prior()
  err_msg <- "sigma2 must be a positive finite number"

  # Assigning wrong sigma2.
  object@sigma2 <- -1
  expect_equal(v_model_one_par_exp_normal_prior(object), err_msg)

  # Assigning sigma2 which is not a scalar.
  object@sigma2 <- 1:2
  expect_equal(v_model_one_par_exp_normal_prior(object), err_msg)
})

# v_model_one_par_exp_prior ----

test_that("v_model_one_par_exp_prior passes for valid object", {
  object <- h_get_one_par_exp_prior()
  expect_true(v_model_one_par_exp_prior(object))
})

test_that("v_model_one_par_exp_prior passes for valid object (finit art. prec. interpolation)", {
  object <- OneParExpPrior(
    skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
    dose_grid = 1:5,
    lambda = 2
  )
  expect_true(v_model_one_par_exp_prior(object))
})

test_that("v_model_one_par_exp_prior returns message for wrong skel_fun - skel_fun_inv", {
  object <- h_get_one_par_exp_prior()

  # Assigning wrong skel_fun/skel_fun_inv
  object@skel_fun <- function(x) 2 * x
  object@skel_fun_inv <- function(x) x^2
  expect_equal(
    v_model_one_par_exp_prior(object),
    c(
      "skel_fun_inv must be an inverse funtion of skel_fun function on within the range of sekeleton probs",
      "skel_fun_inv must be an inverse funtion of skel_fun function on outside the range of sekeleton probs"
    )
  )
})

test_that("v_model_one_par_exp_prior returns message for wrong skel_probs", {
  object <- h_get_one_par_exp_prior()
  err_msg <- "skel_probs must be a unique sorted probability values between 0 and 1"

  # Assigning wrong skel_probs.
  object@skel_probs <- c(-1, 0.5, Inf)
  expect_equal(v_model_one_par_exp_prior(object), err_msg)

  # Assigning non-unique skel_probs.
  object@skel_probs <- c(0.2, 0.2)
  expect_equal(v_model_one_par_exp_prior(object), err_msg)

  # Assigning not sorted skel_probs.
  object@skel_probs <- c(0.2, 0.1)
  expect_equal(v_model_one_par_exp_prior(object), err_msg)
})

test_that("v_model_one_par_exp_prior returns message for wrong lambda", {
  object <- h_get_one_par_exp_prior()
  err_msg <- "lambda must be a positive finite number"

  # Assigning wrong lambda.
  object@lambda <- -1
  expect_equal(v_model_one_par_exp_prior(object), err_msg)

  # Assigning lambda which is not a scalar.
  object@lambda <- 1:2
  expect_equal(v_model_one_par_exp_prior(object), err_msg)
})

# v_logisticlognormalordinal ----
test_that("LogisticLogNormalOrdinal accepts only diagonal covariance matrices", {
  expect_no_error(
    LogisticLogNormalOrdinal(
      mean = c(3, 4, 0),
      cov = diag(c(4, 3, 1)),
      ref_dose = 1
    )
  )

  expect_error(
    LogisticLogNormalOrdinal(
      mean = c(3, 4, 0),
      cov = matrix(c(4, -0.1, -0.1, -0.1, 3, -0.1, -0.1, -0.1, 1), ncol = 3),
      ref_dose = 1
    ),
    "invalid class \"LogisticLogNormalOrdinal\" object\\: covariance matrix must be diagonal"
  )
})
