#' Internal Helper Functions for Validation of [`GeneralModel`] and [`ModelPseudo`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`GeneralModel`] and [`ModelPseudo`] or inherited classes and therefore are
#' not exported.
#'
#' @name v_model_objects
#' @param object (`GeneralModel`) or (`ModelPseudo`) \cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_model_objects validates that the names of the
#'   arguments in `init` function are included in `datanames` or `datanames_prior`
#'   slots.
v_general_model <- function(object) {
  v <- Validate()
  v$check(
    h_check_fun_formals(object@init, allowed = union(object@datanames, object@datanames_prior)),
    "Arguments of the init function must be data names"
  )
  v$result()
}

#' @describeIn v_model_objects validates that the logistic Kadane model
#'   parameters are valid.
v_model_logistic_kadane <- function(object) {
  v <- Validate()
  v$check(
    test_probability(object@theta, bounds_closed = FALSE),
    "theta must be a probability scalar > 0 and < 1"
  )
  is_xmin_number <- test_number(object@xmin)
  v$check(is_xmin_number, "xmin must be scalar")

  is_xmax_number <- test_number(object@xmax)
  v$check(is_xmax_number, "xmax must be scalar")

  if (is_xmin_number && is_xmax_number) {
    v$check(object@xmin < object@xmax, "xmin must be strictly smaller than xmax")
  }
  v$result()
}

#' @describeIn v_model_objects validates that the logistic Kadane model
#'   parameters with a beta and gamma prior are valid.
v_model_logistic_kadane_beta_gamma <- function(object) { # nolintr
  v <- Validate()
  v$check(
    is.scalar(object@alpha) & is.numeric(object@alpha) && object@alpha > 0,
    "Beta distribution shape parameter alpha must be a positive scalar"
  )
  v$check(
    is.scalar(object@beta) & is.numeric(object@beta) && object@beta > 0,
    "Beta distribution shape parameter beta must be a positive scalar"
  )
  v$check(
    is.scalar(object@shape) & is.numeric(object@shape) && object@shape > 0,
    "Gamma distribution shape parameter must be a positive scalar"
  )
  v$check(
    is.scalar(object@rate) & is.numeric(object@rate) && object@rate > 0,
    "Gamma distribution rate parameter must be a positive scalar"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `weightpar` is valid.
v_model_logistic_normal_mix <- function(object) {
  v <- Validate()
  v$check(
    h_test_named_numeric(object@weightpar, permutation.of = c("a", "b")),
    "weightpar must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `component` is a list with
#'   valid `ModelParamsNormal` objects as well as `weights` are correct.
v_model_logistic_normal_fixed_mix <- function(object) { # nolintr
  v <- Validate()
  v$check(
    all(sapply(object@components, test_class, "ModelParamsNormal")),
    "components must be a list with ModelParamsNormal S4 class objects"
  )
  comp_valid_result <- sapply(object@components, validObject, test = TRUE)
  comp_valid <- sapply(comp_valid_result, isTRUE)
  v$check(
    all(comp_valid),
    paste(
      "components must be a list with valid ModelParamsNormal S4 class objects",
      paste(unlist(comp_valid_result[!comp_valid]), collapse = ", "),
      collapse = ", ",
      sep = ", "
    )
  )
  v$check(
    length(object@components) == length(object@weights),
    "components must have same length as weights"
  )
  v$check(
    test_numeric(object@weights, lower = .Machine$double.xmin, finite = TRUE, any.missing = FALSE),
    "weights must be positive"
  )
  v$check(
    sum(object@weights) == 1,
    "weights must sum to 1"
  )
  v$check(
    test_flag(object@log_normal),
    "log_normal must be TRUE or FALSE"
  )
  v$result()
}

#' @describeIn v_model_objects validates that `share_weight` represents probability.
v_model_logistic_log_normal_mix <- function(object) { # nolintr
  v <- Validate()
  v$check(
    test_probability(object@share_weight),
    "share_weight does not specify a probability"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpoint`] class slots are valid.
v_model_dual_endpoint <- function(object) {
  rmin <- .Machine$double.xmin
  v <- Validate()

  v$check(
    test_flag(object@use_log_dose),
    "use_log_dose must be TRUE or FALSE"
  )
  uf_sigma2W <- object@use_fixed["sigma2W"] # nolintr
  v$check(
    test_flag(uf_sigma2W),
    "use_fixed must be a named logical vector that contains name 'sigma2W'"
  )
  uf_rho <- object@use_fixed["rho"]
  v$check(
    test_flag(uf_rho),
    "use_fixed must be a named logical vector that contains name 'rho'"
  )

  if (isTRUE(uf_sigma2W)) {
    v$check(
      test_number(object@sigma2W, lower = rmin, finite = TRUE),
      "sigma2W must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2W is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2W, permutation.of = c("a", "b")),
      "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }

  if (isTRUE(uf_rho)) {
    v$check(
      test_number(object@rho, lower = -1 + rmin, upper = 1 - rmin), # rmin is ignored here!
      "rho must be a number in (-1, 1)"
    )
  } else {
    # object@rho is a vector with parameters for Beta(a, b).
    v$check(
      h_test_named_numeric(object@rho, permutation.of = c("a", "b")),
      "rho must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }

  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointRW`] class slots are valid.
v_model_dual_endpoint_rw <- function(object) {
  v <- Validate()
  uf_sigma2W <- object@use_fixed["sigma2betaW"] # nolintr
  v$check(
    test_flag(uf_sigma2W),
    "use_fixed must be a named logical vector that contains name 'sigma2betaW'"
  )
  if (isTRUE(uf_sigma2W)) {
    v$check(
      test_number(object@sigma2betaW, lower = .Machine$double.xmin, finite = TRUE),
      "sigma2betaW must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2betaW is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2betaW, permutation.of = c("a", "b")),
      "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }
  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointBeta`] class slots are valid.
v_model_dual_endpoint_beta <- function(object) {
  v <- Validate()

  for (s in c("E0", "Emax", "delta1", "mode")) {
    rmin <- .Machine$double.xmin
    uf <- object@use_fixed[s]

    v$check(
      test_flag(uf),
      paste0("use_fixed must be a named logical vector that contains name '", s, "'")
    )
    if (isTRUE(uf)) {
      if (s %in% c("delta1", "mode")) {
        v$check(
          test_number(slot(object, s), lower = rmin, finite = TRUE),
          paste(s, "must be a positive and finite numerical scalar")
        )
      }
    } else {
      # s is a vector with parameters for Uniform(s[1], s[2]) prior.
      v$check(
        test_numeric(
          slot(object, s),
          lower = 0,
          finite = TRUE,
          any.missing = FALSE,
          len = 2,
          unique = TRUE,
          sorted = TRUE
        ),
        paste(s, "must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values")
      )
    }
  }

  v$result()
}

#' @describeIn v_model_objects validates that [`DualEndpointEmax`] class slots are valid.
v_model_dual_endpoint_emax <- function(object) {
  v <- Validate()

  for (s in c("E0", "Emax", "ED50")) {
    rmin <- .Machine$double.xmin
    uf <- object@use_fixed[s]

    v$check(
      test_flag(uf),
      paste0("use_fixed must be a named logical vector that contains name '", s, "'")
    )
    if (isTRUE(uf)) {
      v$check(
        test_number(slot(object, s), lower = rmin, finite = TRUE),
        paste(s, "must be a positive and finite numerical scalar")
      )
    } else {
      # s is a vector with parameters for Uniform(s[1], s[2]) prior.
      v$check(
        test_numeric(
          slot(object, s),
          lower = 0,
          finite = TRUE,
          any.missing = FALSE,
          len = 2,
          unique = TRUE,
          sorted = TRUE
        ),
        paste(s, "must be a numerical vector of length two with non-negative, finite, unique and sorted (asc.) values")
      )
    }
  }

  v$result()
}

#' @describeIn v_model_objects validates that [`LogisticIndepBeta`] class slots are valid.
v_model_logistic_indep_beta <- function(object) {
  v <- Validate()

  dle_len <- length(object@binDLE)
  v$check(
    test_numeric(object@binDLE, finite = TRUE, any.missing = FALSE, min.len = 2),
    "binDLE must be a finite numerical vector of minimum length 2, without missing values"
  )
  v$check(
    test_numeric(object@DLEdose, finite = TRUE, any.missing = FALSE, len = dle_len),
    "DLEdose must be a finite numerical vector of the same length as 'binDLE', without missing values"
  )
  v$check(
    test_integer(object@DLEweights, any.missing = FALSE, len = dle_len),
    "DLEweights must be an integer vector of the same length as 'binDLE', without missing values"
  )
  v$check(
    test_number(object@phi1),
    "phi1 must be a numerical scalar"
  )
  v$check(
    test_number(object@phi2),
    "phi2 must be a numerical scalar"
  )
  v$check(
    h_is_positive_definite(object@Pcov),
    "Pcov must be 2x2 positive-definite matrix without any missing values"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`Effloglog`] class slots are valid.
v_model_eff_log_log <- function(object) {
  rmin <- .Machine$double.xmin

  v <- Validate()
  v$check(
    test_numeric(object@eff, finite = TRUE, any.missing = FALSE, min.len = 2),
    "eff must be a finite numerical vector of minimum length 2, without missing values"
  )
  eff_dose_ok <- test_numeric(
    object@eff_dose,
    lower = rmin, finite = TRUE, any.missing = FALSE, len = length(object@eff)
  )
  v$check(
    eff_dose_ok,
    "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
  )
  v$check(
    test_flag(object@use_fixed),
    "use_fixed must be a flag"
  )
  if (isTRUE(object@use_fixed)) {
    v$check(
      test_number(object@nu, lower = rmin, finite = TRUE),
      "nu must be a positive and finite numerical scalar"
    )
  } else {
    # object@nu is a vector with parameters for Gamma(a, b).
    v$check(
      h_test_named_numeric(object@nu, permutation.of = c("a", "b")),
      "nu must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }
  const_ok <- test_number(object@const, lower = 0)
  v$check(const_ok, "const must be a non-negative number")
  if (eff_dose_ok && const_ok) {
    v$check(
      min(object@data@doseGrid, object@eff_dose) > 1 - object@const,
      "For log-log model, doses and const must be such that dose + const > 1"
    )
  }
  v$check(
    test_number(object@theta1),
    "theta1 must be a numerical scalar"
  )
  v$check(
    test_number(object@theta2),
    "theta2 must be a numerical scalar"
  )
  nobs_no_dlt <- sum(!object@data@y)
  if (nobs_no_dlt + length(object@eff) > 2) {
    v$check(
      h_is_positive_definite(object@Pcov),
      "Pcov must be 2x2 positive-definite matrix without any missing values"
    )
  } else {
    v$check(
      test_matrix(object@Pcov, mode = "numeric", nrows = 2, ncols = 2) && all(is.na(object@Pcov)),
      "Pcov must be 2x2 numeric matrix with all values missing if the length of combined data is 2"
    )
  }
  v$check(
    test_numeric(object@mu, finite = TRUE, len = 2),
    "mu must be a finite numerical vector of length 2"
  )
  Xnrow <- ifelse(nobs_no_dlt > 0, nobs_no_dlt, length(object@eff_dose))
  v$check(
    test_matrix(object@X, mode = "numeric", nrows = Xnrow, ncols = 2, any.missing = FALSE),
    paste(
      "X must be a finite numerical matrix of size",
      Xnrow,
      "x 2, without any missing values"
    )
  )
  v$check(
    all(object@X[, 1] == 1),
    "X must be a design matrix, i.e. first column must be of 1s"
  )
  v$check(
    h_is_positive_definite(object@Q),
    "Q must be 2x2 positive-definite matrix without any missing values"
  )
  v$check(
    test_numeric(object@Y, finite = TRUE, any.missing = FALSE, len = Xnrow),
    paste(
      "Y must be a finite numerical vector of length",
      Xnrow,
      "and without any missing values"
    )
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`EffFlexi`] class slots are valid.
v_model_eff_flexi <- function(object) {
  rmin <- .Machine$double.xmin

  v <- Validate()
  v$check(
    test_numeric(object@eff, finite = TRUE, any.missing = FALSE, min.len = 2),
    "eff must be a finite numerical vector of minimum length 2, without missing values"
  )
  v$check(
    test_numeric(
      object@eff_dose,
      lower = rmin, finite = TRUE, any.missing = FALSE, len = length(object@eff)
    ),
    "eff_dose must be a finite numerical vector of the same length as 'eff', without missing values"
  )

  uf_sigma2W <- object@use_fixed["sigma2W"] # nolintr
  v$check(
    test_flag(uf_sigma2W),
    "use_fixed must be a named logical vector that contains name 'sigma2W'"
  )
  uf_sigma2betaW <- object@use_fixed["sigma2betaW"] # nolintr
  v$check(
    test_flag(uf_sigma2betaW),
    "use_fixed must be a named logical vector that contains name 'sigma2betaW'"
  )

  if (isTRUE(uf_sigma2W)) {
    v$check(
      test_number(object@sigma2W, lower = rmin, finite = TRUE),
      "sigma2W must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2W is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2W, permutation.of = c("a", "b")),
      "sigma2W must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }
  if (isTRUE(uf_sigma2betaW)) {
    v$check(
      test_number(object@sigma2betaW, lower = rmin, finite = TRUE),
      "sigma2betaW must be a positive and finite numerical scalar"
    )
  } else {
    # object@sigma2betaW is a vector with parameters for InverseGamma(a, b).
    v$check(
      h_test_named_numeric(object@sigma2betaW, permutation.of = c("a", "b")),
      "sigma2betaW must be a named numerical vector of length two with positive finite values and names 'a', 'b'"
    )
  }

  v$check(
    test_flag(object@rw1),
    "rw1 must be a flag"
  )
  v$check(
    test_matrix(object@X, mode = "integer", ncols = object@data@nGrid, any.missing = FALSE),
    paste("X must be an integer matrix with", object@data@nGrid, "columns and without any missing values")
  )
  v$check(
    all(object@X == 0L | object@X == 1L),
    "X must be a matrix with 0-1 values only"
  )
  v$check(
    test_matrix(object@RW, nrows = object@data@nGrid, ncols = object@data@nGrid, any.missing = FALSE),
    paste0("RW must be ", object@data@nGrid, "x", object@data@nGrid, " matrix without any missing values")
  )
  v$check(
    test_int(object@RW_rank) && (object@RW_rank == (object@data@nGrid - ifelse(isTRUE(object@rw1), 1L, 2L))),
    "RW_rank must be an integer equal to data@nGrid - 2L"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`DALogisticLogNormal`] class slots are valid.
v_model_da_logistic_log_normal <- function(object) {
  v <- Validate()

  npiece_ok <- test_int(object@npiece)
  v$check(npiece_ok, "npiece must be a is a single integerish value")
  if (npiece_ok) {
    v$check(
      test_numeric(object@l, lower = 0, finite = TRUE, any.missing = FALSE, len = object@npiece),
      "prior parameter vector l of lambda must be a non-negative vector of length equal to npiece"
    )
  }
  v$check(
    test_number(object@c_par, finite = TRUE),
    "c_par must be a finite numerical scalar"
  )
  v$check(
    test_flag(object@cond_pem),
    "cond_pem must be a flag"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`TITELogisticLogNormal`] class slots are valid.
v_model_tite_logistic_log_normal <- function(object) { # nolintr
  v <- Validate()
  v$check(
    test_string(object@weight_method, pattern = "^linear$|^adaptive$"),
    "weight_method must be a string equal either to linear or adaptive"
  )
  v$result()
}

#' @describeIn v_model_objects validates that [`OneParExpNormalPrior`] class slots are valid.
v_model_one_par_exp_normal_prior <- function(object) { # nolintr
  v <- Validate()
  y <- seq(from = 0, to = 1, by = 0.1) # Probabilities.
  x <- object@skel_fun_inv(y) # Dose grid.
  not_na <- !is.na(x)
  v$check(
    isTRUE(all.equal(object@skel_fun(x[not_na]), y[not_na])),
    "skel_fun_inv must be an inverse funtion of skel_fun function"
  )
  v$check(
    test_probabilities(object@skel_probs),
    "skel_probs must be probabilities between 0 and 1"
  )
  v$check(
    test_number(object@sigma2, lower = .Machine$double.xmin, finite = TRUE),
    "sigma2 must be a positive finite number"
  )
  v$result()
}
