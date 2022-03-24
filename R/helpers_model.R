#' Update [`DualEndpoint`] class model components with regard to biomarker
#' regression variance.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] model existing components
#' (`priormodel`, `modelspecs`, `init`, `sample`), and updates them with regard to
#' to biomarker regression variance `sigma2W`.
#'
#' @param use_fixed (`flag`)\cr indicates whether a fixed value for the biomarker
#'   regression variance `sigma2W` should be used or not. If `sigma2W` is not
#'   supposed to be a fixed value, a prior distribution from the Inverse-Gamma
#'   distribution will be used. See the details below, under `sigma2W` argument.
#' @param sigma2W (`numeric`)\cr the biomarker variance. Either a fixed value or
#'   Inverse-Gamma distribution parameters, i.e. vector with two elements named
#'   `a` and `b`.
#' @param comp (`list`)\cr a named list with model components that will be updated.
#'   The names should be: `priormodel`, `modelspecs`, `init`, `sample`. For
#'   definitions of the components, see [`GeneralModel`] class.
#'   The `modelspecs` and `init` components on `comp` list are specified up to
#'   the body of corresponding `GeneralModel@modelspecs` and `GeneralModel@init`
#'   functions. These bodies are simply a lists itself.
#'
#' @return `list` with updated model components.
#'
#' @export
h_model_dual_endpoint_sigma2W <- function(use_fixed,
                                          sigma2W,
                                          comp) {
  if (use_fixed) {
    assert_number(sigma2W, lower = 0 + .Machine$double.xmin, finite = TRUE)
    comp$modelspecs <- c(comp$modelspecs, list(precW = 1 / sigma2W))
  } else {
    assert_true(h_test_named_numeric(sigma2W, permutation.of = c("a", "b")))
    comp$priormodel <- h_jags_join_models(
      comp$priormodel,
      function() {
        precW ~ dgamma(precWa, precWb)
      }
    )
    comp$modelspecs <- c(
      comp$modelspecs,
      list(precWa = sigma2W[["a"]], precWb = sigma2W[["b"]])
    )
    comp$init$precW <- 1
    comp$sample <- c(comp$sample, "precW")
  }
  comp
}

#' Update [`DualEndpoint`] class model components with regard to DLT and biomarker
#' correlation.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] model existing components
#' (`priormodel`, `modelspecs`, `init`, `sample`), and updates them with regard to
#' DLT and biomarker correlation `rho`.
#'
#' @param use_fixed (`flag`)\cr indicates whether a fixed value for DLT and
#'   biomarker correlation `rho` should be used or not. If `rho` is not supposed
#'   to be a fixed value, a prior distribution from the scaled Beta family will
#'   be used. See the details below, under `rho` argument.
#' @param rho (`numeric`)\cr DLT and biomarker correlation. It must be either a
#'   fixed value (between `-1` and `1`), or a named vector with two elements,
#'   named `a` and `b` for the Beta prior on the transformation
#'   `kappa = (rho + 1) / 2`, which is in `(0, 1)`. For example, `a = 1, b = 1`
#'   leads to a uniform prior on `rho`.
#' @param comp (`list`)\cr a named list with model components that will be updated.
#'   The names should be: `priormodel`, `modelspecs`, `init`, `sample`. For
#'   definitions of the components, see [`GeneralModel`] class.
#'   The `modelspecs` and `init` components on `comp` list are specified up to
#'   the body of corresponding `GeneralModel@modelspecs` and `GeneralModel@init`
#'   functions. These bodies are simply a lists itself.
#'
#' @return A `list` with updated model components.
#'
#' @export
h_model_dual_endpoint_rho <- function(use_fixed,
                                      rho,
                                      comp) {
  rmin <- .Machine$double.xmin
  if (use_fixed) {
    assert_number(rho, lower = -1 + rmin, upper = 1 - rmin)
    comp$modelspecs <- c(comp$modelspecs, list(rho = rho))
  } else {
    assert_true(h_test_named_numeric(rho, permutation.of = c("a", "b")))
    comp$priormodel <- h_jags_join_models(
      comp$priormodel,
      function() {
        kappa ~ dbeta(rhoa, rhob)
        rho <- 2 * kappa - 1
      }
    )
    comp$modelspecs <- c(
      comp$modelspecs,
      list(rhoa = rho[["a"]], rhob = rho[["b"]])
    )
    comp$init$kappa <- 0.5
    comp$sample <- c(comp$sample, "rho")
  }
  comp
}

#' Update certain components of [`DualEndpoint`] model with regard to prior variance
#' factor of the random walk.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] object and updates
#' `priormodel`, `modelspecs`, `init`, `sample` slots according to the random walk
#' variance.
#'
#' @param use_fixed (`flag`)\cr indicates whether a fixed value for
#'   `sigma2betaW` should be used or not. If `sigma2betaW` is not supposed
#'   to be a fixed value, a prior distribution from the Inverse-Gamma distribution
#'   will be used. See the details below, under `sigma2betaW` argument.
#' @param sigma2betaW (`numeric`)\cr the prior variance factor of the random walk
#'   prior for the biomarker model. Either a fixed value or Inverse-Gamma distribution
#'   parameters, i.e. vector with two elements named `a` and `b`.
#' @param de (`DualEnpoint`)\cr dual endpoint model whose slots will be updated.
#'
#' @return A [`DualEndpoint`] model with updated `priormodel`, `modelspecs`,
#'   `init`, `sample` slots.
#'
#' @export
h_model_dual_endpoint_sigma2betaW <- function(use_fixed,
                                              sigma2betaW,
                                              de) {
  modelspecs <- de@modelspecs
  init <- de@init

  if (use_fixed) {
    assert_number(sigma2betaW, lower = 0 + .Machine$double.xmin, finite = TRUE)
    de@modelspecs <- function() {
      c(modelspecs(), list(precBetaW = 1 / sigma2betaW))
    }
  } else {
    assert_true(h_test_named_numeric(sigma2betaW, permutation.of = c("a", "b")))
    # gamma prior for random walk precision.
    de@priormodel <- h_jags_join_models(
      de@priormodel,
      function() {
        precBetaW ~ dgamma(precBetaWa, precBetaWb)
      }
    )
    de@modelspecs <- function() {
      c(
        modelspecs(),
        list(precBetaWa = sigma2betaW[["a"]], precBetaWb = sigma2betaW[["b"]])
      )
    }
    de@init <- function(y) {
      c(init(y), list(precBetaW = 1))
    }
    de@sample <- c(de@sample, "precBetaW")
  }
  de
}

#' @export
h_model_dual_endpoint_beta <- function(use_fixed,
                                       param,
                                       param_name,
                                       param_prefix_lh = c("_low", "_high"),
                                       prior,
                                       de) {
  assert_flag(use_fixed)
  assert_numeric(param, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_string(param_name)
  assert_character(param_prefix_lh, len = 2, unique = TRUE, any.missing = FALSE)
  assert_function(prior)
  assert_class(de, "DualEndpoint")

  param_name_lh <- paste0(param_name, param_prefix_lh)
  ms <- de@modelspecs

  if (use_fixed) {
    de@modelspecs <- function() {
      c(ms(), setNames(list(param), param_name))
    }
  } else {
    init <- de@init
    de@priormodel <- h_jags_join_models(
      de@priormodel,
      prior
    )
    de@modelspecs <- function() {
      c(
        ms(),
        setNames(list(param[1], param[2]), param_name_lh)
      )
    }
    de@init <- function(y) {
      c(
        init(y),
        setNames(list(mean(param)), param_name)
      )
    }
    de@sample <- c(de@sample, param_name)
  }
  de
}
