#' Update `DualEndpoint` class model components with regard to biomarker
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
#' @param sigma2W (`numeric`)\cr the biomarker regression variance. It must be
#'   either a fixed value or a numeric vector with two elements named `a` and `b`
#'   for the Inverse-Gamma prior parameters.
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

#' Update `DualEndpoint` class model components with regard to DLT and biomarker
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
