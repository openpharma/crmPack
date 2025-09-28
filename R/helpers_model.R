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
h_model_dual_endpoint_sigma2W <- function(
  use_fixed, # nolintr
  sigma2W,
  comp
) {
  if (use_fixed) {
    assert_number(sigma2W, lower = 0 + .Machine$double.xmin, finite = TRUE)
    comp$modelspecs$precW <- 1 / sigma2W
  } else {
    assert_true(h_test_named_numeric(sigma2W, permutation.of = c("a", "b")))
    comp$priormodel <- h_jags_join_models(
      comp$priormodel,
      function() {
        precW ~ dgamma(precWa, precWb)
      }
    )
    comp$modelspecs$precWa <- sigma2W[["a"]]
    comp$modelspecs$precWb <- sigma2W[["b"]]
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
h_model_dual_endpoint_rho <- function(use_fixed, rho, comp) {
  rmin <- .Machine$double.xmin
  if (use_fixed) {
    assert_number(rho, lower = -1 + rmin, upper = 1 - rmin)
    comp$modelspecs$rho <- rho
  } else {
    assert_true(h_test_named_numeric(rho, permutation.of = c("a", "b")))
    comp$priormodel <- h_jags_join_models(
      comp$priormodel,
      function() {
        kappa ~ dbeta(rhoa, rhob)
        rho <- 2 * kappa - 1
      }
    )
    comp$modelspecs$rhoa <- rho[["a"]]
    comp$modelspecs$rhob <- rho[["b"]]
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
#' @seealso [`DualEndpointRW`].
#' @export
h_model_dual_endpoint_sigma2betaW <- function(
  use_fixed, # nolintr
  sigma2betaW,
  de
) {
  modelspecs <- de@modelspecs
  init <- de@init

  if (use_fixed) {
    assert_number(sigma2betaW, lower = 0 + .Machine$double.xmin, finite = TRUE)
    ms <- list(precBetaW = 1 / sigma2betaW)
  } else {
    assert_true(h_test_named_numeric(sigma2betaW, permutation.of = c("a", "b")))
    # gamma prior for random walk precision.
    de@priormodel <- h_jags_join_models(
      de@priormodel,
      function() {
        precBetaW ~ dgamma(precBetaWa, precBetaWb)
      }
    )
    ms <- list(precBetaWa = sigma2betaW[["a"]], precBetaWb = sigma2betaW[["b"]])
    de@init <- function(y) {
      c(init(y), list(precBetaW = 1))
    }
    de@sample <- c(de@sample, "precBetaW")
  }
  de@modelspecs <- function(from_prior) {
    c(modelspecs(from_prior), ms)
  }
  de
}

#' Update certain components of [`DualEndpoint`] model with regard to parameters
#' of the function that models dose-biomarker relationship defined in the
#' [`DualEndpointBeta`] class.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] object and updates
#' `use_fixed`, `priormodel`, `modelspecs`, `init`, `sample` slots with regard
#' to a given parameter of the dose-biomarker relationship \eqn{f(x)} defined in
#' the [`DualEndpointBeta`] class. This update solely depends on whether a given
#' parameter's value `param` is a fixed-valued scalar or two-elements numeric
#' vector. In the later case, it is assumed that `param` represents two
#' parameters of a probability distribution that will be used in `priormodel`
#' function to generate values for the `param_name` parameter of \eqn{f(x)}.
#' See the help page for [`DualEndpointBeta`] class for more details.
#'
#' @param param (`numeric`)\cr the value of a given `param_name` parameter of
#'   the dose-biomarker relationship function \eqn{f(x)}. Either a fixed-valued
#'   scalar or vector with two elements that are the parameters of a probability
#'   distribution that will be used in `priormodel` function to generate values
#'   for the `param_name` parameter of \eqn{f(x)}.
#' @param param_name (`string`)\cr the name of the parameter of \eqn{f(x)},
#'   whose value depends on `param`.
#' @param param_suffix (`character`)\cr the two suffixes to be appended to
#'   the elements of `param_name` and then used when updating `modelspecs`.
#'   The value of this argument is ignored when `param` is a scalar.
#' @param priormodel (`function` or `NULL`)\cr a function representing the
#'   `JAGS` prior specification that will be appended to existing
#'   `de@priormodel` specification if `param` is not a scalar. Otherwise,
#'   `de@priormodel` remains unchanged.
#' @param de (`DualEnpoint`)\cr dual endpoint model whose slots will be updated.
#'
#' @return A [`DualEndpoint`] model with updated `use_fixed`, `priormodel`,
#'   `modelspecs`, `init`, `sample` slots.
#'
#' @export
h_model_dual_endpoint_beta <- function(
  param,
  param_name,
  param_suffix = c("_low", "_high"),
  priormodel = NULL,
  de
) {
  assert_numeric(param, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_string(param_name)
  assert_class(de, "DualEndpoint")

  use_fixed <- setNames(test_number(param), param_name)
  modelspecs <- de@modelspecs
  init <- de@init

  if (use_fixed) {
    ms <- setNames(list(param), param_name)
  } else {
    assert_character(param_suffix, len = 2, unique = TRUE, any.missing = FALSE)
    assert_function(priormodel)
    param_name2 <- paste0(param_name, param_suffix)

    de@priormodel <- h_jags_join_models(
      de@priormodel,
      priormodel
    )
    ms <- setNames(list(param[1], param[2]), param_name2)
    de@init <- function(y) {
      c(init(y), setNames(list(mean(param)), param_name))
    }
    de@sample <- c(de@sample, param_name)
  }
  de@modelspecs <- function(from_prior) {
    c(modelspecs(from_prior), ms)
  }
  de@use_fixed <- c(de@use_fixed, use_fixed)
  de
}

#' Convert an ordinal CRM model to the Equivalent Binary CRM Model for a Specific
#' Grade
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that takes a [`LogisticLogNormalOrdinal`] and an
#' integer grade and converts them to the equivalent `LogisticLogNormal` model.
#'
#' @param x (`LogisticLogNormalOrdinal`)\cr the `LogisticLogNormalOrdinal`
#'   model to covert
#' @param grade (`integer`)\cr the toxicity grade for which the equivalent model
#' is required.
#' @return A [`LogisticLogNormal`] model.
#'
#' @export
h_convert_ordinal_model <- function(x, grade) {
  # Validate
  assert_integer(grade, len = 1, lower = 1)
  assert_class(x, "LogisticLogNormalOrdinal")
  # Execute
  LogisticLogNormal(
    mean = x@params@mean[-grade],
    cov = x@params@cov[-grade, -grade],
    ref_dose = x@ref_dose
  )
}
