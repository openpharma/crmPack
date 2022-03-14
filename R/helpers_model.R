#' Prepare `DualEndpoint` class model components with regard to biomarker
#' regression variance.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] model existing components
#' (`priormodel`, `modelspecs`, `init`, `sample`), and updates them with regard to
#' to biomarker regression variance `sigma2W`.
#'
#' @param use_fixed (`flag`)\cr TODO
#' @param sigma2W (`numeric`)\cr TODO
#' @param priormodel (`function`)\cr TODO
#' @param modelspecs (`function`)\cr TODO
#' @param init (`function`)\cr TODO
#' @param sample (`character`)\cr TODO
#'
#' @return `list` with updated model components.
#'
#' @export
h_model_dual_endpoint_sigma2W <- function(use_fixed,
                                          sigma2W,
                                          priormodel,
                                          modelspecs,
                                          init,
                                          sample) {
  if (use_fixed) {
    modelspecs <- c(modelspecs, list(precW = 1 / sigma2W))
  } else {
    priormodel <- h_jags_join_models(
      priormodel,
      function() {
        precW ~ dgamma(precWa, precWb)
      }
    )
    modelspecs <- c(modelspecs, list(precWa = sigma2W["a"], precWb = sigma2W["b"]))
    init$precW <- 1
    sample <- c(sample, "precW")
  }

  list(
    priormodel = priormodel,
    modelspecs = modelspecs,
    init = init,
    sample = sample
  )
}

#' Prepare `DualEndpoint` class model components with regard to DLT and biomarker
#' correlation.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that takes [`DualEndpoint`] model existing components
#' (`priormodel`, `modelspecs`, `init`, `sample`), and updates them with regard to
#' DLT and biomarker correlation `rho`.
#'
#' @param use_fixed (`flag`)\cr TODO
#' @param rho (`numeric`)\cr TODO
#' @param priormodel (`function`)\cr TODO
#' @param modelspecs (`function`)\cr TODO
#' @param init (`function`)\cr TODO
#' @param sample (`character`)\cr TODO
#'
#' @return `list` with updated model components.
#'
#' @export
h_model_dual_endpoint_rho <- function(use_fixed,
                                      rho,
                                      priormodel,
                                      modelspecs,
                                      init,
                                      sample) {
  if (use_fixed) {
    modelspecs <- c(modelspecs, list(rho = rho))
  } else {
    priormodel <- h_jags_join_models(
      priormodel,
      function() {
        kappa ~ dbeta(rhoa, rhob)
        rho <- 2 * kappa - 1
      }
    )
    modelspecs <- c(modelspecs, list(rhoa = rho["a"], rhob = rho["b"]))
    init$kappa <- 0.5
    sample <- c(sample, "rho")
  }

  list(
    priormodel = priormodel,
    modelspecs = modelspecs,
    init = init,
    sample = sample
  )
}
