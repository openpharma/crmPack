#' @include helpers.R
#' @include helpers_jags.R
#' @include Model-validity.R
#' @include ModelParams-class.R
NULL

# AllModels-class ----

#' `AllModels`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`AllModels`] is a class from which all the models inherit.
#'
#' @slot datanames (`character`)\cr the names of all data slots that are used
#'   in all the models. In particular, those are also used in the `datamodel` or
#'   `priormodel` definition for [`GeneralModel`].
#'
#' @aliases AllModels
#' @export
#'
.AllModels <- setClass(
  Class = "AllModels",
  slots = c(datanames = "character")
)

# GeneralModel-class ----

#' `GeneralModel`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`GeneralModel`] is a general model class, from which all other specific
#' model-like classes inherit. The [`GeneralModel`] class inherits from
#' [`AllModels`].
#'
#' @note The `datamodel` must obey the convention that the data input is
#'   called exactly in the same way as in the corresponding data class.
#'   All prior distributions for parameters should be contained in the
#'   model function `priormodel`. The background is that this can
#'   be used to simulate from the prior distribution, before obtaining any data.
#'
#' @slot datamodel (`function`)\cr a function representing the `JAGS` data model
#'   specification.
#' @slot priormodel (`function`)\cr a function representing the `JAGS` prior
#'   specification.
#' @slot modelspecs (`function`)\cr a function computing the list of the data
#'   model and prior model specifications that are required to be specified
#'   completely (e.g. prior parameters, reference dose, etc.), based on the data
#'   slots that are required as arguments of this function.
#' @slot init (`function`)\cr a function computing the list of starting values
#'   for parameters required to be initialized in the MCMC sampler, based on the
#'   data slots that are required as arguments of this function.
#' @slot sample (`character`)\cr names of all parameters from which you would
#'   like to save the MCMC samples.
#'
#' @aliases GeneralModel
#' @export
#'
.GeneralModel <- setClass(
  Class = "GeneralModel",
  contains = "AllModels",
  slots = c(
    datamodel = "function",
    priormodel = "function",
    modelspecs = "function",
    init = "function",
    sample = "character"
  ),
  prototype = prototype(
    datamodel = I,
    priormodel = I,
    init = function() {
      list()
    }
  ),
  validity = v_general_model
)

# Model-class (TO REMOVE) ----

#' `Model`
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' [`Model`] is the old class for single agent dose escalation, from which all
#' other specific models inherit. The [`Model`] class inherits from
#' [`GeneralModel`]. It will be soon removed as the `dose` and `prob` are
#' moved to a separate S4 class-specific methods.
#'
#' @note The `datamodel` must obey the convention that the data input is called
#'   exactly as in the [`Data`] class. All prior distributions for parameters
#'   should be contained in the model function `priormodel`. The background is
#'   that this can be used to simulate from the prior distribution, before
#'   obtaining any data.
#'
#' @details The first argument of `dose` function must be the `prob`, which is a
#'   scalar toxicity probability which is targeted. Further arguments are the
#'   model parameters. The `dose` function computes, using model parameter(s)
#'   (samples), the resulting dose. The model parameters are called exactly as
#'   in the `model` and must be included in the `sample` vector. The vectors
#'   of all samples for these parameters will then be supplied to the function.
#'   Hence, a user function must be able to handle vectorized model parameters.
#'
#'   The first argument of `prob` function must be the `dose`, which is a scalar
#'   dose. Further arguments are the model parameters. The `prob` function
#'   computes, using model parameter(s) (samples), the resulting probability of
#'   toxicity at that dose. Again here, the function should support vectorized
#'   model parameters.
#'
#'   Note that `dose` and `prob` are the inverse functions of each other.
#'
#'   If you work with multivariate parameters, then assume that your functions
#'   receive either one parameter value as a row vector, or a samples matrix
#'   where the rows correspond to the sampling index, i.e. the layout is then
#'   `nSamples x dimParameter`.
#'
#' @slot dose (`function`)\cr a function computing the dose reaching a specific
#'   target probability, based on the model parameters and additional prior
#'   settings (see the details above).
#' @slot prob (`function`)\cr a function computing the probability of toxicity
#'   for a specific dose, based on the model parameters and additional prior
#'   settings (see the details above).
#'
#'
#' @aliases Model
#' @export
#'
.Model <- setClass(
  Class = "Model",
  contains = "GeneralModel",
  slots = c(
    dose = "function",
    prob = "function"
  ),
  prototype = prototype(
    dose = function(prob) {},
    prob = function(dose) {}
  ),
  validity = v_model
)

# ModelLogNormal ----

## class ----

#' `ModelLogNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ModelLogNormal`] is the class for a model with a reference dose and bivariate
#' normal prior on the model parameters `alpha0` and natural logarithm of `alpha1`,
#' i.e.: \deqn{(alpha0, log(alpha1)) ~ Normal(mean, cov),}. Transformations other
#' than `log`, e.g. identity, can be specified too in `priormodel` slot.
#' The parameter `alpha1` has a log-normal distribution by default to ensure
#' positivity of `alpha1` which further guarantees `exp(alpha1) > 1`.
#' The slots of this class contain the mean vector, the covariance and
#' precision matrices of the bivariate normal distribution, as well as the
#' reference dose. Note that the precision matrix is an inverse of the
#' covariance matrix in the `JAGS`.
#' All ("normal") model specific classes inherit from this class.
#'
#' @slot params (`ModelParamsNormal`)\cr bivariate normal prior parameters.
#' @slot ref_dose (`number`)\cr the reference dose.
#'
#' @seealso [`ModelParamsNormal`], [`LogisticNormal`], [`LogisticLogNormal`],
#'   [`LogisticLogNormalSub`], [`ProbitLogNormal`], [`ProbitLogNormalRel`].
#'
#' @aliases ModelLogNormal
#' @export
#'
.ModelLogNormal <- setClass(
  Class = "ModelLogNormal",
  contains = "Model",
  slots = c(
    params = "ModelParamsNormal",
    ref_dose = "numeric"
  ),
  validity = v_model_log_normal
)

## constructor ----

#' @rdname ModelLogNormal-class
#'
#' @param mean (`numeric`)\cr the prior mean vector.
#' @param cov (`matrix`)\cr the prior covariance matrix. The precision matrix
#'   `prec` is internally calculated as an inverse of `cov`.
#' @param ref_dose (`number`)\cr the reference dose.
#'
#' @export
#'
ModelLogNormal <- function(mean, cov, ref_dose = 0) {
  params <- ModelParamsNormal(mean, cov)
  .ModelLogNormal(
    params = params,
    ref_dose = ref_dose,
    priormodel = function() {
      theta ~ dmnorm(mean, prec)
      alpha0 <- theta[1]
      alpha1 <- exp(theta[2])
    },
    modelspecs = function() {
      list(ref_dose = ref_dose, mean = params@mean, prec = params@prec)
    },
    init = function() {
      list(theta = c(0, 1))
    },
    sample = c("alpha0", "alpha1"),
    datanames = c("nObs", "y", "x")
  )
}

# LogisticNormal ----

## class ----

#' `LogisticNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticNormal`] is the class for the usual logistic regression model with
#' a bivariate normal prior on the intercept and slope.
#'
#' @details The covariate is the natural logarithm of the dose \eqn{x} divided by
#'   the reference dose \eqn{x*}, i.e.:
#'   \deqn{logit[p(x)] = alpha0 + alpha1 * log(x/x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior \deqn{(alpha0, alpha1) ~ Normal(mean, cov).}
#'
#' @seealso [`ModelLogNormal`], [`LogisticLogNormal`], [`LogisticLogNormalSub`],
#'   [`ProbitLogNormal`], [`ProbitLogNormalRel`], [`LogisticNormalMixture`].
#'
#' @aliases LogisticNormal
#' @export
#'
.LogisticNormal <- setClass(
  Class = "LogisticNormal",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname LogisticNormal-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-LogisticNormal.R
#'
LogisticNormal <- function(mean, cov, ref_dose = 0) {
  model_ln <- ModelLogNormal(mean = mean, cov = cov, ref_dose = ref_dose)

  .LogisticNormal(
    model_ln,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      theta ~ dmnorm(mean, prec)
      alpha0 <- theta[1]
      alpha1 <- theta[2]
    }
  )
}

# LogisticLogNormal ----

## class ----

#' `LogisticLogNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticLogNormal`] is the class for the usual logistic regression model
#' with a bivariate normal prior on the intercept and log slope.
#'
#' @details The covariate is the natural logarithm of the dose \eqn{x} divided by
#'   the reference dose \eqn{x*}, i.e.:
#'   \deqn{logit[p(x)] = alpha0 + alpha1 * log(x/x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior \deqn{(alpha0, log(alpha1)) ~ Normal(mean, cov).}
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormal`], [`LogisticLogNormalSub`],
#'   [`ProbitLogNormal`], [`ProbitLogNormalRel`], [`LogisticLogNormalMixture`]
#'
#' @aliases LogisticLogNormal
#' @export
#'
.LogisticLogNormal <- setClass(
  Class = "LogisticLogNormal",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname LogisticLogNormal-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-LogisticLogNormal.R
#'
LogisticLogNormal <- function(mean, cov, ref_dose = 0) {
  model_ln <- ModelLogNormal(mean = mean, cov = cov, ref_dose = ref_dose)

  .LogisticLogNormal(
    model_ln,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    }
  )
}

# LogisticLogNormalSub ----

## class ----

#' `LogisticLogNormalSub`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticLogNormalSub`] is the class for a standard logistic model with
#' bivariate (log) normal prior with subtractive dose standardization.
#'
#' @details The covariate is the dose \eqn{x} minus the reference dose \eqn{x*},
#'   i.e.:
#'   \deqn{logit[p(x)] = alpha0 + alpha1 * (x - x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior \deqn{(alpha0, log(alpha1)) ~ Normal(mean, cov).}
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormal`], [`LogisticLogNormal`],
#'   [`ProbitLogNormal`], [`ProbitLogNormalRel`].
#'
#' @aliases LogisticLogNormalSub
#' @export
#'
.LogisticLogNormalSub <- setClass(
  Class = "LogisticLogNormalSub",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname LogisticLogNormalSub-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-LogisticLogNormalSub.R
#'
LogisticLogNormalSub <- function(mean, cov, ref_dose = 0) {
  model_ln <- ModelLogNormal(mean = mean, cov = cov, ref_dose = ref_dose)

  .LogisticLogNormalSub(
    model_ln,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * (x[i] - ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    init = function() {
      list(theta = c(0, -20))
    }
  )
}

# ProbitLogNormal ----

## class ----

#' `ProbitLogNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ProbitLogNormal`] is the class for probit regression model with a
#' bivariate normal prior on the intercept and log slope.
#'
#' @details The covariate is the natural logarithm of dose \eqn{x} divided by a
#'   reference dose \eqn{x*}, i.e.:
#'   \deqn{probit[p(x)] = alpha0 + alpha1 * log(x/x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior \deqn{(alpha0, log(alpha1)) ~ Normal(mean, cov).}
#'
#' @note This model is also used in the [`DualEndpoint`] classes, so this class
#'   can be used to check the prior assumptions on the dose-toxicity model, even
#'   when sampling from the prior distribution of the dual endpoint model is not
#'   possible.
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormal`], [`LogisticLogNormal`],
#'   [`LogisticLogNormalSub`], [`ProbitLogNormalRel`].
#'
#' @aliases ProbitLogNormalLogDose
#' @export
#'
.ProbitLogNormal <- setClass(
  Class = "ProbitLogNormal",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname ProbitLogNormal-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-ProbitLogNormal.R
#'
ProbitLogNormal <- function(mean, cov, ref_dose = 0) {
  model_ln <- ModelLogNormal(mean = mean, cov = cov, ref_dose = ref_dose)

  .ProbitLogNormal(
    model_ln,
    datamodel = function() {
      for (i in 1:nObs) {
        probit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    }
  )
}

# ProbitLogNormalRel ----

## class ----

#' `ProbitLogNormalRel`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ProbitLogNormalRel`] is the class for probit regression model with a bivariate
#' normal prior on the intercept and log slope.
#'
#' @details The covariate is the dose \eqn{x} divided by a reference dose \eqn{x*},
#'   i.e.:
#'   \deqn{probit[p(x)] = alpha0 + alpha1 * x/x*,}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior \deqn{(alpha0, log(alpha1)) ~ Normal(mean, cov).}
#'
#' @note This model is also used in the [`DualEndpoint`] classes, so this class
#'   can be used to check the prior assumptions on the dose-toxicity model, even
#'   when sampling from the prior distribution of the dual endpoint model is not
#'   possible.
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormal`], [`LogisticLogNormal`],
#'   [`LogisticLogNormalSub`], [`ProbitLogNormal`].
#'
#' @aliases ProbitLogNormalRel
#' @export
#'
.ProbitLogNormalRel <- setClass(
  Class = "ProbitLogNormalRel",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname ProbitLogNormalRel-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-ProbitLogNormalRel.R
#'
ProbitLogNormalRel <- function(mean, cov, ref_dose = 0) {
  model_ln <- ModelLogNormal(mean = mean, cov = cov, ref_dose = ref_dose)

  .ProbitLogNormalRel(
    model_ln,
    datamodel = function() {
      for (i in 1:nObs) {
        probit(p[i]) <- alpha0 + alpha1 * (x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    }
  )
}

# LogisticKadane ----

## class ----

#' `LogisticKadane`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticKadane`] is the class for the logistic model in the parametrization
#' of Kadane et al. (1980).
#'
#' @details Let `rho0 = p(xmin)` be the probability of a DLT at the minimum dose
#'   `xmin`, and let `gamma` be the dose with target toxicity probability `theta`,
#'   i.e. \eqn{p(gamma) = theta}. Then it can easily be shown that the logistic
#'   regression model has intercept
#'   \deqn{[gamma * logit(rho0) - xmin * logit(theta)] / [gamma - xmin]}
#'   and slope
#'   \deqn{[logit(theta) - logit(rho0)] / [gamma - xmin].}
#'
#'   The priors are \deqn{gamma ~ Unif(xmin, xmax).} and
#'   \deqn{rho0 ~ Unif(0, theta).}
#'
#' @note The slots of this class, required for creating the model, are the target
#'   toxicity, as well as the minimum and maximum of the dose range. Note that
#'   these can be different from the minimum and maximum of the dose grid in the
#'   data later on.
#'
#' @slot theta (`proportion`)\cr the target toxicity probability.
#' @slot xmin (`number`)\cr the minimum of the dose range.
#' @slot xmax (`number`)\cr the maximum of the dose range.
#'
#' @seealso [`ModelLogNormal`]
#'
#' @aliases LogisticKadane
#' @export
#'
.LogisticKadane <- setClass(
  Class = "LogisticKadane",
  contains = "GeneralModel",
  slots = c(
    theta = "numeric",
    xmin = "numeric",
    xmax = "numeric"
  ),
  prototype = prototype(
    theta = 0.3,
    xmin = 0.1,
    xmax = 1
  ),
  validity = v_model_logistic_kadane
)

## constructor ----

#' @rdname LogisticKadane-class
#'
#' @param theta (`proportion`)\cr the target toxicity probability.
#' @param xmin (`number`)\cr the minimum of the dose range.
#' @param xmax (`number`)\cr the maximum of the dose range.
#'
#' @export
#' @example examples/Model-class-LogisticKadane.R
#'
LogisticKadane <- function(theta, xmin, xmax) {
  .LogisticKadane(
    theta = theta,
    xmin = xmin,
    xmax = xmax,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- (1 / (gamma - xmin)) *
          (gamma * logit(rho0) - xmin * logit(theta) + x[i] * (logit(theta) - logit(rho0)))
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      rho0 ~ dunif(0, theta)
      gamma ~ dunif(xmin, xmax)
    },
    modelspecs = function() {
      list(theta = theta, xmin = xmin, xmax = xmax)
    },
    init = function() {
      list(rho0 = theta / 10, gamma = (xmax - xmin) / 2)
    },
    sample = c("rho0", "gamma"),
    datanames = c("nObs", "y", "x")
  )
}

# LogisticKadaneBetaGamma ----

## class ----

#' `LogisticKadaneBetaGamma`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`LogisticKadaneBetaGamma`] is the class for the logistic model in the parametrization
#' of Kadane et al. (1980), using a beta and a gamma distribution as the model priors.
#'
#' @details Let `rho0 = p(xmin)` be the probability of a DLT at the minimum dose
#'   `xmin`, and let `gamma` be the dose with target toxicity probability `theta`,
#'   i.e. \eqn{p(gamma) = theta}. Then it can easily be shown that the logistic
#'   regression model has intercept
#'   \deqn{[gamma * logit(rho0) - xmin * logit(theta)] / [gamma - xmin]}
#'   and slope
#'   \deqn{[logit(theta) - logit(rho0)] / [gamma - xmin].}
#'
#'   The prior for `gamma`, is \deqn{gamma ~ Gamma(shape, rate).}.
#'   The prior for `rho0 = p(xmin)`, is \deqn{rho0 ~ Beta(alpha, beta).}
#'
#' @note The slots of this class, required for creating the model, are the same
#'    as in the `LogisticKadane` class. In addition, the shape parameters of the
#'    Beta prior distribution of `rho0` and the shape and rate parameters of the
#'    Gamma prior distribution of `gamma`, are required for creating the prior model.
#'
#' @slot theta (`proportion`)\cr the target toxicity probability.
#' @slot xmin (`number`)\cr the minimum of the dose range.
#' @slot xmax (`number`)\cr the maximum of the dose range.
#' @slot alpha (`number`)\cr the first shape parameter of the Beta prior distribution
#' of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @slot beta (`number`)\cr the second shape parameter of the Beta prior distribution
#' of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @slot shape (`number`)\cr the shape parameter of the Gamma prior distribution
#' of `gamma` the dose with target toxicity probability `theta`.
#' @slot rate (`number`)\cr the rate parameter of the Gamma prior distribution
#' of `gamma` the dose with target toxicity probability `theta`.
#'
#' @seealso [`ModelLogNormal`], [`LogisticKadane`].
#'
#' @aliases LogisticKadaneBetaGamma
#' @export
#'
.LogisticKadaneBetaGamma <- setClass(
  Class = "LogisticKadaneBetaGamma",
  contains = "LogisticKadane",
  slots = c(
    alpha = "numeric",
    beta = "numeric",
    shape = "numeric",
    rate = "numeric"
  ),
  prototype = prototype(
    theta = 0.3,
    xmin = 0.1,
    xmax = 1,
    alpha = 1,
    beta = 0.5,
    shape = 1.2,
    rate = 2.5
  ),
  validity = v_model_logistic_kadane_beta_gamma
)

## constructor ----

#' @rdname LogisticKadaneBetaGamma-class
#'
#' @inheritParams LogisticKadane
#'
#' @param alpha (`number`)\cr the first shape parameter of the Beta prior distribution
#' of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @param beta (`number`)\cr the second shape parameter of the Beta prior distribution
#' of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @param shape (`number`)\cr the shape parameter of the Gamma prior distribution
#' of `gamma` the dose with target toxicity probability `theta`.
#' @param rate (`number`)\cr the rate parameter of the Gamma prior distribution
#' of `gamma` the dose with target toxicity probability `theta`.
#'
#' @export
#' @example examples/Model-class-LogisticKadaneBetaGamma.R
#'
LogisticKadaneBetaGamma <- function(theta, xmin, xmax, alpha, beta, shape, rate) {
  model_lk <- LogisticKadane(theta = theta, xmin = xmin, xmax = xmax)
  .LogisticKadaneBetaGamma(
    model_lk,
    alpha = alpha,
    beta = beta,
    shape = shape,
    rate = rate,
    priormodel = function() {
      rho0 ~ dbeta(alpha, beta)
      gamma ~ dgamma(shape, rate)
      lowestdose <- xmin
      highestdose <- xmax
      DLTtarget <- theta
    },
    modelspecs = function() {
      list(
        theta = theta,
        xmin = xmin,
        xmax = xmax,
        alpha = alpha,
        beta = beta,
        shape = shape,
        rate = rate
      )
    }
  )
}

# LogisticNormalMixture ----

## class ----

#' `LogisticNormalMixture`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticNormalMixture`] is the class for standard logistic regression model
#' with a mixture of two bivariate normal priors on the intercept and slope parameters.
#'
#' @details The covariate is the natural logarithm of the dose \eqn{x} divided by
#'   the reference dose \eqn{x*}, i.e.:
#'   \deqn{logit[p(x)] = alpha0 + alpha1 * log(x/x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior
#'   \deqn{(alpha0, alpha1) ~ w * Normal(mean1, cov1) + (1 - w) * Normal(mean2, cov2).}
#'   The weight w for the first component is assigned a beta prior `B(a, b)`.
#'
#' @note The weight of the two normal priors is a model parameter, hence it is a
#'   flexible mixture. This type of prior is often used with a mixture of a minimal
#'   informative and an informative component, in order to make the CRM more robust
#'   to data deviations from the informative component.
#'
#' @slot comp1 (`ModelParamsNormal`)\cr bivariate normal prior specification of
#'   the first component.
#' @slot comp2 (`ModelParamsNormal`)\cr bivariate normal prior specification of
#'   the second component.
#' @slot weightpar (`numeric`)\cr the beta parameters for the weight of the
#'   first component. It must a be a named vector of length 2 with names `a` and
#'   `b` and with strictly positive values.
#' @slot ref_dose (`number`)\cr the reference dose.
#'
#' @seealso [`ModelParamsNormal`], [`ModelLogNormal`],
#'   [`LogisticNormalFixedMixture`], [`LogisticLogNormalMixture`].
#'
#' @aliases LogisticNormalMixture
#' @export
#'
.LogisticNormalMixture <- setClass(
  Class = "LogisticNormalMixture",
  contains = "GeneralModel",
  slots = c(
    comp1 = "ModelParamsNormal",
    comp2 = "ModelParamsNormal",
    weightpar = "numeric",
    ref_dose = "numeric"
  ),
  prototype = prototype(
    comp1 = ModelParamsNormal(mean = c(0, 1), cov = diag(2)),
    comp2 = ModelParamsNormal(mean = c(-1, 1), cov = diag(2)),
    weightpar = c(a = 1, b = 1),
    ref_dose = 1
  ),
  validity = v_model_logistic_normal_mix
)

## constructor ----

#' @rdname LogisticNormalMixture-class
#'
#' @param comp1 (`ModelParamsNormal`)\cr bivariate normal prior specification of
#'   the first component. See [`ModelParamsNormal`] for more details.
#' @param comp2 (`ModelParamsNormal`)\cr bivariate normal prior specification of
#'   the second component. See [`ModelParamsNormal`] for more details.
#' @param weightpar (`numeric`)\cr the beta parameters for the weight of the
#'   first component. It must a be a named vector of length 2 with names `a` and
#'   `b` and with strictly positive values.
#' @param ref_dose (`number`)\cr the reference dose.
#'
#' @export
#' @example examples/Model-class-LogisticNormalMixture.R
#'
LogisticNormalMixture <- function(comp1,
                                  comp2,
                                  weightpar,
                                  ref_dose) {
  .LogisticNormalMixture(
    comp1 = comp1,
    comp2 = comp2,
    weightpar = weightpar,
    ref_dose = ref_dose,
    datamodel = function() {
      # The logistic likelihood - the same as for non-mixture case.
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      w ~ dbeta(weightpar[1], weightpar[2])
      wc <- 1 - w
      comp0 ~ dbern(wc)
      comp <- comp0 + 1
      # Conditional on the component index "comp", which is  1 or 2.
      # comp = 1 with probability "w" and comp = 2 with probability "1 - w".
      theta ~ dmnorm(mean[1:2, comp], prec[1:2, 1:2, comp])
      alpha0 <- theta[1]
      alpha1 <- theta[2]
    },
    modelspecs = function() {
      list(
        ref_dose = ref_dose,
        mean = cbind(comp1@mean, comp2@mean),
        prec = array(data = c(comp1@prec, comp2@prec), dim = c(2, 2, 2)),
        weightpar = weightpar
      )
    },
    init = function() {
      list(theta = c(0, 1))
    },
    sample = c("alpha0", "alpha1", "w"),
    datanames = c("nObs", "y", "x")
  )
}

# LogisticNormalFixedMixture ----

## class ----

#' `LogisticNormalFixedMixture`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticNormalFixedMixture`] is the class for standard logistic regression
#' model with fixed mixture of multiple bivariate (log) normal priors on the
#' intercept and slope parameters. The weights of the normal priors are fixed,
#' hence no additional model parameters are introduced. This type of prior is
#' often used to better approximate a given posterior distribution, or when the
#' information is given in terms of a mixture.
#'
#' @details The covariate is the natural logarithm of the dose \eqn{x} divided
#'   by the reference dose \eqn{x*}, i.e.:
#'   \deqn{logit[p(x)] = alpha0 + alpha1 * log(x/x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x}.
#'   The prior
#'   \deqn{(alpha0, alpha1) ~ w1 * Normal(mean1, cov1) + ... + wK * Normal(meanK, covK),}
#'   if a normal prior is used and
#'   \deqn{(alpha0, log(alpha1)) ~ w1 * Normal(mean1, cov1) + ... + wK * Normal(meanK, covK),}
#'   if a log normal prior is used.
#'   The weights \eqn{w1, ..., wK} of the components are fixed and sum to 1.
#'
#'   The slots of this class comprise a list with components parameters. Every
#'   single component contains the mean vector and the covariance matrix of
#'   bivariate normal distributions. Remaining slots are the weights of the
#'   components as well as the reference dose. Moreover, a special indicator
#'   slot specifies whether a log normal prior is used.
#'
#' @slot components (`list`)\cr the specifications of the mixture components,
#'   a list with [`ModelParamsNormal`] objects for each bivariate (log) normal
#'   prior.
#' @slot weights (`numeric`)\cr the weights of the components; these must be
#'   positive and must sum to 1.
#' @slot ref_dose (`number`)\cr the reference dose.
#' @slot log_normal (`flag`)\cr should a log normal prior be used, such
#'   that the mean vectors and covariance matrices are valid for the intercept
#'   and log slope?
#'
#' @seealso [`ModelParamsNormal`], [`ModelLogNormal`],
#'   [`LogisticNormalMixture`], [`LogisticLogNormalMixture`].
#'
#' @aliases LogisticNormalFixedMixture
#' @export
#'
.LogisticNormalFixedMixture <- setClass(
  Class = "LogisticNormalFixedMixture",
  contains = "GeneralModel",
  slots = c(
    components = "list",
    weights = "numeric",
    ref_dose = "numeric",
    log_normal = "logical"
  ),
  prototype = prototype(
    components = list(
      comp1 = ModelParamsNormal(mean = c(0, 1), cov = diag(2)),
      comp2 = ModelParamsNormal(mean = c(-1, 1), cov = diag(2))
    ),
    weights = c(0.5, 0.5),
    ref_dose = 1,
    log_normal = TRUE
  ),
  validity = v_model_logistic_normal_fixed_mix
)

## constructor ----

#' @rdname LogisticNormalFixedMixture-class
#'
#' @param components (`list`)\cr the specifications of the mixture components,
#'   a list with [`ModelParamsNormal`] objects for each bivariate (log) normal
#'   prior.
#' @param weights (`numeric`)\cr the weights of the components; these must be
#'   positive and will be normalized to sum to 1.
#' @param ref_dose (`number`)\cr the reference dose.
#' @param log_normal (`flag`)\cr should a log normal prior be specified, such
#'   that the mean vectors and covariance matrices are valid for the intercept
#'   and log slope?
#'
#' @export
#' @example examples/Model-class-LogisticNormalFixedMixture.R
#'
LogisticNormalFixedMixture <- function(components,
                                       weights,
                                       ref_dose,
                                       log_normal = FALSE) {
  # Normalize weights to sum to 1.
  weights <- weights / sum(weights)

  .LogisticNormalFixedMixture(
    components = components,
    weights = weights,
    ref_dose = ref_dose,
    log_normal = log_normal,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = if (log_normal) {
      function() {
        comp ~ dcat(weights)
        theta ~ dmnorm(mean[1:2, comp], prec[1:2, 1:2, comp])
        alpha0 <- theta[1]
        alpha1 <- exp(theta[2])
      }
    } else {
      function() {
        comp ~ dcat(weights)
        theta ~ dmnorm(mean[1:2, comp], prec[1:2, 1:2, comp])
        alpha0 <- theta[1]
        alpha1 <- theta[2]
      }
    },
    modelspecs = function() {
      list(
        ref_dose = ref_dose,
        weights = weights,
        mean = do.call(cbind, lapply(components, h_slots, "mean", simplify = TRUE)),
        prec = array(
          do.call(c, lapply(components, h_slots, "prec", simplify = TRUE)),
          dim = c(2, 2, length(components))
        )
      )
    },
    init = function() {
      list(theta = c(0, 1))
    },
    sample = c("alpha0", "alpha1"),
    datanames = c("nObs", "y", "x")
  )
}

# LogisticLogNormalMixture ----

## class ----

#' `LogisticLogNormalMixture`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticLogNormalMixture`] is the class for standard logistic model with
#' online mixture of two bivariate log normal priors.
#'
#' @details This model can be used when data is arising online from the informative
#'   component of the prior, at the same time with the data of the trial of
#'   main interest. Formally, this is achieved by assuming that the probability
#'   of a DLT at dose \eqn{x} is given by
#'   \deqn{p(x) = \pi * p1(x) + (1 - \pi) * p2(x)}
#'   where \eqn{\pi} is the probability for the model \eqn{p(x)} being the same
#'   as the model \eqn{p1(x)}, which is the informative component of the prior.
#'   From this model data arises in parallel: at doses `xshare`, DLT information
#'   `yshare` is observed, in total `nObsshare` data points (see [`DataMixture`]).
#'   On the other hand, \eqn{1 - \pi}, is the probability of a separate model
#'   \eqn{p2(x)}. Both components have the same log normal prior distribution,
#'   which can be specified by the user, and which is inherited from the
#'   [`LogisticLogNormal`] class.
#'
#' @slot share_weight (`proportion`)\cr the prior weight for the share component
#'   \eqn{p_{1}(x)}.
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormalMixture`],
#'   [`LogisticNormalFixedMixture`].
#'
#' @aliases LogisticLogNormalMixture
#' @export
#'
.LogisticLogNormalMixture <- setClass(
  Class = "LogisticLogNormalMixture",
  contains = "LogisticLogNormal",
  slots = c(
    share_weight = "numeric"
  ),
  prototype = prototype(
    share_weight = 0.1
  ),
  validity = v_model_logistic_log_normal_mix
)

## constructor ----

#' @rdname LogisticLogNormalMixture-class
#'
#' @inheritParams ModelLogNormal
#' @param share_weight (`proportion`)\cr the prior weight for the share component.
#'
#' @export
#' @example examples/Model-class-LogisticLogNormalMixture.R
#'
LogisticLogNormalMixture <- function(mean,
                                     cov,
                                     ref_dose,
                                     share_weight) {
  params <- ModelParamsNormal(mean, cov)
  .LogisticLogNormalMixture(
    params = params,
    ref_dose = ref_dose,
    share_weight = share_weight,
    datamodel = function() {
      for (i in 1:nObs) {
        # comp gives the component: non-informative (1) or share (2) the two components.
        stand_log_dose[i] <- log(x[i] / ref_dose)
        logit(p[i]) <- alpha0[comp] + alpha1[comp] * stand_log_dose[i]
        y[i] ~ dbern(p[i])
      }
      for (j in 1:nObsshare) {
        stand_log_dose_share[j] <- log(xshare[j] / ref_dose)
        logit(pshare[j]) <- alpha0[2] + alpha1[2] * stand_log_dose_share[j]
        yshare[j] ~ dbern(pshare[j])
      }
    },
    priormodel = function() {
      for (k in 1:2) {
        theta[k, 1:2] ~ dmnorm(mean, prec)
        alpha0[k] <- theta[k, 1]
        alpha1[k] <- exp(theta[k, 2])
      }
      # The component indicator.
      comp ~ dcat(cat_probs)
    },
    modelspecs = function() {
      list(
        ref_dose = ref_dose,
        cat_probs = c(1 - share_weight, share_weight),
        mean = params@mean,
        prec = params@prec
      )
    },
    init = function() {
      list(theta = matrix(c(0, 0, 1, 1), nrow = 2))
    },
    sample = c("alpha0", "alpha1", "comp"),
    datanames = c("nObs", "y", "x", "nObsshare", "yshare", "xshare")
  )
}

# DualEndpoint ----

## class ----

#' `DualEndpoint`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DualEndpoint`] is the general class for the dual endpoint model.
#'
#' @details The idea of the dual-endpoint models is to model not only the
#'   dose-toxicity relationship, but also to model, at the same time, the
#'   relationship of a PD biomarker with the dose. The sub-classes of this class
#'   define how the dose-biomarker relationship is parametrized. This class here
#'   shall contain all the common features to reduce duplicate code.
#'   (This class however, must not be virtual as we need to create objects
#'   of it during the construction of subclass objects.)
#'
#'   The dose-toxicity relationship is modeled with probit regression model
#'   \deqn{probit[p(x)] = betaZ1 + betaZ2 * x/x*,}
#'   or
#'   \deqn{probit[p(x)] = betaZ1 + betaZ2 * log(x/x*),}
#'   in case when the option `use_log_dose` is `TRUE`.
#'   Here, \eqn{p(x)} is the probability of observing a DLT for a given
#'   dose \eqn{x} and \eqn{x*} is the reference dose.
#'   The prior \deqn{(betaZ1, log(betaZ2)) ~ Normal(mean, cov).}
#'
#'   For the biomarker response \eqn{w} at a dose \eqn{x}, we assume
#'   \deqn{w(x) ~ Normal(f(x), sigma2W),}
#'   where \eqn{f(x)} is a function of the dose \eqn{x}, which is further
#'   specified in sub-classes. The biomarker variance \eqn{sigma2W} can be fixed
#'   or assigned an Inverse-Gamma prior distribution; see the details below under
#'   slot `sigma2W`.
#'
#'   Finally, the two endpoints \eqn{y} (the binary DLT variable) and \eqn{w}
#'   (the biomarker) can be correlated, by assuming a correlation of level
#'   \eqn{rho} between the underlying continuous latent toxicity variable \eqn{z}
#'   and the biomarker \eqn{w}. Again, this correlation can be fixed or assigned
#'   a prior distribution from the scaled Beta family; see the details below
#'   under slot `rho`.
#'
#'   Please see the example vignette by typing `crmPackExample()` for a full example.
#'
#' @slot betaZ_params (`ModelParamsNormal`)\cr for the probit toxicity model, it
#'   contains the prior mean, covariance matrix and precision matrix which is
#'   internally calculated as an inverse of the covariance matrix.
#' @slot ref_dose (`number`)\cr for the probit toxicity model,, the reference dose.
#' @slot use_log_dose (`flag`)\cr for the probit toxicity model, whether a log
#'   transformation of the (standardized) dose should be used?
#' @slot sigma2W (`numeric`)\cr the biomarker variance. Either a fixed value or
#'   Inverse-Gamma distribution parameters, i.e. vector with two elements named
#'   `a` and `b`.
#' @slot rho (`numeric`)\cr either a fixed value for the correlation
#'   (between `-1` and `1`), or a named vector with two elements named `a` and `b`
#'   for the Beta prior on the transformation `kappa = (rho + 1) / 2`, which is
#'   in `(0, 1)`. For example, `a = 1, b = 1` leads to a uniform prior on `rho`.
#' @slot use_fixed rho (`logical`)\cr indicates whether a fixed value for
#'   `sigma2W` and `rho` (for each parameter separately) is used or not. This
#'   slot is needed for internal purposes and must not be touched by the user.
#'
#' @seealso [`DualEndpointRW`], [`DualEndpointBeta`], [`DualEndpointEmax`].
#'
#' @aliases DualEndpoint
#' @export
#'
.DualEndpoint <- setClass(
  Class = "DualEndpoint",
  slots = c(
    betaZ_params = "ModelParamsNormal",
    ref_dose = "numeric",
    use_log_dose = "logical",
    sigma2W = "numeric",
    rho = "numeric",
    use_fixed = "logical"
  ),
  prototype = prototype(
    betaZ_params = ModelParamsNormal(
      mean = c(0, 1),
      cov = diag(2)
    ),
    ref_dose = 1,
    use_log_dose = FALSE,
    sigma2W = 1,
    rho = 0,
    use_fixed = c(
      sigma2W = TRUE,
      rho = TRUE
    )
  ),
  contains = "Model",
  validity = v_model_dual_endpoint
)

## constructor ----

#' @rdname DualEndpoint-class
#'
#' @param mean (`numeric`)\cr for the probit toxicity model, the prior mean vector.
#' @param cov (`matrix`)\cr for the probit toxicity model, the prior covariance
#'   matrix. The precision matrix is internally calculated as an inverse of `cov`.
#' @param ref_dose (`number`)\cr for the probit toxicity model, the reference dose.
#' @param use_log_dose (`flag`)\cr for the probit toxicity model, whether a log
#'   transformation of the (standardized) dose should be used?
#' @param sigma2W (`numeric`)\cr the biomarker variance. Either a fixed value or
#'   Inverse-Gamma distribution parameters, i.e. vector with two elements named
#'   `a` and `b`.
#' @param rho (`numeric`)\cr either a fixed value for the correlation
#'   (between `-1` and `1`), or a named vector with two elements named `a` and `b`
#'   for the Beta prior on the transformation `kappa = (rho + 1) / 2`, which is
#'   in `(0, 1)`. For example, `a = 1, b = 1` leads to a uniform prior on `rho`.
#'
#' @export
#'
DualEndpoint <- function(mean,
                         cov,
                         ref_dose = 1,
                         use_log_dose = FALSE,
                         sigma2W,
                         rho) {
  use_fixed <- c(sigma2W = is.scalar(sigma2W), rho = is.scalar(rho))
  betaZ_params <- ModelParamsNormal(mean, cov)

  datamodel <- function() {
    for (i in 1:nObs) {
      # The toxicity model.
      stand_dose_temp[i] <- x[i] / ref_dose
      stand_dose[i] <- ifelse(use_log_dose, log(stand_dose_temp[i]), stand_dose_temp[i])
      meanZ[i] <- betaZ[1] + betaZ[2] * stand_dose[i]
      z[i] ~ dnorm(meanZ[i], 1)
      y[i] ~ dinterval(z[i], 0)

      # The conditional biomarker model. betaW defined in subclasses!
      condMeanW[i] <- betaW[xLevel[i]] + rho / sqrt(precW) * (z[i] - meanZ[i])
      w[i] ~ dnorm(condMeanW[i], condPrecW)
    }
  }
  priormodel <- function() {
    # Priors for betaW defined in subclasses!
    theta ~ dmnorm(betaZ_mean, betaZ_prec)
    betaZ[1] <- theta[1]
    betaZ[2] <- exp(theta[2])
    # Conditional precision for biomarker.
    condPrecW <- precW / (1 - pow(rho, 2))
  }
  modelspecs <- list(
    use_log_dose = use_log_dose,
    ref_dose = ref_dose,
    betaZ_mean = betaZ_params@mean,
    betaZ_prec = betaZ_params@prec
  )
  init <- NULL
  sample <- "betaZ"

  comp <- list(
    priormodel = priormodel, modelspecs = modelspecs, init = init, sample = sample
  )
  # Update model components with regard to biomarker regression variance.
  comp <- h_model_dual_endpoint_sigma2W(
    use_fixed["sigma2W"],
    sigma2W = sigma2W, comp = comp
  )

  # Update model components with regard to DLT and biomarker correlation.
  comp <- h_model_dual_endpoint_rho(
    use_fixed["rho"],
    rho = rho, comp = comp
  )

  .DualEndpoint(
    betaZ_params = betaZ_params,
    ref_dose = ref_dose,
    use_log_dose = use_log_dose,
    sigma2W = sigma2W,
    rho = rho,
    use_fixed = use_fixed,
    datamodel = datamodel,
    priormodel = comp$priormodel,
    modelspecs = function() {
      comp$modelspecs
    },
    init = function(y) {
      c(comp$init, list(z = ifelse(y == 0, -1, 1), theta = c(0, 1)))
    },
    sample = comp$sample,
    datanames = c("nObs", "w", "x", "xLevel", "y", "nGrid")
  )
}

# DualEndpointRW ----

## class ----

#' `DualEndpointRW`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DualEndpointRW`] is the class for the dual endpoint model with random walk
#' prior for biomarker.
#'
#' @details This class extends the [`DualEndpoint`] class. Here, the dose-biomarker
#'   relationship \eqn{f(x)} is modelled by a non-parametric random walk of first
#'   or second order. That means, for the first order random walk we assume
#'   \deqn{betaW_i - betaW_i-1 ~ Normal(0, (x_i - x_i-1) ⋅ sigma2betaW),}
#'   where \eqn{betaW_i = f(x_i)} is the biomarker mean at the \eqn{i}-th dose
#'   gridpoint \eqn{x_i}.
#'   For the second order random walk, the second-order differences instead of
#'   the first-order differences of the biomarker means follow the normal distribution
#'   with \eqn{0} mean and \eqn{2 ⋅ (x_i - x_i-2) ⋅ sigma2betaW} variance.
#'
#'   The variance parameter \eqn{sigma2betaW} is important because it steers the
#'   smoothness of the function \eqn{f(x)}, i.e.: if it is large, then \eqn{f(x)}
#'   will be very wiggly; if it is small, then \eqn{f(x)} will be smooth.
#'   This parameter can either be a fixed value or assigned an inverse gamma prior
#'   distribution.
#'
#' @note Non-equidistant dose grids can be used now, because the difference
#'   \eqn{x_i - x_i-1} is included in the modelling assumption above.
#'   Please note that due to impropriety of the random walk prior distributions,
#'   it is not possible to produce MCMC samples with empty data objects (i.e.,
#'   sample from the prior). This is not a bug, but a theoretical feature of this
#'   model.
#'
#' @slot sigma2betaW (`numeric`)\cr the prior variance factor of the random walk
#'   prior for the biomarker model. Either a fixed value or Inverse-Gamma distribution
#'   parameters, i.e. vector with two elements named `a` and `b`.
#' @slot rw1 rho (`flag`)\cr for specifying the random walk prior on the biomarker
#'   level. When `TRUE`, random walk of first order is used. Otherwise, the
#'   random walk of second order is used.
#'
#' @seealso [`DualEndpoint`], [`DualEndpointBeta`], [`DualEndpointEmax`].
#'
#' @aliases DualEndpointRW
#' @export
#'
.DualEndpointRW <- setClass(
  Class = "DualEndpointRW",
  slots = c(
    sigma2betaW = "numeric",
    rw1 = "logical"
  ),
  prototype = prototype(
    sigma2betaW = 1,
    rw1 = TRUE,
    use_fixed = c(
      sigma2W = TRUE,
      rho = TRUE,
      sigma2betaW = TRUE
    )
  ),
  contains = "DualEndpoint",
  validity = v_model_dual_endpoint_rw
)

## constructor ----

#' @rdname DualEndpointRW-class
#'
#' @param sigma2betaW (`numeric`)\cr the prior variance factor of the random walk
#'   prior for the biomarker model. Either a fixed value or Inverse-Gamma distribution
#'   parameters, i.e. vector with two elements named `a` and `b`.
#' @param rw1 (`flag`)\cr for specifying the random walk prior on the biomarker
#'   level. When `TRUE`, random walk of first order is used. Otherwise, the
#'   random walk of second order is used.
#' @param ... parameters passed to [DualEndpoint()].
#'
#' @export
#' @example examples/Model-class-DualEndpointRW.R
#'
DualEndpointRW <- function(sigma2betaW,
                           rw1 = TRUE,
                           ...) {
  start <- DualEndpoint(...)
  start@use_fixed["sigma2betaW"] <- length(sigma2betaW) == 1L

  start@priormodel <- if (rw1) {
    h_jags_join_models(
      start@priormodel,
      function() {
        # The 1st order differences.
        # Essentially dflat(), which is not available in JAGS.
        betaW[1] ~ dnorm(0, 0.000001)
        for (i in 2:nGrid) {
          delta[i - 1] ~ dnorm(0, precBetaW / (doseGrid[i] - doseGrid[i - 1]))
          betaW[i] <- betaW[i - 1] + delta[i - 1]
        }
      }
    )
  } else {
    h_jags_join_models(
      start@priormodel,
      function() {
        # The 2nd order differences.
        delta[1] ~ dnorm(0, 0.000001)
        betaW[1] ~ dnorm(0, 0.000001)
        betaW[2] <- betaW[1] + delta[1]
        for (i in 3:nGrid) {
          # delta2: differences of the differences of betaW follow normal dist.
          delta2[i - 2] ~ dnorm(0, 2 * precBetaW / (doseGrid[i] - doseGrid[i - 2]))
          delta[i - 1] <- delta[i - 2] + delta2[i - 2]
          betaW[i] <- betaW[i - 1] + delta[i - 1]
        }
      }
    )
  }

  start@sample <- c(start@sample, "betaW", "delta")
  # We need the doseGrid in the BUGS model.
  start@datanames <- c(start@datanames, "doseGrid")

  # Update model components with regard to biomarker regression variance.
  start <- h_model_dual_endpoint_sigma2betaW(
    start@use_fixed["sigma2betaW"],
    sigma2betaW = sigma2betaW,
    de = start
  )

  .DualEndpointRW(
    start,
    sigma2betaW = sigma2betaW,
    rw1 = rw1
  )
}

# nolint start

# DualEndpointBeta ----

##' Dual endpoint model with beta function for dose-biomarker relationship
##'
##' This class extends the \code{\linkS4class{DualEndpoint}} class. Here the
##' dose-biomarker relationship \eqn{f(x)} is modelled by a parametric, rescaled
##' beta density function:
##'
##' \deqn{f(x) = E_{0} + (E_{max} - E_{0}) * Beta(\delta_{1}, \delta_{2}) *
##'                   (x/x^{*})^{\delta_{1}} * (1 - x/x^{*})^{\delta_{2}}}
##'
##' where \eqn{x^{*}} is the maximum dose (end of the dose range to be
##' considered), \eqn{\delta_{1}} and \eqn{\delta_{2}} are the two beta
##' parameters, and \eqn{E_{0}} and \eqn{E_{max}} are the minimum and maximum
##' levels, respectively. For ease of interpretation, we parametrize with
##' \eqn{\delta_{1}} and the mode of the curve instead, where
##' \deqn{mode = \delta_{1} / (\delta_{1} + \delta_{2}),}
##' and multiplying this with \eqn{x^{*}} gives the mode on the dose grid.
##'
##' All parameters can currently be assigned uniform distributions or be fixed
##' in advance. Note that \code{E0} and \code{Emax} can have negative values or uniform
##' distributions reaching into negative range, while \code{delta1} and \code{mode}
##' must be positive or have uniform distributions in the positive range.
##'
##' @slot E0 either a fixed number or the two uniform distribution parameters
##' @slot Emax either a fixed number or the two uniform distribution parameters
##' @slot delta1 either a fixed number or the two uniform distribution parameters
##' @slot mode either a fixed number or the two uniform distribution parameters
##' @slot refDoseBeta the reference dose \eqn{x^{*}} (note that this is different from
##' the \code{refDose} in the inherited \code{\linkS4class{DualEndpoint}} model)
##'
##' @example examples/Model-class-DualEndpointBeta.R
##' @export
##' @keywords classes
.DualEndpointBeta <-
    setClass("DualEndpointBeta",
             representation(E0="numeric",
                            Emax="numeric",
                            delta1="numeric",
                            mode="numeric",
                            refDoseBeta="numeric"),
             prototype(E0=c(0, 100),
                       Emax=c(0, 500),
                       delta1=c(0, 5),
                       mode=c(1, 15),
                       refDoseBeta=1000,
                       use_fixed=
                       c(sigma2W=TRUE,
                            rho=TRUE,
                            E0=FALSE,
                            Emax=FALSE,
                            delta1=FALSE,
                            mode=FALSE)),
             contains="DualEndpoint",
             validity=
                 function(object){
                     o <- Validate()

                     ## check delta1
                     if(object@use_fixed["delta1"])
                     {
                       o$check(object@delta1 > 0,
                               "delta1 must be positive")
                     } else {
                       o$check(all(object@delta1 >= 0) &&
                                 (diff(object@delta1) > 0),
                               "delta1 has not proper prior parameters")
                     }

                     ## check delta1 and mode
                     for(parName in c("delta1", "mode"))
                     {
                       ## if we use a fixed value for this parameter
                       if(object@use_fixed[parName])
                       {
                         ## check range of value
                         o$check(slot(object, parName) > 0,
                                 paste(parName, "must be positive"))
                       } else {
                         ## use a Uniform(a, b) prior
                         o$check(all(slot(object, parName) >= 0) &&
                                   (diff(slot(object, parName)) > 0),
                                 paste(parName,
                                       "has not proper prior parameters"))
                       }
                     }

                     ## check E0 and Emax
                     for(parName in c("E0", "Emax"))
                     {
                         ## if we don't use a fixed value for this parameter
                         if(! object@use_fixed[parName])
                         {
                             ## use a Uniform(a, b) prior
                             o$check(diff(slot(object, parName)) > 0,
                                     paste(parName,
                                           "has not proper prior parameters"))
                         }
                     }

                     ## check the refDoseBeta
                     o$check(object@refDoseBeta > 0,
                             "refDoseBeta must be positive")

                     o$result()
                 })

##' Initialization function for the "DualEndpointBeta" class
##'
##' @param E0 see \code{\linkS4class{DualEndpointBeta}}
##' @param Emax see \code{\linkS4class{DualEndpointBeta}}
##' @param delta1 see \code{\linkS4class{DualEndpointBeta}}
##' @param mode see \code{\linkS4class{DualEndpointBeta}}
##' @param refDoseBeta see \code{\linkS4class{DualEndpointBeta}}
##' @param \dots additional parameters, see \code{\linkS4class{DualEndpoint}}
##' @return the \code{\linkS4class{DualEndpointBeta}} object
##'
##' @export
##' @keywords methods
DualEndpointBeta <- function(E0,
                             Emax,
                             delta1,
                             mode,
                             refDoseBeta,
                             ...) {
    start <- DualEndpoint(...)

    ## we need the dose grid here in the BUGS model, therefore add it to datanames
    start@datanames <- c(start@datanames, "doseGrid")

    start@use_fixed <- c(
      start@use_fixed,
      sapply(list(E0 = E0, Emax = Emax, delta1 = delta1, mode = mode), is.scalar)
    )

    start@priormodel <-
        h_jags_join_models(start@priormodel,
                   function(){
                       ## delta2 <- delta1 * (1 - (mode/refDoseBeta)) / (mode/refDoseBeta)
                       delta2 <- delta1 * (refDoseBeta/mode - 1)
                       ## betafun <- (delta1 + delta2)^(delta1 + delta2) *
                       ##     delta1^(- delta1) * delta2^(- delta2)
                       betafun <- (1 + delta2 / delta1)^delta1 *
                           (delta1 / delta2 + 1)^delta2

                       for (j in 1:nGrid)
                       {
                           StandDoseBeta[j] <- doseGrid[j] / refDoseBeta
                           betaW[j] <- E0 + (Emax - E0) * betafun *
                               StandDoseBeta[j]^delta1 * (1 - StandDoseBeta[j])^delta2
                       }
                   })

    ## we will fill in more, depending on which parameters
    ## are fixed, in these two variables:
    start@sample <- c(start@sample,
                      "betaW")
    newInits <- list()
    newModelspecs <- list(refDoseBeta=refDoseBeta)

    ## for E0:
    if(! start@use_fixed["E0"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for E0
                           E0 ~ dunif(E0low, E0high)
                       })

        start@sample <- c(start@sample,
                          "E0")

        newInits$E0 <- mean(E0)
        newModelspecs$E0low <- E0[1]
        newModelspecs$E0high <- E0[2]
    } else {
        newModelspecs$E0 <- E0
    }

    ## for Emax:
    if(! start@use_fixed["Emax"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for Emax
                           Emax ~ dunif(EmaxLow, EmaxHigh)
                       })

        start@sample <- c(start@sample,
                          "Emax")

        newInits$Emax <- mean(Emax)
        newModelspecs$EmaxLow <- Emax[1]
        newModelspecs$EmaxHigh <- Emax[2]
    } else {
        newModelspecs$Emax <- Emax
    }

    ## for delta1 and delta2:
    if(! start@use_fixed["delta1"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for E0
                           delta1 ~ dunif(delta1Low, delta1High)
                       })

        start@sample <- c(start@sample,
                          "delta1")

        newInits$delta1 <- mean(delta1)
        newModelspecs$delta1Low <- delta1[1]
        newModelspecs$delta1High <- delta1[2]
    } else {
        newModelspecs$delta1 <- delta1
    }

    if(! start@use_fixed["mode"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for E0
                           mode ~ dunif(modeLow, modeHigh)
                       })

        start@sample <- c(start@sample,
                          "mode")

        newInits$mode <- mean(mode)
        newModelspecs$modeLow <- mode[1]
        newModelspecs$modeHigh <- mode[2]
    } else {
        newModelspecs$mode <- mode
    }

    ## now define the new modelspecs and init functions:
    oldModelspecs <- start@modelspecs
    start@modelspecs <- function()
    {
        c(oldModelspecs(),
          newModelspecs)
    }

    oldInit <- start@init
    start@init <- function(y, w, nGrid)
    {
        c(oldInit(y, w, nGrid),
          newInits)
    }

    ## finally call the constructor
    .DualEndpointBeta(start,
                      E0=E0,
                      Emax=Emax,
                      delta1=delta1,
                      mode=mode,
                      refDoseBeta=refDoseBeta)
}
#validObject(DualEndpointBeta(E0=10,
#                             Emax=50,
#                             delta1=c(1, 5),
#                             mode=c(3, 10),
#                             refDoseBeta=10,
#                             mean=c(0, 1),
#                             cov=diag(2),
#                             sigma2W=1,
#                             rho=0))


# DualEndpointEmax ----

##' Dual endpoint model with emax function for dose-biomarker relationship
##'
##' This class extends the \code{\linkS4class{DualEndpoint}} class. Here the
##' dose-biomarker relationship \eqn{f(x)} is modelled by a parametric EMAX function:
##'
##' \deqn{f(x) = E_{0} + \frac{(E_{max} - E_{0}) * (x/x^{*})}{ED_{50} + (x/x^{*})}}
##'
##' where \eqn{x^{*}} is a reference dose, \eqn{E_{0}} and \eqn{E_{max}} are the
##' minimum and maximum levels for the biomarker and \eqn{ED_{50}} is the dose
##' achieving half of the maximum effect \eqn{0.5 * E_{max}}.
##'
##' All parameters can currently be assigned uniform distributions or be fixed
##' in advance.
##'
##' @slot E0 either a fixed number or the two uniform distribution parameters
##' @slot Emax either a fixed number or the two uniform distribution parameters
##' @slot ED50 either a fixed number or the two uniform distribution parameters
##' @slot refDoseEmax the reference dose \eqn{x^{*}}
##'
##' @example examples/Model-class-DualEndpointEmax.R
##' @export
##' @keywords classes
.DualEndpointEmax <-
    setClass("DualEndpointEmax",
             representation(E0="numeric",
                            Emax="numeric",
                            ED50="numeric",
                            refDoseEmax="numeric"),
             prototype(E0=c(0, 100),
                       Emax=c(0, 500),
                       ED50=c(0,500),
                       refDoseEmax=1000,
                       use_fixed=
                           c(sigma2W=TRUE,
                                rho=TRUE,
                                E0=FALSE,
                                Emax=FALSE,
                                ED50=FALSE)),
             contains="DualEndpoint",
             validity=
                 function(object){
                     o <- Validate()

                     ## check the prior parameters with variable content
                     for(parName in c("E0", "Emax", "ED50"))
                     {
                         ## if we use a fixed value for this parameter
                         if(object@use_fixed[parName])
                         {
                             ## check range of value
                             o$check(slot(object, parName) > 0,
                                     paste(parName, "must be positive"))
                         } else {
                             ## use a Uniform(a, b) prior
                             o$check(all(slot(object, parName) >= 0) &&
                                         (diff(slot(object, parName)) > 0),
                                     paste(parName,
                                           "has not proper prior parameters"))
                         }
                     }

                     ## check the refDoseEmax
                     o$check(object@refDoseEmax > 0,
                             "refDoseEmax must be positive")

                     o$result()
                 })

##' Initialization function for the "DualEndpointEmax" class
##'
##' @param E0 see \code{\linkS4class{DualEndpointEmax}}
##' @param Emax see \code{\linkS4class{DualEndpointEmax}}
##' @param ED50 see \code{\linkS4class{DualEndpointEmax}}
##' @param refDoseEmax see \code{\linkS4class{DualEndpointEmax}}
##' @param \dots additional parameters, see \code{\linkS4class{DualEndpoint}}
##' @return the \code{\linkS4class{DualEndpointEmax}} object
##'
##' @export
##' @keywords methods
DualEndpointEmax <- function(E0,
                             Emax,
                             ED50,
                             refDoseEmax,
                             ...) {
    start <- DualEndpoint(...)

    start@datanames <- c(start@datanames, "doseGrid")
    start@use_fixed <- c(
      start@use_fixed,
      sapply(list(E0 = E0, Emax = Emax, ED50 = ED50), is.scalar)
    )
    start@priormodel <- h_jags_join_models(
      start@priormodel,
      function() {
        for (j in 1:nGrid) {
          StandDoseEmax[j] <- doseGrid[j] / refDoseEmax
          betaW[j] <- E0 + (Emax - E0) * StandDoseEmax[j] / (ED50 + StandDoseEmax[j])
        }
      }
    )

    ## we will fill in more, depending on which parameters
    ## are fixed, in these two variables:
    start@sample <- c(start@sample,
                      "betaW")
    newInits <- list()
    newModelspecs <- list(refDoseEmax=refDoseEmax)

    ## for E0:
    if(! start@use_fixed["E0"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for E0
                           E0 ~ dunif(E0low, E0high)
                       })

        start@sample <- c(start@sample,
                          "E0")

        newInits$E0 <- mean(E0)
        newModelspecs$E0low <- E0[1]
        newModelspecs$E0high <- E0[2]
    } else {
        newModelspecs$E0 <- E0
    }

    ## for Emax:
    if(! start@use_fixed["Emax"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for Emax
                           Emax ~ dunif(EmaxLow, EmaxHigh)
                       })

        start@sample <- c(start@sample,
                          "Emax")

        newInits$Emax <- mean(Emax)
        newModelspecs$EmaxLow <- Emax[1]
        newModelspecs$EmaxHigh <- Emax[2]
    } else {
        newModelspecs$Emax <- Emax
    }

    ## for ED50:
    if(! start@use_fixed["ED50"])
    {
        start@priormodel <-
            h_jags_join_models(start@priormodel,
                       function(){
                           ## uniform for ED50
                           ED50 ~ dunif(ED50Low, ED50High)
                       })

        start@sample <- c(start@sample,
                          "ED50")

        newInits$ED50 <- mean(ED50)
        newModelspecs$ED50Low <- ED50[1]
        newModelspecs$ED50High <- ED50[2]
    } else {
        newModelspecs$ED50 <- ED50
    }


    ## now define the new modelspecs and init functions:
    oldModelspecs <- start@modelspecs
    start@modelspecs <- function()
    {
        c(oldModelspecs(),
          newModelspecs)
    }

    oldInit <- start@init
    start@init <- function(y, w, nGrid)
    {
        c(oldInit(y, w, nGrid),
          newInits)
    }

    ## finally call the constructor
    .DualEndpointEmax(start,
                      E0=E0,
                      Emax=Emax,
                      ED50=ED50,
                      refDoseEmax=refDoseEmax)
}
#validObject(DualEndpointEmax(E0=10,
#                             Emax=50,
#                             ED50=20,
#                             refDoseEmax=10,
#                             mean=c(0, 1),
#                             cov=diag(2),
#                             sigma2W=1,
#                             rho=0))

# ModelPseudo ----

## =========================================================================
##' Class of models using expressing their prior in form of Pseudo data
##'
##' This is the Pseudo model class, from which all models where their prior
##' are expressed in form of pseudo data (as if some data are
##' available before the trial starts) inherit. It also inherits all slots
##' from \code{\linkS4class{AllModels}}.No slots for this class
##'
##' @seealso \code{\linkS4class{LogisticIndepBeta}},
##' \code{\linkS4class{Effloglog}},
##' \code{\linkS4class{EffFlexi}}
##'
##' @export
##' @keywords classes
.ModelPseudo<-setClass(Class="ModelPseudo",
                       contains="AllModels"
)
##' No intialization function

# ModelTox ----

## ===========================================================================

##' Class for DLE models using pseudo data prior.
##' This is a class of DLE (dose-limiting events) models/ toxicity model which contains all DLE models
##' for which their prior are specified in form of pseudo data (as if there is some data before
##' the trial starts). It inherits all slots from \code{\linkS4class{ModelPseudo}}
##'
##' The \code{data} must obey the convention that the data input is called exactly in the
##' \code{\linkS4class{Data}} class. This refers to any observed DLE responses (\code{y} in
##' \code{\linkS4class{Data}} class), the dose (levels) (\code{x} in \code{\linkS4class{Data}} class)
##' at which these responses are observed, all dose levels considered in the study (\code{doseGrid}
##' in \code{\linkS4class{Data}}) class and other specifications in \code{\linkS4class{Data}}
##' class that can be used to generate prior or
##' posterior modal estimates or samples estimates for model parameter(s). If no responses is observed,
##' at least \code{doseGrid} in \code{\linkS4class{Data}} has to be specified in \code{data} slot for which
##' prior modal estimates or samples can be obtained for model parameters based on the specified pseudo
##' data.
##' @slot data refers to the data input specification in \code{\linkS4class{Data}} class which are used to
##' obtain model parameters estimates or samples (see details above)
##'
##'
##' @seealso \code{\linkS4class{LogisticIndepBeta}},
##' \code{\linkS4class{Effloglog}},
##' \code{\linkS4class{EffFlexi}}
##'
##'
##' @export
##' @keywords classes
.ModelTox<-setClass(Class="ModelTox",
                    representation(data="Data"),
                    contains="ModelPseudo"
)
##' No Initialization function

# ModelEff ----

## ==========================================================================================

##' class for Efficacy models using pseudo data prior
##'
##' This is a class of which contains all efficacy models for which their prior are specified in
##' form of pseudo data. It inherits all slots from \code{\linkS4class{ModelPseudo}}
##'
##' The \code{dose} function has a first argument \code{ExpEff}, a scalar expected efficacy value
##' which is targeted. Additional arguments are model parameters. It computes using modal estimate(s)
##' or samples model parameter(s), the resulting expected efficacy value at that dose level. If samples
##' of the model parameters are used, the function must vectorize over the model parameters.
##'
##' The \code{ExpEff} function has a first argument \code{dose}, a scalar dose level which is targeted.
##' Additional arguments are model parameters. It computes using modal estimates or samples of the
##' model parameter(s), the resulting dose level given that particular expected efficacy value. If samples
##' of the model parameter(s) are used, the function must vectorize over the model parameters.
##'
##' The \code{data} must obey the convention that the data input is called exactly in the
##' \code{\linkS4class{DataDual}} class. This refers to any observed Efficacy/biomarker responses
##' (\code{w} in
##' \code{\linkS4class{DataDual}} class), the dose (levels) (\code{x} in \code{\linkS4class{DataDual}} or
##' \code{Data} class)
##' at which these responses are observed, all dose levels considered in the study (\code{doseGrid}
##' in \code{\linkS4class{DataDual}} or \code{Data}) class and other specifications in
##' \code{\linkS4class{DataDual}}
##' class that can be used to generate prior or
##' posterior modal estimates or samples estimates for model parameter(s). If no responses is observed,
##' at least \code{doseGrid} in \code{\linkS4class{DataDual}} has to be specified in \code{data} slot
##' for which prior modal estimates or samples can be obtained for model parameters based on
##' the specified pseudo data.
##'
##' @slot dose a function computing the dose reaching a specific target value of expected efficacy, based
##' on the model parameter(s). The model parameter(s) (samples) are obtained based on prior specified
##' in form of pseudo data and if any together with any observed responses (see details above)
##'
##' @slot ExpEff a function computing the expected efficacy (value) for a specific dose, based on model
##' parameter(s). The model parameter(s) (samples) are obtained based on pseudo data prior and (if any)
##' with observed responses (see details above)
##'
##' @slot data refers to the data input specification in \code{\linkS4class{DataDual}} class which are used to
##' obtain model parameters estimates or samples (see details above)
##'
##' @seealso \code{\linkS4class{Effloglog}},
##' \code{\linkS4class{EffFlexi}}
##'
##' @export
##' @keywords classes
.ModelEff<-setClass(Class="ModelEff",
                    representation(dose="function",
                                   ExpEff="function",
                                   data="DataDual"),
                    contains="ModelPseudo"
)
validObject(.ModelEff)
##' No initialization function

# LogisticIndepBeta ----

## ==============================================================================
##' Standard logistic model with prior in form of pseudo data
##'
##' This is a class for the two-parameter logistic regression DLE model with prior expressed
##' in form of pseudo data. This model describe the relationship of the binary DLE (dose-limiting
##' events) responses and the dose levels. More specifically, this DLE model represents the relationship
##' of the probabilities of the occurrence of a DLE with their corresponding dose levels in log scale.
##' This model is specified as
##' \deqn{p(d_{(j)})= \frac{exp(\phi_1+\phi_2 log(d_{(j)}))}{1+exp(\phi_1+\phi_2 log(d_{(j)}))}}
##' for any dose j where \eqn{p(d_{(j)})} is the probability of the occurrence of a DLE at dose j.
##' The two parameters of this model is the intercept \eqn{\phi_1} and the slope \eqn{\phi_2}
##' It inherits all slots from \code{\linkS4class{ModelTox}} class.
##'
##' The pseudo data can be interpreted as as if we obtain some observations before the trial starts.
##' These pseudo data can be used to express our prior, the initial beliefs for the model parameter(s).
##' The pseudo data are expressed in the following way. First, fix at least two dose levels which are
##' Then ask for experts' opinion how many subjects are to be treated at each of these dose levels and
##' the number of subjects observed with DLE are observed. At each dose level, the number of subjects
##' observed with a DLE divided by the total number of subjects treated is the probability of the
##' occurrence of a DLE at that particular dose level. The probabilities of the occurrence of a DLE
##' based on these pseudo data are independent Beta distributions. Therefore, the joint prior probability
##' density function of all these probabilities can be obtained. Hence, by a change of variable, the
##' joint prior probability density function of the two parameters in this model can also be obtained.
##' In addition, a conjugate joint prior density function of the two parameters in the model is used.
##' For details about the form of all these joint prior and posterior probability density function, please
##' refers to Whitehead and Willamson (1998).
##'
##'
##' When expressing the pseudo data, \code{binDLE},\code{DLEdose} and \code{DLEweights} are used.
##' The \code{binDLE} represents the number of subjects observed with DLE. Note that, since the imaginary
##' nature of the pseudo data, the number of subjects observed with DLE is not necessary to be integer(s)
##' but any scalar value.
##' The \code{DLEdose} represents the dose levels at which the pseudo DLE responses (\code{binDLE}) are
##' observed.
##' The \code{DLEweights} represents the total number of subjects treated.
##' Since at least two DLE pseudo responses are needed to obtain prior modal estimates (same as the maximum
##' likelihood estimates) for the model parameters. \code{binDLE}, \code{DLEdose} and \code{DLEweights} must
##' all be vectors of at least length 2. Since given one pseudo DLE responses, the number of subjects observed
##' with a DLE relates to at which dose level they are treated and the total number of of subjects treated at
##' this dose level. Therefore, each of the elements in any of the vectors of \code{binDLE}, \code{DLEdose} and
##' \code{DLEweights} must have a corresponding elements in the other two vectors. A set of three values with
##' one of each in the vectors of \code{binDLE}, \code{DLEdose} and \code{DLEweights}. In this model, each of
##' these three values must be specified in the same position as in each of the vector of \code{binDLE},
##' \code{DLEdose} and \code{DLEweights}. The order of the values or elements in one of the vector \code{binDLE},
##' \code{DLEdose} and \code{DLEweights} must corresponds to the values or elements specified in the other two
##' vectors.
##'
##' @slot binDLE represents the vector of pseudo DLE responses. This must be at least f length 2 and the
##' order of its elements must corresponds to values specified in \code{DLEdose} and \code{DLEweights}.
##' (see details from above)
##' @slot DLEdose represents the vector of the corresponding dose levels observed at each of the
##' pseudo DLE responses (\code{binDLE}). This mus be at least of length 2 and the order of its elements
##' must corresponds to values specified in \code{binDLE} and \code{DLEweights}.
##' (see details from above)
##' @slot DLEweights refers to the total number of subjects treated at each of the pseudo dose level
##' (\code{DLEdose}). This must be of length of at least 2 and the order of its elements must corresponds
##' to values specified in \code{binDLE} and \code{DLEdose}. (see details from above)
##' @slot phi1 refers the intercept of the model. This slot is used in output to display the resulting prior
##' or posterior modal estimate of the intercept obtained based on the pseudo data and (if any)
##' observed data/responses.
##' @slot phi2 refers to slope of the model. This slot is used in output to display the resulting prior or
##' posterior modal estimate of the slope obtained based on the pseudo data and (if any) the observed data/responses.
##' @slot Pcov refers to the covariance matrix of the intercept (phi1) and the slope parameters (phi2) of the
##' model. This is used in output to display the resulting prior and posterior covariance matrix of phi1 and
##' phi2 obtained, based on the pseudo data and (if any) the observed data and responses. This slot is needed for
##' internal purposes.
##'
##' @example examples/Model-class-LogisticIndepBeta.R
##' @export
##' @keywords classes
.LogisticIndepBeta<-
  setClass(Class="LogisticIndepBeta",
           representation(binDLE="numeric",
                          DLEdose="numeric",
                          DLEweights="numeric",

                          phi1="numeric",
                          phi2="numeric",
                          Pcov="matrix"),
           prototype(binDLE=c(0,0),
                     DLEdose=c(1,1),
                     DLEweights=c(1,1)),
           contains="ModelTox",
           validity=
             function(object){
               o <- Validate()
               ##Check if at least two pseudo DLE responses are given
               o$check(length(object@binDLE) >= 2,
                       "length of binDLE must be at least 2")
               ##Check if at least two weights for pseudo DLE are given
               o$check(length(object@DLEweights) >= 2,
                       "length of DLEweights must be at least 2")
               ##Check if at least two corresponding dose levels are given for the pseudo DLE responses
               o$check(length(object@DLEdose) >= 2,
                       "length of DLEdose must be at least 2")
               ##Check if pseudo DLE responses have same length with it corresponding dose levels and weights
               o$check((length(object@binDLE)==length(object@DLEweights))&(length(object@binDLE)==length(object@DLEdose))&(length(object@DLEweights)==length(object@DLEdose)),
                       "length of binDLE, DLEweights, DLEDose must be equal")
               o$result()
             })
validObject(.LogisticIndepBeta())

##' Intialization function for "LogisticIndepBeta" class
##' @param binDLE the number of subjects observed with a DLE, the pseudo DLE responses
##' @param DLEdose the corresponding dose levels for the pseudo DLE responses, pseudo dose levels
##' @param DLEweights the total number of subjects treated at each of the dose levels, pseudo weights
##' @param data the input data to update estimates of model parameters and
##' follow the \code{\linkS4class{Data}} object class specification
##' @return the \code{\linkS4class{LogisticIndepBeta}}
##'
##' @export
##' @keywords methods
LogisticIndepBeta <- function(binDLE,
                              DLEdose,
                              DLEweights,
                              data)
{##if no observed DLE(data)
  if (length(data@y)==0){
    w1<-DLEweights
    y1<-binDLE
    x1<-DLEdose} else {w1<-c(DLEweights,rep(1,data@nObs))
    ##combine pseudo and observed
    y1<-c(binDLE,data@y)
    x1<-c(DLEdose,data@x)}
  ##Fit the pseudo data and DLE responses with their corresponding dose levels
  FitDLE<-suppressWarnings(glm(y1/w1~log(x1),family=binomial(link="logit"),weights=w1))
  SFitDLE<-summary(FitDLE)
  ##Obtain parameter estimates for dose-DLE curve
  phi1<-coef(SFitDLE)[1,1]
  phi2<-coef(SFitDLE)[2,1]
  ## covariance matrix of phi1 and phi2
  Pcov <- vcov(FitDLE)

  .LogisticIndepBeta(binDLE=binDLE,
                     DLEdose=DLEdose,
                     DLEweights=DLEweights,
                     phi1=phi1,
                     phi2=phi2,
                     Pcov=Pcov,
                     datanames=c("nObs","y","x"),
                     data=data
  )
}

# Effloglog ----

## ======================================================================================================
##' Class for the linear log-log efficacy model using pseudo data prior
##'
##' This is the efficacy model which describe the relationship of the continuous efficacy responses and
##' the dose levels. More specifically, this is a model to describe the linear relationship between the
##' continuous efficacy responses and its coressponding dose level in log-log scale.
##' The efficacy log-log model is given as
##' \deqn{y_i=\theta_1 +theta_2 log(log(d_i))+\epsilon_i}
##' where \eqn{y_i} is the efficacy responses
##' for subject i, \eqn{d_i} is the dose level treated for subject i and \eqn{\epsilon_i} is the random error
##' term of efficacy model at subject i such that \eqn{\epsilon_i} has a normal distribution of mean 0 and
##' variance \eqn{\sigma^2=\nu^{-1}}. This variance is assumed to be the same for all subjects.
##'
##' There are three parameters in this model which is to intercept \eqn{\theta_1}, the slope \eqn{\theta_2}
##' and the precision \eqn{\nu} of the efficacy responses.
##' It inherit all slots from \code{\linkS4class{ModelEff}}
##'
##' The prior of this model is specified in form of pseudo data. First at least two dose levels are fixed.
##' Then ask for experts' opinion about the efficacy values that can be obtained at each of the dose levels
##' if one subject is treated at each of these dose levels. The prior modal estimates (same as the maximum
##' likelihood estimates) can be obtained for the intercept and slope paramters in this model.
##'
##' The \code{Eff} and \code{Effdose} are used to represent the prior in form of the pseudo data.
##' The \code{Eff} represents the pseudo scalar efficacy values. The \code{Effdose} represents the dose levels
##' at which these pseudo efficacy values are observed. These pseudo efficacy values are always specified by
##' assuming one subject are treated in each of the dose levels. Since at least 2 pseudo efficacy values are
##' needed to obtain modal estimates of the intercept and slope parameters, both \code{Eff} and \code{Effdose}
##' must be vector of at least length 2. The position of the values or elements specified in \code{Eff} or
##' \code{Effdose} must be corresponds to the same elements or values in the other vector.
##'
##' The \code{nu} represents the prior precision \eqn{\nu} of the pseudo efficacy responses. It is also known as the inverse
##' of the variance of the pseudo efficacy responses. The precision can be a fixed constant or having a gamma
##' distribution. Therefore, single scalar value, a fixed
##' value of the precision can be specified. If not, two positive scalar values must be specified as the
##' shape and rate parameter of the gamma distribution. If there are some observed efficacy responses available,
##' in the output, \code{nu} will display the updated value of the precision or the updated values for the
##' parameters of the gamma distribution.
##'
##'
##' Given the variance of the pseudo efficacy responses, the joint prior distribution of the intercept \eqn{\theta_1}
##' (theta1) and the slope \eqn{\theta_2} (theta2) of this model is a bivariate normal distribution.
##' A conjugate posterior joint distribution is also used for theta1 and theta2. The joint prior bivariate
##' normal distribution has
##' mean \eqn{\boldsymbol\mu_0} and covariance matrix \eqn{(\nu \mathbf{Q}_0)^{-1}}. \eqn{\boldsymbol\mu_0} is a
##' \eqn{2 \times 1}
##' column vector contains the prior modal estimates of the intercept (theta1) and the slope (theta2). Based on
##' \eqn{r} for \eqn{r \geq 2} pseudo efficacy responses specified, \eqn{\mathbf{X}_0} will be the
##' \eqn{r \times 2} design matrix
##' obtained for these pseudo efficacy responses. the matrix \eqn{\mathbf{Q}_0} will be calculated by
##' \eqn{\mathbf{Q}_0=\mathbf{X}_0 \mathbf{X}^T_0} where \eqn{\nu} is the precision of the pseudo efficacy responses.
##' For the joint posterior bivariate distribution, we have \eqn{\boldsymbol{\mu}} as the mean and
##' \eqn{(\nu\mathbf{Q}_0)^{-1}} as the covariance matrix. Here, \eqn{\boldsymbol\mu} is the column vector containing the
##' posterior modal estimates
##' of the intercept (theta1) and the slope (theta2). The design matrix \eqn{\mathbf{X}} obtained based only on
##' observed efficacy responses will give \eqn{\mathbf{Q}=\mathbf{X}\mathbf{X}^T} with \eqn{\nu} as the precision of
##' the observed efficacy responses. If no observed efficacy responses are available (i.e only pseudo
##' efficacy responses are used), the \code{vecmu}, \code{matX}, \code{matQ} and \code{vecY} represents
##' \eqn{\boldsymbol\mu_0}, \eqn{\mathbf{X}_0}, \eqn{\mathbf{Q}_0} and the column vector of pseudo efficacy responses,
##' respectively. If there are some observed efficacy responses, \code{vecmu}, \code{matX}, \code{matQ}
##' and \code{vecY} will represent \eqn{\boldsymbol\mu}, \eqn{\mathbf{X}}, \eqn{\mathbf{Q}} and the column vector contains
##' all observed efficacy responses, respectively. (see details in about the form of prior and posterior distribution)
##'
##' @slot Eff the pseudo efficacy response, the scalar efficacy values. This must be a vector of at least
##' length 2. Each element or value here must represents responses treated based on one subject. The order
##'  of its elements must corresponds to the values presented in vector \code{Effdose} (see details above)
##' @slot Effdose the pseudo efficacy dose level. This is the dose levels at which the pseudo efficacy
##' responses are observed at. This must be a vector of at least length 2 and the order of its elements must
##' corresponds to values presented in vector \code{Eff} (see details above)
##' @slot nu refers to the prior precision of pseudo efficacy responses. This is either a fixed value or a
##' vector of elements \code{a}, a positive scalar for the shape parameter, and \code{b}, a positive scalar
##' for the rate parameter for the gamma distribution. (see detail from above)
##' @slot useFixed a logical value if \code{nu} specified is a fixed value or not. This slot is needed for
##' internal purposes and not to be touched by the user.
##' @slot theta1 The intercept \eqn{\theta_1} parameter of this efficacy log-log model. This slot is used in output to display
##' the resulting prior or posterior modal estimates obtained based on the pseudo data and (if any) the
##' observed data/ responses.
##' @slot theta2 The slope \eqn{theta_2} parameter of the efficacy log-log model. This slot is used in output to display
##' the resulting prior or posterior modal estimates obtained based on the pseudo data and (if any) the
##' observed data/ responses.
##' @slot Pcov refers to the covariance matrix of the intercept (phi1) and slope (phi2) parameters of this model.
##' This slot is used in output to display the covariance matrix obtained based on the pseudo data and (if any)
##' the observed data/responses. This slot is needed for internal purposes.
##' @slot vecmu is the column vector of the prior or the posterior modal estimates of the intercept (phi1) and
##' the slope (phi2).
##' This slot is used in output to display as the mean of the prior or posterior bivariate normal distribution
##' for phi1 and phi2. (see details from above)
##' @slot matX is the design matrix based on either the pseudo or all observed efficacy response. This is used in
##' output to display the design matrix for the pseudo or the observed efficacy responses (see details from above)
##' @slot matQ is the square matrix of multiplying the the design matrix with its transponse. This is represented
##' either using the only the pseudo efficacy responses or only with the observed efficacy responses. This is display
##' in the output (see details from above)
##' @slot vecY is the column vector either contains the pseudo efficacy responses or all the observed efficacy
##' responses. This is used in output to display the pseudo or observed efficacy responses (see detail from above)
##' @slot c is a constant value greater or equal to 0, with the default 0 leading
##' to the model form described above. In general, the model has the form
##' \eqn{y_i=\theta_1 +theta_2 log(log(d_i + c))+\epsilon_i}, such that dose levels
##' greater than \eqn{1-c} can be considered as described in Yeung et al. (2015).
##'
##' @example examples/Model-class-Effloglog.R
##' @export
##' @keywords methods
.Effloglog<-
  setClass(Class="Effloglog",
           representation(Eff="numeric",
                          Effdose="numeric",
                          nu="numeric",
                          useFixed="logical",
                          theta1="numeric",
                          theta2="numeric",
                          Pcov="matrix",
                          vecmu="matrix",
                          matX="matrix",
                          matQ="matrix",
                          vecY="matrix",
                          c="numeric"),
           prototype(Eff=c(0,0),
                     Effdose=c(1,1),
                     nu=1/0.025,
                     useFixed=TRUE,
                     c=0),
           contains="ModelEff",
           validity=
             function(object){
               o <- Validate()

               o$check(length(object@Eff) >= 2,
                       "length of Eff must be at least 2")
               o$check(length(object@Effdose) >= 2,
                       "length of Effdose must be at least 2")
               o$check(length(object@Eff)==length(object@Effdose),
                       "length of Eff and Effdose must be equal")
               if (object@useFixed == "TRUE"){
                 o$check((length(object@nu)==1)&&(object@nu > 0),
                         "nu must be a single positive real number")} else {
                           o$check(identical(names(slot(object,"nu")),c("a","b")),
                                   "nu must have names 'a' and 'b' ")
                           o$check(all(slot(object,"nu") > 0),
                                   "nu must have positive prior paramters")
                           o$check(identical(length(object@nu),2L),
                                   "nu must have length at most 2")
                         }
               o$result()
             })
validObject(.Effloglog())

##' Initialization function for the "Effloglog" class
##'
##' @param Eff the pseudo efficacy responses
##' @param Effdose the corresponding dose levels for the pseudo efficacy responses
##' @param nu the precision (inverse of the variance) of the efficacy responses
##' @param data the input data of \code{\linkS4class{DataDual}} class to update model estimates
##' @param c the constant value added to the dose level when the dose level value is less than or
##' equal to 1 and a special form of the linear log-log has to applied (Yeung et al. (2015).).
##' @return the \code{\linkS4class{Effloglog}} object
##'
##' @importFrom MASS ginv
##' @export
##' @keywords methods
Effloglog<-function(Eff,
                    Effdose,
                    nu,
                    data,
                    c=0)

{if (!all(data@doseGrid > 1 - c))
  stop("doseGrid in data must be greater than 1 - c for Effloglog model")

  ##No observed Efficacy response
  if (length(data@w)==0){
    w1 <- Eff
    ## always add the constant value c (default is 0)
    x1 <- Effdose + c
  } else {##Combine pseudo data and Observed Efficacy without DLT
    w1<-c(Eff, getEff(data)$w_no_dlt)
    x1<-c(Effdose, getEff(data)$x_no_dlt + c)

    w2 <- getEff(data)$w_no_dlt
    x2 <- getEff(data)$x_no_dlt + c
  }

  ##Check if sigma2/nu is a fixed contant

  useFixed <- identical(length(nu), 1L)
  ##Fit pseudo and observed efficacy
  FitEff <- suppressWarnings(glm(w1~log(log(x1)),family=gaussian))
  SFitEff <- summary(FitEff)
  ##Obtain paramter estimates
  theta1<-coef(SFitEff)[1,1]
  theta2<-coef(SFitEff)[2,1]
  ##covariance matrix of theta1 and theta2
  Pcov <- vcov(FitEff)
  ##if sigma2/nu is not a fixed constant
  if (length(nu)==2){
    mu0<-matrix(c(theta1,theta2),2,1)
    vecmu<-mu0
    X0<-matrix(c(1,1,log(log(Effdose[1] + c)),log(log(Effdose[2] + c))),2,2)
    matX<-X0
    Q0=t(X0)%*%X0
    matQ<-Q0
    vecY<-matrix(Eff,2,1)
    ##if there are some observed efficacy
    if (length(data@w)!=0){
      X<-matrix(c(rep(1,length(x2)), log(log(x2))), length(x2),2)
      matX<-X
      mu<-MASS::ginv(Q0+t(X)%*%X)%*%(Q0%*%mu0+t(X)%*%t(t(w2)))
      vecmu<-mu
      Q<-Q0+t(X)%*%X
      matQ<-Q
      vecY<-matrix(w2,length(w2),1)
      a<-nu[1]+(length(w2))/2
      b<-nu[2]+(t(w2)%*%t(t(w2))+t(mu0)%*%Q0%*%mu0-t(mu)%*%Q%*%mu)/2
      nu[1]<-a
      nu[2]<-b}} else {nu<-nu}

  .Effloglog(Eff=Eff,
             Effdose=Effdose,
             nu=nu,
             useFixed=useFixed,
             datanames=c("nObs","w","x"),
             data=data,
             dose=function(ExpEff,theta1,theta2){
               LogDose<-exp((ExpEff-theta1)/theta2)
               return(exp(LogDose) - c)
             },
             ExpEff=function(dose,theta1,theta2){
               dose <- dose + c
               return(theta1+theta2*log(log(dose)))
             },
             theta1=theta1,
             theta2=theta2,
             Pcov=Pcov,
             vecmu=vecmu,
             matX=matX,
             matQ=matQ,
             vecY=vecY,
             c=c
  )}

# EffFlexi ----

## =========================================================================================
##' Class for the efficacy model in flexible form for prior expressed in form of pseudo data
##'
##' This is a class where a flexible form is used to describe the relationship between the efficacy
##' responses and the dose levels. This flexible form aims to capture different shape for the
##' dose-efficacy curve and the mean efficacy responses at each dose level are estimated using MCMC.
##' In addition, the first (RW1) or second order (RW2) random walk model can be used for smoothing data. That is
##' the random walk model is used to model the first or the second order difference of the mean
##' efficacy responses to its neighboring dose levels of their mean efficacy responses.
##' The flexible form is specified as
##' \deqn{\mathbf{W}\vert\boldsymbol{\beta_w}, \sigma^2 \sim Normal (\mathbf{X}_w \boldsymbol{\beta_w}, \sigma^2 \mathbf{I})}
##' where \eqn{\mathbf{W}} represent the column vector of the efficacy responses, \eqn{\boldsymbol{\beta_w}}
##' is th column vector of the mean efficacy responses for all dose levels, \eqn{\mathbf{X_w}} is the
##' design matrix with entries \eqn{I_{i(j)}} which gives a value 1 if subject i is allocated to
##' dose j. The \eqn{\sigma^2} (sigma2) is the variance of the efficacy responses which can be either fixed or from
##' an inverse gamma distribution.
##'
##' The RW1 model is given as
##' \deqn{\beta_{W,(j)} - \beta_{W,(j-1)} \sim Normal(0, \sigma^{2}_{\beta_{W}})}
##' where \eqn{\beta_{W,(j)}} is the mean efficacy responses at dose j
##' For the RW2 is given as
##' \deqn{\beta_{W,(j-2)} - 2 \beta_{W,(j-1)} + \beta_{W,(j)} \sim Normal(0, \sigma^{2}_{\beta_{W}})}
##' The variance parameter \eqn{\sigma^{2}_{\beta_{W}}}. The variance \eqn{\sigma^{2}_{\beta_{W}}}
##' (sigma2betaW) will be the same at all dose levels and can
##' either be fixed or assigned an inverse gamma prior distribution.
##'
##' The \code{Eff} and \code{Effdose} are the pseudo efficacy responses and dose levels at which these
##' pseudo efficacy responses are observed at. (see more details for \code{\linkS4class{Effloglog}} class)
##' \code{Eff} and \code{Effdose} must be vector of at least length 2. The values or elements in vectors
##' \code{Eff} or \code{Effdose} must put in the same position with its corresponding value in the other
##' vector. The \code{sigma2} is the prior variance of the flexible efficacy form. The variance is either specified
##' with a single scalar value (fixed) or positive scalar value have to be specified for the \code{a} shape and
##' \code{b} slope parameter for th inverse gamma distribution. Similarly, \code{sigma2betaW} is the prior variance
##' of the random walk model which can be specified with a single scalar (fixed) value or specifying positive
##' scalar values for the shape \code{a} and rate \code{b} parameters for the inverse gamma distributions.
##' This model will output the updated value or the updated values of the parameters of the inverse gamma
##' distributions for \eqn{sigma^2} (sigma2) and \eqn{\sigma^2_{\beta_W}} (`sigma2betaW`)
##'
##' @slot Eff the pseudo efficacy responses. A vector of at least length 2 with the elements here and its
##' corresponding value in \code{Effdose} must be specified in the same position. (see details above)
##' @slot Effdose the dose levels at which the pseudo efficacy responses are observed. This is a vector of at
##' least length 2 and the elements here and its corresponding value in \code{Eff} must be specified in the
##' same position. (see details from above)
##' @slot sigma2 the prior variance of the flexible efficacy form. It can be specified with a single positive
##' scalar or specifying \code{a}, the shape and \code{b}, the rate parameter of the inverse gamma
##' distribution. (see details from above)
##' @slot sigma2betaW the prior variance of the random walk model for the mean efficacy responses. A single
##' positive scalar can be specified or specifying \code{a}, the shape and \code{b}, the rate parameter of
##' the inverse gamma distribution (see details from above)
##' @slot useFixed a list of with logical value to each of the parameters \code{sigma2} and \code{sigma2betaw}
##' indicating whether a fixed value is used or not; this slot is needed for internal purposes and not to
##' be touched by the user.
##' @slot useRW1 for specifying the random walk model for the mean efficacy responses; if \code{TRUE},
##' first order random walk model is used, otherwise the second-order random walk model.
##' @slot designW is the design matrix for the efficacy responses. If only the pseudo efficacy responses
##' are used, this will be the design matrix of the pseudo efficacy responses. If there are some observed
##' efficacy responses available. It will be the design matrix based on both the pseudo and the observed
##' efficacy responses.
##' @slot RWmat is the the difference matrix for the random walk model. This slot is needed for internal
##' purposes and not to be touched by the user.
##' @slot RWmatRank is the rank of the difference matrix. This slot is needed for internal purposes and not
##' to be touched by the user.
##'
##' @example examples/Model-class-EffFlexi.R
##' @export
##' @keywords class
.EffFlexi<-setClass(Class="EffFlexi",
representation(Eff="numeric",
               Effdose="numeric",
               sigma2="numeric",
               sigma2betaW="numeric",
               useFixed="list",
               useRW1="logical",
               designW="matrix",
               RWmat="matrix",
               RWmatRank="integer"),
prototype(Eff=c(0,0),
          Effdose=c(1,1),
          sigma2=0.025,
          sigma2betaW=1,
          useRW1=TRUE,
          useFixed=list(sigma2=TRUE,sigma2betaW=TRUE)),
contains="ModelEff",
validity=
  function(object){
    o<- Validate()
    o$check(length(object@Eff) >= 2,
            "length of Eff must be at least 2")
    o$check(length(object@Effdose) >= 2,
            "length of Effdose must be at least 2")
    o$check(length(object@Eff)==length(object@Effdose),
            "length of Eff and Effdose must be equal")
    for (parName in c("sigma2","sigma2betaW"))
    {
      if (object@useFixed[[parName]]){
        o$check(slot(object,parName) > 0,
                paste(parName, "must be positive"))} else {
                  o$check(identical(names(slot(object,parName)),c("a","b")),
                          paste(parName,"must have names 'a' and 'b'"))
                  o$check(all(slot(object,parName) > 0),
                          paste(parName, "must have positive prior parameters"))
                }
    }
    o$result()
  })
validObject(.EffFlexi())

##' Initialization function for the "EffFlexi" class
##'
##' @param Eff the pseudo efficacy responses
##' @param Effdose the corresponding dose levels for the pseudo efficacy responses
##' @param sigma2 the prior variance of the efficacy responses which can be specified
##' with a single positive scalar or with two positive scalar values for the shape \code{a} and
##' the rate \code{b} parameters of the inverse gamma distribution.
##' @param sigma2betaW the prior variance of the random walk model used for smoothing which can be
##' specified with a single positive scalar or with two positive scalars representing the shape \code{a}
##' and the rate \code{b} parameter of the inverse gamma distribution.
##' @param smooth used for smoothing data for this efficacy model. That is either the "RW1", the
##' first-order random walk model or "RW2", the second-order random walk model is used of the mean
##' efficacy responses.
##' @param data the input data to update estimates of model parameters and
##' follow the \code{\linkS4class{DataDual}} object class specification
##' @return the \code{\linkS4class{EffFlexi}} class object
##'
##' @export
##' @keywords methods

EffFlexi <- function(Eff,
                     Effdose,
                     sigma2,
                     sigma2betaW,
                     smooth=c("RW1","RW2"),
                     data
)
{##No observed Efficacy response
  if (length(data@w)==0){
    w1<-Eff
    x1<-Effdose} else {
      ## with observed efficacy responses and no DLT observed
      w1<-c(Eff, getEff(data)$w_no_dlt)
      x1<-c(Effdose, getEff(data)$x_no_dlt)
    }
  ## Match dose levels in x1 with the all dose levels for evaluations
  x1Level <- matchTolerance(x1,data@doseGrid)
  smooth<-match.arg(smooth)
  useRW1<- smooth == "RW1"
  useFixed<-list()
  for (parName in c("sigma2","sigma2betaW"))
  {useFixed[[parName]] <- identical(length(get(parName)),1L)}
  #design matrics
  designW <- model.matrix(~ -1 + I(factor(x1Level, levels=seq_len(data@nGrid))))
  dimnames(designW) <- list(NULL,NULL)

  ##difference matrix of order 1:
  D1mat<- cbind(0,diag(data@nGrid-1)) - cbind(diag(data@nGrid - 1),0)

  ## set up the random walk penalty matrix and its rank:
  if (useRW1)
  {## the rank-deficient prior precision for the RW1 prior:
    RWmat <- crossprod(D1mat)
    ##Rank: dimension -1
    RWmatRank <- data@nGrid-1L
  } else {##second-order difference
    D2mat <- D1mat[-1,-1] %*% D1mat
    RWmat <- crossprod(D2mat)
    RWmatRank <- data@nGrid-2L
  }
  .EffFlexi(Eff=Eff,
            Effdose=Effdose,
            sigma2=sigma2,
            sigma2betaW=sigma2betaW,
            datanames=c("nObs","w","x"),
            data=data,
            dose=function(ExpEff){
              ##Find dose level given a particular Expected Efficacy level with linear Interpolation
              dosevec<-c()
              for (k in 1:sampleSize(options)){
                IxEff0<- max(which((ExpEff-Effsamples@data$ExpEff[k,]) >= 0))
                IxEff1<- min(which((ExpEff-Effsamples@data$ExpEff[k,]) < 0))
                Interpoldose<-data@doseGrid[IxEff0]+(data@doseGrid[IxEff1]-data@doseGrid[IxEff0])*((ExpEff-Effsamples@data$ExpEff[k,IxEff0])/(Effsamples@data$ExpEff[k,IxEff1]-Effsamples@data$ExpEff[k,IxEff0]))
                dosevec[k]<-Interpoldose
              }
              ##return coreresponding dose levels
              return(dosevec)},

            ExpEff=function(dose,data,Effsamples){
              ##Find the ExpEff with a given dose level
              ##Check if given dose is in doseGrid
              DoseInGrid<-!is.na(matchTolerance(dose,data@doseGrid))
              if (DoseInGrid==TRUE){
                ##Find which dose is this in the dose Grid
                EIx<-matchTolerance(dose,data@doseGrid)
                ##Return corresponding expected efficacy values from mcmc samples
                return(Effsamples@data$ExpEff[,EIx])} else {##if dose not in doseGrid do linear Interploation
                  ## check if this dose is within doseGrid
                  stopifnot(dose <= max(data@doseGrid), dose >= min(data@doseGrid))
                  Ixd0 <- max(which((dose-data@doseGrid) > 0))
                  Ixd1<-min(which((dose-data@doseGrid) < 0))
                  ExpEffd0<-Effsamples@data$ExpEff[,Ixd0]
                  ExpEffd1<-Effsamples@data$ExpEff[,Ixd1]
                  InterpolExpEff<- ExpEffd0+(ExpEffd1-ExpEffd0)*((dose-data@doseGrid[Ixd0])/(data@doseGrid[Ixd1]-data@doseGrid[Ixd0]))
                  return(InterpolExpEff)}
            },
            useFixed=useFixed,
            useRW1=useRW1,
            designW=designW,
            RWmat=RWmat,
            RWmatRank=RWmatRank)}
## ---------------------------------------------------------------------------------------------------------

# DALogisticLogNormal ----

##' Logistic model with bivariate (log) normal prior and data augmentation
##'
##' This is a modified data augmented CRM with logistic regression model using
##' a bivariate normal prior on the intercept and log slope parameters.
##' This class inherits from the normal logistic model class.
##'
##' We still need to include here formula for the lambda prior.
##'
##' @slot npiece the number of pieces in the `PEM`
##' @slot l a vector used in the lambda prior
##' @slot C_par a parameter used in the lambda prior,
##' according to Liu's paper, `C_par=2` is recommended.
##' @slot conditionalPEM is a conditional piecewise-exponential model used?
##' (default) Otherwise an unconditional model is used.
##'
##' @example examples/Model-class-DALogisticLogNormal.R
##' @export
##' @keywords classes
.DALogisticLogNormal <-
  setClass(Class="DALogisticLogNormal",
           representation(npiece="numeric",
                          l="numeric",
                          C_par="numeric",
                          conditionalPEM="logical"),
           prototype(npiece=3,
                     l=0.5,
                     C_par=2, #according to Liu's paper, C_par=2 is recommended
                     conditionalPEM=TRUE),
           contains="LogisticLogNormal",
           validity=
             function(object){
               o <-  Validate()

               o$check(all(object@l >= 0),
                       "l, prior parameter of lambda must be positive scalar")

               ## need to check C_par here.

               o$check(is.scalar(object@conditionalPEM))

               o$result()
             })


##' Initialization function for the `DALogisticLogNormal` class
##'
##' @param npiece see \code{\linkS4class{DALogisticLogNormal}}
##' @param l see \code{\linkS4class{DALogisticLogNormal}}
##' @param C_par see \code{\linkS4class{DALogisticLogNormal}}
##' @param conditionalPEM see \code{\linkS4class{DALogisticLogNormal}}
##' @param \dots additional parameters from \code{\link{LogisticLogNormal}}
##' @return the \code{\linkS4class{DALogisticLogNormal}} object
##'
##' @export
##' @keywords programming
DALogisticLogNormal <- function(npiece=3,
                                l,
                                C_par=2,
                                conditionalPEM=TRUE,
                                ...)
{
  start <- LogisticLogNormal(...)

  .DALogisticLogNormal(start,
                       npiece=safeInteger(npiece),
                       l=l,
                       C_par=C_par,
                       conditionalPEM=conditionalPEM,
                       datamodel=
                         function(){
                           # user should be able to specified time interval and number of pieces;
                           #time intervals of piecewise exponential distribution;
                           ## has this been done already?
                           for (i in 1:nObs) #for each patient
                           {
                             #  DLT[i] ~ dbern(p[i]) #Use y[i] and u[i] to represent DLT[i]
                             #  NOTE: In the original r2JAGs code, the notation "y[i]" was "event[i]" and "DLT[i]" was "y[i]";
                             #other notations: t[i]-- the true DLT time of patient i if he/she has DLT eventually;
                             #                 u[i]-- DLT free survival

                             #Part I: describe the logistic model of DLTs vs dose;
                             logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
                             StandLogDose[i] <- log(x[i] / ref_dose)

                             #Part II: describe the piecewise exponential;
                             #please notice:
                             #when y=1             -> DLT=1 and u=<T;
                             #when y=0 & T<t (u=T) -> DLT=0;
                             #when y=0 & T>t (u<T) -> DLT=NA/missing;

                             #when indx=0 -> censored, i.e u<T and event=0;
                             #when indx=1 -> not censored, i.e. u>=T or event=1;
                             indx[i]<-1-step(Tmax-u[i]-eps)*(1-y[i])

                             #a loop which goes through all pieces
                             for  (j in 1:npiece){
                               #When not censored, i.e DLT!=NA & t[i]=u[i];
                               #if t[i]<h[j], d[i,j]=0;
                               #if h[j]<t[i]=<h[j+1], d[i,j]=1
                               #if h[j+1]<t[i], d[i,j]=0

                               #When censored t[i]>u[i] -> d[i,j]=0
                               d[i,j]<-y[i]*step(u[i]-h[j]-eps)*step(h[j+1]-u[i])

                               #DLT free survival(time) for patient i in interval I(j);
                               #if t[i]<h[j], s[i,j]=0;
                               #if h[j]<t[i]<=h[j+1], s[i,j]=t[i]-h[j]
                               #if h[j+1]<=t[i], s[i,j]=h[j+1]-h[j]
                               s[i,j]<-min(u[i]-h[j],h[j+1]-h[j])*step(u[i]-h[j])

                               #piecewise exponential hazard rate lambda[j];
                               mu_u[i,j] <- lambda[j]*s[i,j]
                               mu[i,j]<- d[i,j]*log(lambda[j])-y[i]*mu_u[i,j]
                             }

                             #apply zero trick in JAGS;
                             zeros[i]~dpois(phi[i])

                             phi[i]<- -log(L[i]) + cadj

                             #the likelihood function;
                             L[i]<- pow(L_obs[i],indx[i])*pow(L_miss[i],1-indx[i])
                             #not censored
                             L_obs[i]<-exp(sum(mu[i,]))*pow(p[i]/A,y[i])*pow(1-p[i],1-y[i])
                             #censored
                             L_miss[i]<- 1-p[i]*(1-exp(-sum(mu_u[i,])))/A

                           }

                         },

                       priormodel=
                         h_jags_join_models(start@priormodel,
                                    function(){
                                      # ## the multivariate normal prior on the (transformed)
                                      # ## coefficients
                                      # priorPrec[1:2,1:2] <- inverse(priorCov[,])
                                      # theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2,1:2])
                                      # ## extract actual coefficients
                                      # alpha0 <- theta[1]
                                      # alpha1 <- exp(theta[2])
                                      #
                                      # ## dummy to use refDose here.
                                      # ## It is contained in the modelspecs list below,
                                      # ## so it must occur here
                                      # bla <- refDose + 1

                                      # dummies to use eps and cadj. Otherwise
                                      # empty data sampling fails.
                                      blu <- eps + cadj

                                      ## the piecewise exponential prior;
                                      g_beta<- 1/C_par

                                      for  (j in 1:npiece){

                                        muT[j]<- lambda[j]*sT[j]

                                        sT[j]<-h[j+1]-h[j]

                                        #prior of lambda ;

                                        g_alpha[j]<-l[j]/C_par

                                        lambda_p[j]~dgamma(g_alpha[j],g_beta)

                                        lambda[j]<-lambda_p[j]
                                      }

                                      ## for conditional:
                                      sum_muT <- sum(muT[])

                                      ## If cond = 1, then conditional PEM is used and this
                                      ## is defined as the probability to have DLT, i.e. t<T
                                      ## otherwise cond = 0 and it is just 1 (so no
                                      ## impact in likelihood)
                                      A <- cond * (1-exp(-sum_muT)) + (1 - cond)
                                    }),

                       datanames=c("nObs", "y", "x", "u", "Tmax"),
                       modelspecs=
                         function(nObs, Tmax){
                           list(ref_dose=start@ref_dose,
                                prec=start@params@prec,
                                mean=start@params@mean,
                                npiece=npiece,
                                l=l,
                                C_par=C_par,
                                zeros=rep(0, nObs),
                                eps=1e-10,
                                cadj=1e10,
                                h=seq(from=0L, to=Tmax, length=npiece + 1),
                                cond=as.integer(conditionalPEM) ## here pass the option to JAGS code
                           )
                         },
                       sample=
                         c("alpha0", "alpha1", "lambda")

  )
}
validObject(DALogisticLogNormal(mean=c(0, 1),
                                cov=diag(2),
                                ref_dose=1,
                                npiece=3,
                                l=0.5,
                                C_par=2))

# TITELogisticLogNormal ----

## ============================================================

##' Standard logistic model with bivariate (log) normal prior (for TITE-CRM use)
##'
##' This is a TITE-CRM based on a logistic regression model using
##' a bivariate normal prior on the intercept and log slope parameters.
##' This class inherits from the normal logistic model class.
##'
##' @slot weightMethod weight function method: linear or adaptive
##' (this was used in Liu, Yin and Yuan's paper)
##'
##' @export
##' @keywords classes
.TITELogisticLogNormal <-
  setClass(Class="TITELogisticLogNormal",
           representation(weightMethod="character"),
           prototype(weightMethod="linear"),
           contains="LogisticLogNormal",
           validity=
             function(object){
               o <- Validate()

               allweightMethod <- c("linear", "adaptive")

               o$check(all((object@weightMethod) %in%
                             allweightMethod),
                       "weightMethod must be either linear or adaptive")

               o$check(identical(length(object@weightMethod), 1L),
                       "weightMethod must have length 1")
             })


##' Initialization function for the `TITELogisticLogNormal` class
##'
##' @param weightMethod see \code{\linkS4class{TITELogisticLogNormal}}
##' @param \dots see \code{\linkS4class{LogisticLogNormal}}
##' @return the \code{\linkS4class{TITELogisticLogNormal}} object
##'
##' @export
##' @keywords methods
TITELogisticLogNormal <- function(weightMethod=c("linear", "adaptive"),
                                  ...)
{
  weightMethod <- match.arg(weightMethod)

  start <- LogisticLogNormal(...)

  .TITELogisticLogNormal(start,
                         weightMethod=weightMethod,
                         datamodel=
                           function(){
                             # here need to dummy use u and Tmax from the DataDA object
                             use_u <- u + 1
                             use_Tmax <- Tmax + 1

                             for (i in 1:nObs) #for each patient
                             {
                               #  DLT[i] ~ dbern(p[i]) #Use y[i] and u[i] to represent DLT[i]

                               #  NOTE: In the original r2JAGs code, the notation "y[i]" was "event[i]"
                               #        and "DLT[i]" was "y[i]";
                               #  Other notations: t[i]-- the true DLT time of patient i if he/she has
                               #                          DLT eventually;
                               #                   u[i]-- DLT free survival


                               #Part I: describe the logistic model of DLTs vs dose;

                               logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
                               StandLogDose[i] <- log(x[i] / ref_dose)

                               #Part II: describe the piecewise exponential;
                               #please notice:
                               #when y=1             -> DLT=1 and u=<T;
                               #when y=0 & T<t (u=T) -> DLT=0;
                               #when y=0 & T>t (u<T) -> DLT=NA/missing;

                               #when indx=0 -> censored, i.e u<T and event=0;
                               #when indx=1 -> not censored, i.e. u>=T or event=1;


                               #apply zero trick in JAGS;
                               zeros[i]~dpois(phi[i])
                               phi[i]<- -log(L[i]) + cadj

                               #the likelihood function;
                               L[i]<- pow(p[i],y[i])*pow((1-w[i]*p[i]),(1-y[i]))
                             }
                           },

                         datanames=c("nObs", "y", "x", "u", "Tmax"),

                         modelspecs=
                           function(nObs, u, Tmax, y){

                             ## calculate weight w based on the input data
                             if(length(u)){

                               if(weightMethod=="linear"){
                                 w<-u/Tmax
                               }
                               else if(weightMethod=="adaptive"){

                                 w <- NA
                                 support <- sort(u[y == 1])
                                 totalToxic <- sum(y)

                                 if (totalToxic) {
                                   for (i in 1:length(u)) {
                                     m <- length(support[support <= u[i]])
                                     if (!m){
                                       w[i] <- u[i]/support[1]/(totalToxic + 1)
                                     }

                                     else if (m == totalToxic){
                                       w[i] <- (totalToxic + (u[i] - support[totalToxic])/(Tmax+0.00000001 -
                                                                                             support[totalToxic]))/(totalToxic + 1)
                                     }

                                     else {
                                       w[i] <- (m + (u[i] - support[m])/(support[m + 1] - support[m]))/(totalToxic + 1)
                                     }
                                   }
                                 }
                                 else{
                                   w <- u/Tmax
                                 }
                               }
                             }
                             else{
                               w <- 1
                             }
                             w[y == 1] <- 1
                             w[u==Tmax] <- 1


                             list(ref_dose=start@ref_dose,
                                  prec=start@params@prec,
                                  mean=start@params@mean,
                                  zeros=rep(0, nObs),
                                  cadj=1e10,
                                  w=w)
                           })

}
validObject(TITELogisticLogNormal(mean=c(0, 1),
                                  cov=diag(2),
                                  ref_dose=1,
                                  weightMethod="linear"))

# OneParExpNormalPrior ----

## ============================================================

##' CRM Model with Skeleton Prior Probabilities
##'
##' This is a standard CRM with a normal prior on the log power parameter
##' for the skeleton prior probabilities.
##'
##' @slot skeletonFun function to calculate the prior DLT probabilities.
##' @slot skeletonProbs skeleton prior probabilities.
##' @slot sigma2 prior variance of log power parameter alpha.
##'
##' @export
##' @aliases OneParExpNormalPrior
##' @keywords classes
.OneParExpNormalPrior <-
  setClass(Class = "OneParExpNormalPrior",
           contains = "Model",
           representation(skeletonFun = "function",
                          skeletonProbs = "numeric",
                          sigma2 = "numeric"),
           validity=
             function(object){
               o <- Validate()

               o$check(identical(length(object@sigma2), 1L) &&
                         object@sigma2 > 0 && is.finite(object@sigma2),
                       "sigma2 must be a positive finite number")

               o$check(all(object@skeletonProbs >= 0 & object@skeletonProbs <= 1),
                       "skeletonProbs must be probabilities between 0 and 1")
             })

##' @describeIn OneParExpNormalPrior-class Initialization function for the
##'   `OneParExpNormalPrior` class.
##' @param skeletonProbs skeleton prior probabilities.
##' @param doseGrid dose grid.
##' @param sigma2 prior variance of log power parameter alpha.
##'
##' @export
##' @keywords methods
OneParExpNormalPrior <- function(skeletonProbs,
                                 doseGrid,
                                 sigma2)
{
  skeletonFun <- approxfun(x = doseGrid, y = skeletonProbs, rule = 2)
  invSkeletonFun <- approxfun(x = skeletonProbs, y = doseGrid, rule = 1)

  .OneParExpNormalPrior(
    skeletonFun = skeletonFun,
    skeletonProbs = skeletonProbs,
    sigma2 = sigma2,
    datamodel = function(){
      for (i in 1:nObs)
      {
        y[i] ~ dbern(p[i])
        p[i] <- skeletonProbs[xLevel[i]]^exp(alpha)
      }},
    datanames = c("nObs", "y", "xLevel"),
    prob = function(dose, alpha){ skeletonFun(dose)^exp(alpha) },
    dose = function(prob, alpha){ invSkeletonFun(prob^(1 / exp(alpha))) },
    priormodel = function(){ alpha ~ dnorm(0, 1 / sigma2) },
    modelspecs = function(){ list(skeletonProbs = skeletonProbs,
                                  sigma2 = sigma2) },
    init = function(){ list(alpha = 1) }, sample = "alpha")
}

data <- Data(doseGrid = c(1, 2, 3, 4, 5))
skeletonProbs <- seq(from = 0.1, to = 0.9, length = length(data@doseGrid))
validObject(OneParExpNormalPrior(
  skeletonProbs = skeletonProbs,
  doseGrid = data@doseGrid,
  sigma2 = 2
))

## ============================================================

##' Fractional CRM following paper and code by Guosheng Yin et al
##'
##' This is a fractional CRM model based on a one parameter CRM (with normal
##' prior on the log-power parameter) as well as Kaplan-Meier based estimation
##' of the conditional probability to experience a DLT for non-complete
##' observations.
##'
##' @export
##' @aliases FractionalCRM
##' @keywords classes
.FractionalCRM <-
  setClass(Class = "FractionalCRM",
           contains="OneParExpNormalPrior")

##' @describeIn FractionalCRM-class Initialization function for the fractional CRM.
##'   Takes the same arguments as [OneParExpNormalPrior()].
##' @param \dots inputs as in [OneParExpNormalPrior()].
##' @importFrom survival survfit Surv
##' @export
FractionalCRM <- function(...) {
  start <- OneParExpNormalPrior(...)

  .FractionalCRM(
    start,
    datamodel=
      # This is adapted from the TITELogisticLogNormal class.
      function(){
        # Note: here we need to dummy use stuff from the DataDA object.
        use_u <- u + 1
        use_Tmax <- Tmax + 1
        use_y <- y + 1

        for (i in 1:nObs) #for each patient
        {
          #  DLT[i] ~ dbern(p[i])  # but part of DLTs are fractions.
          p[i] <- skeletonProbs[xLevel[i]]^exp(alpha)

          #please notice:
          #when y=1             -> DLT=1 and u=<T;
          #when y=0 & T<t (u=T) -> DLT=0;
          #when y=0 & T>t (u<T) -> DLT=is a fraction
          # therefore not y directly is used but yhat, which is specified
          # in modelspecs below.

          #apply zero trick in JAGS;
          zeros[i] ~ dpois(phi[i])
          phi[i]<- -log(L[i]) + cadj

          #the likelihood function;
          L[i] <- pow(p[i], yhat[i]) * pow((1 - p[i]), (1 - yhat[i]))
        }
      },
    modelspecs=
      function(nObs, u, Tmax, y){

        ## calculate yhat based on the input data using the Kaplan-Meier method.
        if(length(u)){
          km   <- survival::survfit(survival::Surv(u, y) ~ 1)  ## K-M Method for Estimating Survival Probability
          stau <- km$surv[which.min(km$surv[km$time <= Tmax])] ## Calc the Survival Probability = S(Tmax)
          yhat <- y                                         ## initialize yhat with original y.
          for(k in seq_len(nObs)){
            if(u < Tmax && y[k] == 0){ ##  Within the Assessment Window and so far no DLT
              sTime   <- km$surv[which.min(km$surv[km$time <= u[k]])]
              yhat[k] <- (sTime - stau) / sTime                   ## Calculate the Fractional Contribution
            }
          }
        } else {
          yhat <- 1 # just for logistics we need this
        }

        list(
          skeletonProbs = start@skeletonProbs,
          sigma2 = start@sigma2,
          zeros = rep(0, nObs),
          cadj = 1e10,
          yhat = yhat
        )
      },
    datanames=c("nObs", "y", "xLevel", "u", "Tmax")
  )
}
validObject(FractionalCRM(
  skeletonProbs = c(0.1, 0.2, 0.3, 0.4),
  doseGrid = c(10, 30, 50, 100),
  sigma2 = 2
))

## ============================================================
# nolint end
