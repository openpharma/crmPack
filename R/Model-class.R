#' @include helpers.R
#' @include helpers_jags.R
#' @include Model-validity.R
#' @include ModelParams-class.R
#' @include CrmPackClass-class.R
NULL

# GeneralModel-class ----

#' `GeneralModel`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`GeneralModel`] is a general model class, from which all other specific
#' model-like classes inherit.
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
#'   Apart of data arguments, this function can be specified with one additional
#'   (optional) argument `from_prior` of type `logical` and length one. This
#'   `from_prior` flag can be used to differentiate the output of the `modelspecs`,
#'   as its value is taken directly from the `from_prior` argument of the `mcmc`
#'   method that invokes `modelspecs` function. That is, when `from_prior` is
#'   `TRUE`, then only `priormodel` JAGS model is used (`datamodel` is not used)
#'   by the `mcmc`, and hence `modelspecs` function should return all the parameters
#'   that are required by the `priormodel` only. If the value of `from_prior` is
#'   `FALSE`, then both JAGS models `datamodel` and `priormodel` are used in the
#'   MCMC sampler, and hence `modelspecs` function should return all the parameters
#'   required by both `datamodel` and `priormodel`.
#' @slot init (`function`)\cr a function computing the list of starting values
#'   for parameters required to be initialized in the MCMC sampler, based on the
#'   data slots that are required as arguments of this function.
#' @slot datanames (`character`)\cr the names of all data slots that are used
#'   by `datamodel` JAGS function. No other names should be specified here.
#' @slot datanames_prior (`character`)\cr the names of all data slots that are
#'   used by `priormodel` JAGS function. No other names should be specified here.
#' @slot sample (`character`)\cr names of all parameters from which you would
#'   like to save the MCMC samples.
#'
#' @seealso [`ModelPseudo`].
#'
#' @aliases GeneralModel
#' @export
#'
.GeneralModel <- setClass(
  Class = "GeneralModel",
  slots = c(
    datamodel = "function",
    priormodel = "function",
    modelspecs = "function",
    init = "function",
    datanames = "character",
    datanames_prior = "character",
    sample = "character"
  ),
  prototype = prototype(
    datamodel = I,
    priormodel = I,
    init = function() {
      list()
    }
  ),
  contains = "CrmPackClass",
  validity = v_general_model
)

## default constructor ----

#' @rdname GeneralModel-class
#' @note Typically, end users will not use the `.DefaultGeneralModel()` function.
#' @export
.DefaultGeneralModel <- function() {
  stop(paste0(
    "Class GeneralModel should not be instantiated directly.  Please use one of its subclasses instead."
  ))
}


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
#' @slot ref_dose (`positive_number`)\cr the reference dose.
#'
#' @seealso [`ModelParamsNormal`], [`LogisticNormal`], [`LogisticLogNormal`],
#'   [`LogisticLogNormalSub`], [`ProbitLogNormal`], [`ProbitLogNormalRel`].
#'
#' @aliases ModelLogNormal
#' @export
#'
.ModelLogNormal <- setClass(
  Class = "ModelLogNormal",
  contains = "GeneralModel",
  slots = c(
    params = "ModelParamsNormal",
    ref_dose = "positive_number"
  )
)

## constructor ----

#' @rdname ModelLogNormal-class
#'
#' @param mean (`numeric`)\cr the prior mean vector.
#' @param cov (`matrix`)\cr the prior covariance matrix. The precision matrix
#'   `prec` is internally calculated as an inverse of `cov`.
#' @param ref_dose (`number`)\cr the reference dose \eqn{x*} (strictly positive
#'   number).
#'
#' @export
#'
ModelLogNormal <- function(mean, cov, ref_dose = 1) {
  params <- ModelParamsNormal(mean, cov)
  .ModelLogNormal(
    params = params,
    ref_dose = positive_number(ref_dose),
    priormodel = function() {
      theta ~ dmnorm(mean, prec)
      alpha0 <- theta[1]
      alpha1 <- exp(theta[2])
    },
    modelspecs = function(from_prior) {
      ms <- list(mean = params@mean, prec = params@prec)
      if (!from_prior) {
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(theta = c(0, 1))
    },
    datanames = c("nObs", "y", "x"),
    sample = c("alpha0", "alpha1")
  )
}

## default constructor ----

#' @rdname ModelLogNormal-class
#' @note Typically, end users will not use the `.DefaultModelLogNormal()` function.
#' @export
.DefaultModelLogNormal <- function() {
  ModelLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
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
LogisticNormal <- function(mean, cov, ref_dose = 1) {
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

## default constructor ----

#' @rdname LogisticNormal-class
#' @note Typically, end users will not use the `.DefaultLogisticNormal()` function.
#' @export
.DefaultLogisticNormal <- function() {
  LogisticNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
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
#'   [`ProbitLogNormal`], [`ProbitLogNormalRel`], [`LogisticLogNormalMixture`],
#'   [`DALogisticLogNormal`].
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
LogisticLogNormal <- function(mean, cov, ref_dose = 1) {
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

## default constructor ----

#' @rdname LogisticLogNormal-class
#' @note Typically, end users will not use the `.DefaultLogisticLogNormal()` function.
#' @export
.DefaultLogisticLogNormal <- function() {
  LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
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
#' @slot params (`ModelParamsNormal`)\cr bivariate normal prior parameters.
#' @slot ref_dose (`number`)\cr the reference dose \eqn{x*}.
#'
#' @seealso [`LogisticNormal`], [`LogisticLogNormal`], [`ProbitLogNormal`],
#'   [`ProbitLogNormalRel`].
#'
#' @aliases LogisticLogNormalSub
#' @export
#'
.LogisticLogNormalSub <- setClass(
  Class = "LogisticLogNormalSub",
  slots = c(
    params = "ModelParamsNormal",
    ref_dose = "numeric"
  ),
  contains = "GeneralModel"
)

## constructor ----

#' @rdname LogisticLogNormalSub-class
#'
#' @param mean (`numeric`)\cr the prior mean vector.
#' @param cov (`matrix`)\cr the prior covariance matrix. The precision matrix
#'   `prec` is internally calculated as an inverse of `cov`.
#' @param ref_dose (`number`)\cr the reference dose \eqn{x*}.
#'
#' @export
#' @example examples/Model-class-LogisticLogNormalSub.R
#'
LogisticLogNormalSub <- function(mean, cov, ref_dose = 0) {
  params <- ModelParamsNormal(mean, cov)
  .LogisticLogNormalSub(
    params = params,
    ref_dose = ref_dose,
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- alpha0 + alpha1 * (x[i] - ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      theta ~ dmnorm(mean, prec)
      alpha0 <- theta[1]
      alpha1 <- exp(theta[2])
    },
    modelspecs = function(from_prior) {
      ms <- list(mean = params@mean, prec = params@prec)
      if (!from_prior) {
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(theta = c(0, -20))
    },
    datanames = c("nObs", "y", "x"),
    sample = c("alpha0", "alpha1")
  )
}


## default constructor ----

#' @rdname LogisticLogNormalSub-class
#' @note Typically, end-users will not use the `.DefaultLogisticLogNormalSub()` function.
#' @export
.DefaultLogisticLogNormalSub <- function() {
  LogisticLogNormalSub(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
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
#' @note The model used in the [`DualEndpoint`] classes is an extension of this model:
#'   `DualEndpoint` supports both `ProbitNormal` (which is not implemented yet) and
#'   `ProbitLogNormal` models through its `use_log_dose` slot.
#'   `ProbitLogNormal` has no such flag, so always uses `log(x/x*)`as a covariate in
#'   its model. Therefore this class can be used to check the prior assumptions on the
#'   dose-toxicity model, even when sampling from the prior distribution of the dual
#'   endpoint model is not possible, when `use_log_dose = TRUE` is used.
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
ProbitLogNormal <- function(mean, cov, ref_dose = 1) {
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

## default constructor ----

#' @rdname ProbitLogNormal-class
#' @note Typically, end users will not use the `.DefaultProbitLogNormal()` function.
#' @export
.DefaultProbitLogNormal <- function() {
  ProbitLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 7.2
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
ProbitLogNormalRel <- function(mean, cov, ref_dose = 1) {
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

## default constructor ----

#' @rdname ProbitLogNormalRel-class
#' @note Typically, end users will not use the `.DefaultProbitLogNormalRel()` function.
#' @export
.DefaultProbitLogNormalRel <- function() {
  ProbitLogNormalRel(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
  )
}

# LogisticLogNormalGrouped ----

## class ----

#' `LogisticLogNormalGrouped`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`LogisticLogNormalGrouped`] is the class for a logistic regression model
#'  for both the mono and the combo arms of the simultaneous dose escalation
#'  design.
#'
#' @details The continuous covariate is the natural logarithm of the dose \eqn{x} divided by
#'   the reference dose \eqn{x*} as in [`LogisticLogNormal`]. In addition,
#'   \eqn{I_c} is a binary indicator covariate which is 1 for the combo arm and 0 for the mono arm.
#'   The model is then defined as:
#'   \deqn{logit[p(x)] = (alpha0 + I_c * delta0) + (alpha1 + I_c * delta1) * log(x / x*),}
#'   where \eqn{p(x)} is the probability of observing a DLT for a given dose \eqn{x},
#'   and `delta0` and `delta1` are the differences in the combo arm compared to the mono intercept
#'   and slope parameters `alpha0` and `alpha1`.
#'   The prior is defined as \deqn{(alpha0, log(delta0), log(alpha1), log(delta1)) ~ Normal(mean, cov).}
#'
#' @seealso [`ModelLogNormal`], [`LogisticLogNormal`].
#'
#' @aliases LogisticLogNormalGrouped
#' @export
#'
.LogisticLogNormalGrouped <- setClass(
  Class = "LogisticLogNormalGrouped",
  contains = "ModelLogNormal"
)

## constructor ----

#' @rdname LogisticLogNormalGrouped-class
#'
#' @inheritParams ModelLogNormal
#'
#' @export
#' @example examples/Model-class-LogisticLogNormalGrouped.R
#'
LogisticLogNormalGrouped <- function(mean, cov, ref_dose = 1) {
  params <- ModelParamsNormal(mean, cov)
  .LogisticLogNormalGrouped(
    params = params,
    ref_dose = positive_number(ref_dose),
    priormodel = function() {
      theta ~ dmnorm(mean, prec)
      alpha0 <- theta[1]
      delta0 <- exp(theta[2])
      alpha1 <- exp(theta[3])
      delta1 <- exp(theta[4])
    },
    datamodel = function() {
      for (i in 1:nObs) {
        logit(p[i]) <- (alpha0 + is_combo[i] * delta0) +
          (alpha1 + is_combo[i] * delta1) * log(x[i] / ref_dose)
        y[i] ~ dbern(p[i])
      }
    },
    modelspecs = function(group, from_prior) {
      ms <- list(
        mean = params@mean,
        prec = params@prec
      )
      if (!from_prior) {
        ms$ref_dose <- ref_dose
        ms$is_combo <- as.integer(group == "combo")
      }
      ms
    },
    init = function() {
      list(theta = c(0, 1, 1, 1))
    },
    datanames = c("nObs", "y", "x"),
    sample = c("alpha0", "delta0", "alpha1", "delta1")
  )
}

## default constructor ----

#' @rdname LogisticLogNormalGrouped-class
#' @note Typically, end users will not use the `.DefaultLogisticLogNormalGrouped()` function.
#' @export
.DefaultLogisticLogNormalGrouped <- function() {
  LogisticLogNormalGrouped(
    mean = rep(0, 4),
    cov = diag(rep(1, 4)),
  )
}

# LogisticKadane ----

## class ----

#' `LogisticKadane`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticKadane`] is the class for the logistic model in the parametrization
#' of \insertCite{KadaneDickeyWinklerSmithPeters1980;textual}{crmPack}.
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
#' @references
#'   \insertAllCited{}
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
          (gamma *
            logit(rho0) -
            xmin * logit(theta) +
            x[i] * (logit(theta) - logit(rho0)))
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
    datanames = c("nObs", "y", "x"),
    sample = c("rho0", "gamma")
  )
}

## default constructor ----

#' @rdname LogisticKadane-class
#' @note Typically, end-users will not use the `.DefaultLogisticKadane()` function.
#' @export
.DefaultLogisticKadane <- function() {
  LogisticKadane(theta = 0.33, xmin = 1, xmax = 200)
}


# LogisticKadaneBetaGamma ----

## class ----

#' `LogisticKadaneBetaGamma`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`LogisticKadaneBetaGamma`] is the class for the logistic model in the parametrization
#' of \insertCite{KadaneDickeyWinklerSmithPeters1980;textual}{crmPack},
#' using a beta and a gamma distribution as the model priors.
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
#'   as in the `LogisticKadane` class. In addition, the shape parameters of the
#'   Beta prior distribution of `rho0` and the shape and rate parameters of the
#'   Gamma prior distribution of `gamma`, are required for creating the prior model.
#'
#' @slot theta (`proportion`)\cr the target toxicity probability.
#' @slot xmin (`number`)\cr the minimum of the dose range.
#' @slot xmax (`number`)\cr the maximum of the dose range.
#' @slot alpha (`number`)\cr the first shape parameter of the Beta prior distribution
#'   of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @slot beta (`number`)\cr the second shape parameter of the Beta prior distribution
#'   of `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @slot shape (`number`)\cr the shape parameter of the Gamma prior distribution
#'   of `gamma` the dose with target toxicity probability `theta`.
#' @slot rate (`number`)\cr the rate parameter of the Gamma prior distribution
#'   of `gamma` the dose with target toxicity probability `theta`.
#'
#' @seealso [`ModelLogNormal`], [`LogisticKadane`].
#'
#' @aliases LogisticKadaneBetaGamma
#' @export
#' @references
#'   \insertAllCited{}
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
#'   `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @param beta (`number`)\cr the second shape parameter of the Beta prior distribution
#'   `rho0 = p(xmin)` the probability of a DLT at the minimum dose `xmin`.
#' @param shape (`number`)\cr the shape parameter of the Gamma prior distribution
#'   `gamma` the dose with target toxicity probability `theta`.
#' @param rate (`number`)\cr the rate parameter of the Gamma prior distribution
#'   `gamma` the dose with target toxicity probability `theta`.
#'
#' @export
#' @example examples/Model-class-LogisticKadaneBetaGamma.R
#'
LogisticKadaneBetaGamma <- function(
  theta,
  xmin,
  xmax,
  alpha,
  beta,
  shape,
  rate
) {
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

## default constructor ----

#' @rdname LogisticKadaneBetaGamma-class
#' @note Typically, end users will not use the `.Default()` function.
#' @export
.DefaultLogisticKadaneBetaGamma <- function() {
  LogisticKadaneBetaGamma(
    theta = 0.3,
    xmin = 0,
    xmax = 7,
    alpha = 1,
    beta = 19,
    shape = 0.5625,
    rate = 0.125
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
#' @slot ref_dose (`positive_number`)\cr the reference dose.
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
#' @param ref_dose (`number`)\cr the reference dose \eqn{x*}
#'   (strictly positive number).
#'
#' @export
#' @example examples/Model-class-LogisticNormalMixture.R
#'
LogisticNormalMixture <- function(comp1, comp2, weightpar, ref_dose) {
  assert_number(ref_dose)

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
    modelspecs = function(from_prior) {
      ms <- list(
        mean = cbind(comp1@mean, comp2@mean),
        prec = array(data = c(comp1@prec, comp2@prec), dim = c(2, 2, 2)),
        weightpar = weightpar
      )
      if (!from_prior) {
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(theta = c(0, 1))
    },
    datanames = c("nObs", "y", "x"),
    sample = c("alpha0", "alpha1", "w")
  )
}

## default constructor ----

#' @rdname LogisticNormalMixture-class
#' @note Typically, end-users will not use the `.DefaultLogisticNormalMixture()` function.
#' @export
.DefaultLogisticNormalMixture <- function() {
  # nolint
  LogisticNormalMixture(
    comp1 = ModelParamsNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    ),
    comp2 = ModelParamsNormal(
      mean = c(1, 1.5),
      cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
    ),
    weightpar = c(a = 1, b = 1),
    ref_dose = 50
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
#' @slot ref_dose (`positive_number`)\cr the reference dose.
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
    log_normal = FALSE
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
#' @param ref_dose (`number`)\cr the reference dose \eqn{x*}
#'   (strictly positive number).
#' @param log_normal (`flag`)\cr should a log normal prior be specified, such
#'   that the mean vectors and covariance matrices are valid for the intercept
#'   and log slope?
#'
#' @export
#' @example examples/Model-class-LogisticNormalFixedMixture.R
#'
LogisticNormalFixedMixture <- function(
  components,
  weights,
  ref_dose,
  log_normal = FALSE
) {
  assert_numeric(weights)
  assert_number(ref_dose)
  assert_flag(log_normal)

  # Normalize weights to sum to 1.
  weights <- weights / sum(weights)

  .LogisticNormalFixedMixture(
    components = components,
    weights = weights,
    ref_dose = positive_number(ref_dose),
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
    modelspecs = function(from_prior) {
      ms <- list(
        weights = weights,
        mean = do.call(
          cbind,
          lapply(components, h_slots, "mean", simplify = TRUE)
        ),
        prec = array(
          do.call(c, lapply(components, h_slots, "prec", simplify = TRUE)),
          dim = c(2, 2, length(components))
        )
      )
      if (!from_prior) {
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(theta = c(0, 1))
    },
    datanames = c("nObs", "y", "x"),
    sample = c("alpha0", "alpha1")
  )
}

## default constructor ----

#' @rdname LogisticNormalFixedMixture-class
#' @note Typically, end-users will not use the `.DefaultLogisticNormalFixedMixture()`
#' function.
#' @export
.DefaultLogisticNormalFixedMixture <- function() {
  # nolint
  LogisticNormalFixedMixture(
    components = list(
      comp1 = ModelParamsNormal(
        mean = c(-0.85, 1),
        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
      ),
      comp2 = ModelParamsNormal(
        mean = c(1, 1.5),
        cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
      )
    ),
    weights = c(0.3, 0.7),
    ref_dose = 50
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
LogisticLogNormalMixture <- function(mean, cov, ref_dose, share_weight) {
  assert_number(ref_dose)

  params <- ModelParamsNormal(mean, cov)
  .LogisticLogNormalMixture(
    params = params,
    ref_dose = positive_number(ref_dose),
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
    modelspecs = function(from_prior) {
      ms <- list(
        cat_probs = c(1 - share_weight, share_weight),
        mean = params@mean,
        prec = params@prec
      )
      if (!from_prior) {
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(theta = matrix(c(0, 0, 1, 1), nrow = 2))
    },
    datanames = c("nObs", "y", "x", "nObsshare", "yshare", "xshare"),
    sample = c("alpha0", "alpha1", "comp")
  )
}

## default constructor ----

#' @rdname LogisticLogNormalMixture-class
#' @note Typically, end users will not use the `.DefaultLogNormalMixture()` function.
#' @export
.DefaultLogisticLogNormalMixture <- function() {
  # nolint
  LogisticLogNormalMixture(
    share_weight = 0.1,
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 50
  )
}

# DualEndpoint ----

## class ----

#' `DualEndpoint`
#'
#' @description `r lifecycle::badge("experimental")`
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
#' @slot ref_dose (`positive_number`)\cr for the probit toxicity model, the
#'   reference dose.
#' @slot use_log_dose (`flag`)\cr for the probit toxicity model, whether a log
#'   transformation of the (standardized) dose should be used?
#' @slot sigma2W (`numeric`)\cr the biomarker variance. Either a fixed value or
#'   Inverse-Gamma distribution parameters, i.e. vector with two elements named
#'   `a` and `b`.
#' @slot rho (`numeric`)\cr either a fixed value for the correlation
#'   (between `-1` and `1`), or a named vector with two elements named `a` and `b`
#'   for the Beta prior on the transformation `kappa = (rho + 1) / 2`, which is
#'   in `(0, 1)`. For example, `a = 1, b = 1` leads to a uniform prior on `rho`.
#' @slot use_fixed (`logical`)\cr indicates whether a fixed value for `sigma2W`
#'   or `rho` (for each parameter separately) is used or not. This slot is
#'   needed for internal purposes and must not be touched by the user.
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
    ref_dose = "positive_number",
    use_log_dose = "logical",
    sigma2W = "numeric",
    rho = "numeric",
    use_fixed = "logical"
  ),
  prototype = prototype(
    betaZ_params = ModelParamsNormal(mean = c(0, 1), cov = diag(2)),
    ref_dose = positive_number(1),
    use_log_dose = FALSE,
    sigma2W = 1,
    rho = 0,
    use_fixed = c(sigma2W = TRUE, rho = TRUE)
  ),
  contains = "GeneralModel",
  validity = v_model_dual_endpoint
)

## constructor ----

#' @rdname DualEndpoint-class
#'
#' @param mean (`numeric`)\cr for the probit toxicity model, the prior mean vector.
#' @param cov (`matrix`)\cr for the probit toxicity model, the prior covariance
#'   matrix. The precision matrix is internally calculated as an inverse of `cov`.
#' @param ref_dose (`number`)\cr for the probit toxicity model, the reference
#'   dose \eqn{x*} (strictly positive number).
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
DualEndpoint <- function(
  mean,
  cov,
  ref_dose = 1,
  use_log_dose = FALSE,
  sigma2W,
  rho
) {
  assert_number(ref_dose)
  assert_numeric(sigma2W, min.len = 1, max.len = 2)
  assert_numeric(rho, min.len = 1, max.len = 2)

  use_fixed <- c(
    sigma2W = test_number(sigma2W),
    rho = test_number(rho)
  )
  beta_z_params <- ModelParamsNormal(mean, cov)

  datamodel <- function() {
    for (i in 1:nObs) {
      # The toxicity model.
      stand_dose_temp[i] <- x[i] / ref_dose
      stand_dose[i] <- ifelse(
        use_log_dose,
        log(stand_dose_temp[i]),
        stand_dose_temp[i]
      )
      meanZ[i] <- betaZ[1] + betaZ[2] * stand_dose[i]
      z[i] ~ dnorm(meanZ[i], 1)
      y[i] ~ dinterval(z[i], 0)

      # The conditional biomarker model; betaW defined in subclasses!
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
    # Code for `precW` and `rho` will be added by
    # `h_model_dual_endpoint_sigma2w()`, `h_model_dual_endpoint_rho()` helpers, below.
    condPrecW <- precW / (1 - pow(rho, 2))
  }
  modelspecs_prior <- list(
    betaZ_mean = beta_z_params@mean,
    betaZ_prec = beta_z_params@prec
  )

  comp <- list(
    priormodel = priormodel,
    modelspecs = modelspecs_prior,
    init = NULL,
    sample = "betaZ"
  )

  # Update model components with regard to biomarker regression variance.
  comp <- h_model_dual_endpoint_sigma2w(
    use_fixed["sigma2W"],
    sigma2W = sigma2W,
    comp = comp
  )

  # Update model components with regard to DLT and biomarker correlation.
  comp <- h_model_dual_endpoint_rho(
    use_fixed["rho"],
    rho = rho,
    comp = comp
  )

  .DualEndpoint(
    betaZ_params = beta_z_params,
    ref_dose = positive_number(ref_dose),
    use_log_dose = use_log_dose,
    sigma2W = sigma2W,
    rho = rho,
    use_fixed = use_fixed,
    datamodel = datamodel,
    priormodel = comp$priormodel,
    modelspecs = function(from_prior) {
      if (!from_prior) {
        comp$modelspecs$ref_dose <- ref_dose
        comp$modelspecs$use_log_dose <- use_log_dose
      }
      comp$modelspecs
    },
    init = function(y) {
      c(comp$init, list(z = ifelse(y == 0, -1, 1), theta = c(0, 1)))
    },
    datanames = c("nObs", "w", "x", "xLevel", "y"),
    sample = comp$sample
  )
}

## default constructor ----

#' @rdname DualEndpoint-class
#' @note Typically, end users will not use the `.DefaultDualEndpoint()` function.
#' @export
.DefaultDualEndpoint <- function() {
  stop(paste0(
    "Class DualEndpoint cannot be instantiated directly.  Please use one of its subclasses instead."
  ))
}

# DualEndpointRW ----

## class ----

#' `DualEndpointRW`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DualEndpointRW`] is the class for the dual endpoint model with random walk
#' prior for biomarker.
#'
#'
#' @details This class extends the [`DualEndpoint`] class so that the dose-biomarker
#'   relationship \eqn{f(x)} is modelled by a non-parametric random walk of first
#'   or second order. That means, for the first order random walk we assume
#'   \deqn{betaW_i - betaW_i-1 ~ Normal(0, (x_i - x_i-1) * sigma2betaW),}
#'   where \eqn{betaW_i = f(x_i)} is the biomarker mean at the \eqn{i}-th dose
#'   gridpoint \eqn{x_i}.
#'   For the second order random walk, the second-order differences instead of
#'   the first-order differences of the biomarker means follow the normal distribution
#'   with \eqn{0} mean and \eqn{2 * (x_i - x_i-2) * sigma2betaW} variance.
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
#' @slot rw1 (`flag`)\cr for specifying the random walk prior on the biomarker
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
DualEndpointRW <- function(sigma2betaW, rw1 = TRUE, ...) {
  assert_numeric(sigma2betaW, min.len = 1, max.len = 2)
  assert_flag(rw1)

  start <- DualEndpoint(...)
  start@use_fixed["sigma2betaW"] <- length(sigma2betaW) == 1L

  priormodel <- if (rw1) {
    function() {
      # The 1st order differences.
      # Essentially dflat(), which is not available in JAGS.
      betaW[1] ~ dnorm(0, 0.000001)
      for (i in 2:nGrid) {
        delta[i - 1] ~ dnorm(0, precBetaW / (doseGrid[i] - doseGrid[i - 1]))
        betaW[i] <- betaW[i - 1] + delta[i - 1]
      }
    }
  } else {
    function() {
      # The 2nd order differences.
      delta[1] ~ dnorm(0, 0.000001)
      betaW[1] ~ dnorm(0, 0.000001)
      betaW[2] <- betaW[1] + delta[1]
      for (i in 3:nGrid) {
        # delta2: differences of the differences of betaW follow normal dist.
        delta2[i - 2] ~
          dnorm(0, 2 * precBetaW / (doseGrid[i] - doseGrid[i - 2]))
        delta[i - 1] <- delta[i - 2] + delta2[i - 2]
        betaW[i] <- betaW[i - 1] + delta[i - 1]
      }
    }
  }
  start@priormodel <- h_jags_join_models(start@priormodel, priormodel)
  start@datanames_prior <- c("nGrid", "doseGrid")
  start@sample <- c(start@sample, "betaW", "delta")

  # Update model components with regard to biomarker regression variance.
  start <- h_model_dual_endpoint_sigma2betaw(
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

## default constructor ----

#' @rdname DualEndpointRW-class
#' @note Typically, end users will not use the `.DefaultDualEndpointRW()` function.
#' @export
.DefaultDualEndpointRW <- function() {
  DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    sigma2betaW = 0.01,
    rw1 = TRUE
  )
}

# DualEndpointBeta ----

## class ----

#' `DualEndpointBeta`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DualEndpointBeta`] is the class for the dual endpoint model with beta
#' function for dose-biomarker relationship.
#'
#' @details This class extends the [`DualEndpoint`] class so that the dose-biomarker
#'   relationship \eqn{f(x)} is modelled by a parametric, rescaled beta density
#'   function:
#'   \deqn{f(x) = E0 + (Emax - E0) * Beta(delta1, delta2) * (x/x*)^{delta1} * (1 - x/x*)^{delta2},}
#'   where \eqn{x*} is the maximum dose (end of the dose range to be considered),
#'   \eqn{delta1} and \eqn{delta2} are the two beta function parameters, and
#'   \eqn{E0}, \eqn{Emax} are the minimum and maximum levels, respectively.
#'   For ease of interpretation, we use the parametrization based on \eqn{delta1}
#'   and the mode, where
#'   \deqn{mode = delta1 / (delta1 + delta2),}
#'   so that multiplying this by \eqn{x*} gives the mode on the dose grid.
#'
#'   All parameters can currently be assigned uniform distributions or be fixed
#'   in advance. Note that \code{E0} and \code{Emax} can have negative values or
#'   uniform distributions reaching into negative range, while \code{delta1} and
#'   \code{mode} must be positive or have uniform distributions in the positive
#'   range.
#'
#' @slot E0 (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @slot Emax (`numeric`)\cr either a fixed number or the two uniform
#'   distribution parameters.
#' @slot delta1 (`numeric`)\cr either a fixed positive number or the two
#'   parameters of the uniform distribution, that can take only positive values.
#' @slot mode (`numeric`)\cr either a fixed positive number or the two
#'   parameters of the uniform distribution, that can take only positive values.
#' @slot ref_dose_beta (`positive_number`)\cr the reference dose \eqn{x*} (note
#'   that this is different from the `ref_dose` in the inherited [`DualEndpoint`]
#'   model).
#'
#' @seealso [`DualEndpoint`], [`DualEndpointRW`], [`DualEndpointEmax`].
#'
#' @aliases DualEndpointBeta
#' @export
#'
.DualEndpointBeta <- setClass(
  Class = "DualEndpointBeta",
  slots = c(
    E0 = "numeric",
    Emax = "numeric",
    delta1 = "numeric",
    mode = "numeric",
    ref_dose_beta = "positive_number"
  ),
  prototype = prototype(
    E0 = c(0, 100),
    Emax = c(0, 500),
    delta1 = c(0, 5),
    mode = c(1, 15),
    ref_dose_beta = positive_number(1),
    use_fixed = c(
      sigma2W = TRUE,
      rho = TRUE,
      E0 = FALSE,
      Emax = FALSE,
      delta1 = FALSE,
      mode = FALSE
    )
  ),
  contains = "DualEndpoint",
  validity = v_model_dual_endpoint_beta
)

## constructor ----

#' @rdname DualEndpointBeta-class
#'
#' @param E0 (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @param Emax (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @param delta1 (`numeric`)\cr either a fixed positive number or the two parameters
#'   of the uniform distribution, that can take only positive values.
#' @param mode (`numeric`)\cr either a fixed positive number or the two parameters
#'   of the uniform distribution, that can take only positive values.
#' @param ref_dose_beta (`number`)\cr the reference dose \eqn{x*} (strictly
#'   positive number). Note that this is different from the `ref_dose` in the
#'   inherited [`DualEndpoint`] model).
#' @param ... parameters passed to [DualEndpoint()].
#'
#' @export
#' @example examples/Model-class-DualEndpointBeta.R
#'
DualEndpointBeta <- function(E0, Emax, delta1, mode, ref_dose_beta = 1, ...) {
  assert_numeric(E0, min.len = 1, max.len = 2)
  assert_numeric(Emax, min.len = 1, max.len = 2)
  assert_numeric(delta1, min.len = 1, max.len = 2)
  assert_numeric(mode, min.len = 1, max.len = 2)
  assert_number(ref_dose_beta)

  start <- DualEndpoint(...)

  ms <- start@modelspecs
  start@modelspecs <- function(from_prior) {
    c(list(ref_dose_beta = ref_dose_beta), ms(from_prior))
  }
  start@datanames_prior <- c("nGrid", "doseGrid")
  start@sample <- c(start@sample, "betaW")

  start <- h_model_dual_endpoint_beta(
    param = E0,
    param_name = "E0",
    priormodel = function() {
      E0 ~ dunif(E0_low, E0_high)
    },
    de = start
  )

  start <- h_model_dual_endpoint_beta(
    param = Emax,
    param_name = "Emax",
    priormodel = function() {
      Emax ~ dunif(Emax_low, Emax_high)
    },
    de = start
  )

  start <- h_model_dual_endpoint_beta(
    param = delta1,
    param_name = "delta1",
    priormodel = function() {
      delta1 ~ dunif(delta1_low, delta1_high)
    },
    de = start
  )

  start <- h_model_dual_endpoint_beta(
    param = mode,
    param_name = "mode",
    priormodel = function() {
      mode ~ dunif(mode_low, mode_high)
    },
    de = start
  )

  start@priormodel <- h_jags_join_models(
    start@priormodel,
    function() {
      # delta2 <- delta1 * (1 - (mode/ref_dose_beta)) / (mode/ref_dose_beta) # nolint
      delta2 <- delta1 * (ref_dose_beta / mode - 1)
      # betafun <- (delta1 + delta2)^(delta1 + delta2) * delta1^(- delta1) * delta2^(- delta2) # nolint
      betafun <- (1 + delta2 / delta1)^delta1 * (delta1 / delta2 + 1)^delta2
      for (i in 1:nGrid) {
        stand_dose_beta[i] <- doseGrid[i] / ref_dose_beta
        betaW[i] <- E0 +
          (Emax - E0) *
            betafun *
            stand_dose_beta[i]^delta1 *
            (1 - stand_dose_beta[i])^delta2
      }
    }
  )

  .DualEndpointBeta(
    start,
    E0 = E0,
    Emax = Emax,
    delta1 = delta1,
    mode = mode,
    ref_dose_beta = positive_number(ref_dose_beta)
  )
}

## default constructor ----

#' @rdname DualEndpointBeta-class
#' @note Typically, end users will not use the `.DefaultDualEndpointBeta()` function.
#' @export
.DefaultDualEndpointBeta <- function() {
  DualEndpointBeta(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    ref_dose = 10,
    use_log_dose = TRUE,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    E0 = c(0, 100),
    Emax = c(0, 500),
    delta1 = c(0, 5),
    mode = c(1, 15),
    ref_dose_beta = 1000
  )
}

# DualEndpointEmax ----

## class ----

#' `DualEndpointEmax`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DualEndpointEmax`] is the class for the dual endpoint model with `Emax`
#' function for dose-biomarker relationship.
#'
#' @details This class extends the [`DualEndpoint`] class so that the dose-biomarker
#'   relationship \eqn{f(x)} is modelled by a parametric `Emax` function:
#'   \deqn{f(x) = E0 + [(Emax - E0) * (x/x*)]/[ED50 + (x/x*)],}
#'   where \eqn{x*} is a reference dose, \eqn{E0} and \eqn{Emax} are the minimum
#'   and maximum levels for the biomarker, and \eqn{ED50} is the dose achieving
#'   half of the maximum effect \eqn{0.5 * Emax}.
#'   All parameters can currently be assigned uniform distributions or be fixed.
#'
#' @slot E0 (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @slot Emax (`numeric`)\cr either a fixed number or the two uniform
#'   distribution parameters.
#' @slot ED50 (`numeric`)\cr either a fixed number or the two uniform
#'   distribution parameters.
#' @slot ref_dose_emax (`positive_number`)\cr the reference dose \eqn{x*} (note
#'   that this is different from the `ref_dose` in the inherited [`DualEndpoint`]
#'   model).
#'
#' @seealso [`DualEndpoint`], [`DualEndpointRW`], [`DualEndpointBeta`].
#'
#' @aliases DualEndpointEmax
#' @export
#'
.DualEndpointEmax <- setClass(
  Class = "DualEndpointEmax",
  slots = c(
    E0 = "numeric",
    Emax = "numeric",
    ED50 = "numeric",
    ref_dose_emax = "numeric"
  ),
  prototype = prototype(
    E0 = c(0, 100),
    Emax = c(0, 500),
    ED50 = c(0, 500),
    ref_dose_emax = positive_number(1),
    use_fixed = c(
      sigma2W = TRUE,
      rho = TRUE,
      E0 = FALSE,
      Emax = FALSE,
      ED50 = FALSE
    )
  ),
  contains = "DualEndpoint",
  validity = v_model_dual_endpoint_emax
)

## constructor ----

#' @rdname DualEndpointEmax-class
#'
#' @param E0 (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @param Emax (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @param ED50 (`numeric`)\cr either a fixed number or the two uniform distribution
#'   parameters.
#' @param ref_dose_emax (`number`)\cr the reference dose \eqn{x*} (strictly
#'   positive number). Note that this is different from the `ref_dose` in the
#'   inherited [`DualEndpoint`] model).
#' @param ... parameters passed to [DualEndpoint()].
#'
#' @export
#' @example examples/Model-class-DualEndpointEmax.R
#'
DualEndpointEmax <- function(E0, Emax, ED50, ref_dose_emax = 1, ...) {
  assert_numeric(E0, min.len = 1, max.len = 2)
  assert_numeric(Emax, min.len = 1, max.len = 2)
  assert_numeric(ED50, min.len = 1, max.len = 2)
  assert_number(ref_dose_emax)

  start <- DualEndpoint(...)

  start@sample <- c(start@sample, "betaW")
  start@datanames_prior <- c("nGrid", "doseGrid")
  ms <- start@modelspecs
  start@modelspecs <- function(from_prior) {
    c(list(ref_dose_emax = ref_dose_emax), ms(from_prior))
  }

  start <- h_model_dual_endpoint_beta(
    param = E0,
    param_name = "E0",
    priormodel = function() {
      E0 ~ dunif(E0_low, E0_high)
    },
    de = start
  )

  start <- h_model_dual_endpoint_beta(
    param = Emax,
    param_name = "Emax",
    priormodel = function() {
      Emax ~ dunif(Emax_low, Emax_high)
    },
    de = start
  )

  start <- h_model_dual_endpoint_beta(
    param = ED50,
    param_name = "ED50",
    priormodel = function() {
      ED50 ~ dunif(ED50_low, ED50_high)
    },
    de = start
  )

  start@priormodel <- h_jags_join_models(
    start@priormodel,
    function() {
      for (i in 1:nGrid) {
        stand_dose_emax[i] <- doseGrid[i] / ref_dose_emax
        betaW[i] <- E0 +
          (Emax - E0) * stand_dose_emax[i] / (ED50 + stand_dose_emax[i])
      }
    }
  )

  .DualEndpointEmax(
    start,
    E0 = E0,
    Emax = Emax,
    ED50 = ED50,
    ref_dose_emax = positive_number(ref_dose_emax)
  )
}

## default constructor ----

#' @rdname DualEndpointEmax-class
#' @note Typically, end users will not use the `.DefaultDualEndpointEmax()` function.
#' @export
.DefaultDualEndpointEmax <- function() {
  DualEndpointEmax(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    E0 = c(0, 100),
    Emax = c(0, 500),
    ED50 = c(10, 200),
    ref_dose_emax = 1000
  )
}

# ModelPseudo ----

## class ----

#' `ModelPseudo`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ModelPseudo`] is the parent class for models that express their prior in
#' the form of pseudo data (as if there is some data before the trial starts).
#'
#' @seealso [`GeneralModel`].
#'
#' @aliases ModelPseudo
#' @export
#'
.ModelPseudo <- setClass(
  Class = "ModelPseudo",
  contains = "CrmPackClass"
)

## default constructor ----

#' @rdname ModelPseudo-class
#' @note Typically, end users will not use the `.DefaultModelPseudo()` function.
#' @export
.DefaultModelPseudo <- function() {
  stop(paste0(
    "Class ModelPseudo should not be instantiated directly.  Please use one of its subclasses instead."
  ))
}

# ModelTox ----

## class ----

#' `ModelTox`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ModelTox`] is the parent class for DLE (dose-limiting events) models using
#' pseudo data prior. It is dedicated for DLE models or toxicity models that
#' have their prior specified in the form of pseudo data (as if there is some
#' data before the trial starts).
#'
#' The `data` must obey the convention of the [`Data`] class. This refers to any
#' observed DLE responses (`y` in [`Data`]), the dose levels (`x` in [`Data`])
#' at which these responses are observed, all dose levels considered in the
#' study (`doseGrid` in [`Data`]), and finally other specifications in [`Data`]
#' class that can be used to generate prior or posterior modal estimates or
#' samples estimates for model parameter(s).
#' If no responses are observed, at least `doseGrid` has to be specified
#' in `data` for which prior modal estimates or samples can be obtained for
#' model parameters based on the specified pseudo data.
#'
#' @slot data (`Data`)\cr observed data that is used to obtain model parameters
#'   estimates or samples (see details above).
#'
#' @seealso [`ModelEff`].
#'
#' @aliases ModelTox
#' @export
#'
.ModelTox <- setClass(
  Class = "ModelTox",
  slots = c(
    data = "Data"
  ),
  contains = "ModelPseudo"
)

## default constructor ----

#' @rdname ModelTox-class
#' @note Typically, end users will not use the `.DefaultModelTox()` function.
#' @export
.DefaultModelTox <- function() {
  stop(paste0(
    "Class ModelTox should not be instantiated directly.  Please use one of its subclasses instead."
  ))
}

# ModelEff ----

## class ----

#' `ModelEff`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`ModelEff`] is the parent class for efficacy models using pseudo data prior.
#' It is dedicated all efficacy models that have their prior specified in the
#' form of pseudo data (as if there is some data before the trial starts).
#'
#' The `data` must obey the convention of the [`DataDual`] class. This refers to
#' any observed efficacy/biomarker responses (`w` in [`DataDual`]), the dose
#' levels at which these responses are observed (`x` in [`DataDual`]), all dose
#' levels considered in the study (`doseGrid` in [`DataDual`]), and finally
#' other specifications in [`DataDual`] class that can be used to generate prior
#' or posterior modal estimates or samples estimates for model parameter(s).
#' If no responses are observed, at least `doseGrid` has to be specified
#' in `data` for which prior modal estimates or samples can be obtained for
#' model parameters based on the specified pseudo data.
#'
#' @slot data (`DataDual`)\cr observed data that is used to obtain model
#'   parameters estimates or samples (see details above).
#'
#' @seealso [`ModelTox`].
#'
#' @aliases ModelEff
#' @export
#'
.ModelEff <- setClass(
  Class = "ModelEff",
  slots = c(
    data = "DataDual"
  ),
  contains = "ModelPseudo"
)

## default constructor ----

#' @rdname ModelEff-class
#' @note Typically, end users will not use the `.DefaultModelEff()` function.
#' @export
.DefaultModelEff <- function() {
  stop(paste0(
    "Class ModelEff should not be instantiated directly.  Please use one of its subclasses instead."
  ))
}

# LogisticIndepBeta ----

## class ----

#' `LogisticIndepBeta`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`LogisticIndepBeta`] is the class for the two-parameters logistic regression
#' dose-limiting events (DLE) model with prior expressed in form of pseudo data.
#' This model describes the relationship between the binary DLE responses
#' and the dose levels. More specifically, it represents the relationship of the
#' probabilities of the occurrence of a DLE for corresponding dose levels in log
#' scale. This model is specified as
#' \deqn{p(x) = exp(phi1 + phi2 * log(x)) / (1 + exp(phi1 + phi2 * log(x)))}
#' where \eqn{p(x)} is the probability of the occurrence of a DLE at dose \eqn{x}.
#' The two parameters of this model are the intercept \eqn{phi1} and the slope
#' \eqn{phi2}. The `LogisticIndepBeta` inherits all slots from [`ModelTox`] class.
#'
#' In the context of pseudo data, the following three arguments are used,
#' `binDLE`, `DLEdose` and `DLEweights`. The `DLEdose` represents fixed dose
#' levels at which the pseudo DLE responses `binDLE` are observed. `DLEweights`
#' represents total number of subjects treated per each dose level in `DLEdose`.
#' The `binDLE` represents the number of subjects observed with DLE per each
#' dose level in `DLEdose`. Hence, all these three vectors must be of the same
#' length and the order of the elements in any of the vectors `binDLE`,
#' `DLEdose` and `DLEweights` must be kept, so that an element of a given vector
#' corresponds to the elements of the remaining two vectors (see the example for
#' more insight).
#' Finally, since at least two DLE pseudo responses are needed to
#' obtain prior modal estimates (same as the maximum likelihood estimates) for
#' the model parameters, the `binDLE`, `DLEdose` and `DLEweights` must all be
#' vectors of at least length 2.
#'
#' @details The pseudo data can be interpreted as if we obtain some observations
#'   before the trial starts. It can be used to express our prior, i.e. the
#'   initial beliefs for the model parameters. The pseudo data is expressed in
#'   the following way. First, fix at least two dose levels, then ask for experts'
#'   opinion on how many subjects are to be treated at each of these dose levels
#'   and on the number of subjects observed with a DLE. At each dose level, the
#'   number of subjects observed with a DLE, divided by the total number of
#'   subjects treated, is the probability of the occurrence of a DLE at that
#'   particular dose level. The probabilities of the occurrence of a DLE based
#'   on this pseudo data are independent and they follow Beta distributions.
#'   Therefore, the joint prior probability density function of all these
#'   probabilities can be obtained. Hence, by a change of variable, the joint
#'   prior probability density function of the two parameters in this model can
#'   also be obtained. In addition, a conjugate joint prior density function of
#'   the two parameters in the model is used. For details about the form of all
#'   these joint prior and posterior probability density functions, please refer
#'   to \insertCite{WhiteheadWilliamson1998;textual}{crmPack}.
#'
#' @slot binDLE (`numeric`)\cr a vector of total numbers of DLE responses.
#'   It must be at least of length 2 and the order of its elements must
#'   correspond to values specified in `DLEdose` and `DLEweights`.
#' @slot DLEdose (`numeric`)\cr a vector of the dose levels corresponding to
#'   It must be at least of length 2 and the order of its elements must
#'   correspond to values specified in `binDLE` and `DLEweights`.
#' @slot DLEweights (`integer`)\cr total number of subjects treated at each of
#'   the pseudo dose level `DLEdose`.
#'   It must be at least of length 2 and the order of its elements must
#'   correspond to values specified in `binDLE` and `DLEdose`.
#' @slot phi1 (`number`)\cr  the intercept of the model. This slot is used in
#'   output to display the resulting prior or posterior modal estimate of the
#'   intercept obtained based on the pseudo data and (if any) observed data/responses.
#' @slot phi2 (`number`)\cr  the slope of the model. This slot is used in output
#'   to display the resulting prior or posterior modal estimate of the slope
#'   obtained based on the pseudo data and (if any) the observed data/responses.
#' @slot Pcov (`matrix`)\cr refers to the 2x2 covariance matrix of the intercept
#'   (\eqn{phi1}) and the slope parameters (\eqn{phi2}) of the model.
#'   This is used in output to display the resulting prior and posterior
#'   covariance matrix of \eqn{phi1} and \eqn{phi2} obtained, based on the
#'   pseudo data and (if any) the observed data and responses. This slot is
#'   needed for internal purposes.
#'
#' @aliases LogisticIndepBeta
#' @export
#' @references
#'   \insertAllCited{}
#'
.LogisticIndepBeta <- setClass(
  Class = "LogisticIndepBeta",
  slots = c(
    binDLE = "numeric",
    DLEdose = "numeric",
    DLEweights = "integer",
    phi1 = "numeric",
    phi2 = "numeric",
    Pcov = "matrix"
  ),
  prototype = prototype(
    binDLE = c(0, 0),
    DLEdose = c(1, 1),
    DLEweights = c(1L, 1L)
  ),
  contains = "ModelTox",
  validity = v_model_logistic_indep_beta
)

## constructor ----

#' @rdname LogisticIndepBeta-class
#'
#' @param binDLE (`numeric`)\cr the number of subjects observed with a DLE, the
#'   pseudo DLE responses, depending on dose levels `DLEdose`.
#'   Elements of `binDLE` must correspond to the elements of `DLEdose` and
#'   `DLEweights`.
#' @param DLEdose (`numeric`)\cr dose levels for the pseudo DLE responses.
#'   Elements of `DLEdose` must correspond to the elements of `binDLE` and
#'   `DLEweights`.
#' @param DLEweights (`numeric`)\cr the total number of subjects treated at each
#'   of the dose levels `DLEdose`, pseudo weights.
#'   Elements of `DLEweights` must correspond to the elements of `binDLE` and
#'   `DLEdose`.
#' @param data (`Data`)\cr the input data to update estimates of the model
#'   parameters.
#'
#' @export
#' @example examples/Model-class-LogisticIndepBeta.R
#'
LogisticIndepBeta <- function(binDLE, DLEdose, DLEweights, data) {
  assert_numeric(binDLE)
  assert_numeric(DLEdose)
  assert_integerish(DLEweights, lower = 0, any.missing = FALSE)
  assert_class(data, "Data")

  # Combine pseudo and observed data. It can also happen that data@nObs == 0.
  y <- c(binDLE, data@y)
  x <- c(DLEdose, data@x)
  w <- c(DLEweights, rep(1, data@nObs))

  fit_dle <- suppressWarnings(
    glm(y / w ~ log(x), family = binomial(link = "logit"), weights = w)
  )
  phi1 <- coef(fit_dle)[["(Intercept)"]]
  phi2 <- coef(fit_dle)[["log(x)"]]
  Pcov <- vcov(fit_dle)

  .LogisticIndepBeta(
    binDLE = binDLE,
    DLEdose = DLEdose,
    DLEweights = as.integer(DLEweights),
    phi1 = phi1,
    phi2 = phi2,
    Pcov = Pcov,
    data = data
  )
}

## default constructor ----

#' @rdname LogisticIndepBeta-class
#' @note Typically, end users will not use the `.DefaultLogisticIndepBeta()` function.
#' @export
.DefaultLogisticIndepBeta <- function() {
  my_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3L, 3L),
    DLEdose = c(25, 300),
    data = Data(doseGrid = seq(25, 300, 25))
  )
}


# Effloglog ----

## class ----

#' `Effloglog`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Effloglog`] is the class for the linear log-log efficacy model using pseudo
#' data prior. It describes the relationship between continuous efficacy
#' responses and corresponding dose levels in log-log scale. This efficacy
#' log-log model is given as
#' \deqn{y_i = theta1 + theta2 * log(log(x_i)) + epsilon_i,}
#' where \eqn{y_i} is the efficacy response for subject \eqn{i}, \eqn{x_i} is
#' the dose level treated for subject \eqn{i} and \eqn{epsilon_i} is the random
#' error term of efficacy model at subject \eqn{i}. The error term
#' \eqn{epsilon_i} is a random variable that follows normal distribution with
#' mean \eqn{0} and variance \eqn{nu^{-1}}, which is assumed to be the
#' same for all subjects.
#' There are three parameters in this model, the intercept \eqn{theta1}, the
#' slope \eqn{theta2} and the precision \eqn{nu} of the efficacy responses, also
#' known as the inverse of the variance of the pseudo efficacy responses. It can
#' be a fixed constant or having a gamma distribution. Therefore, a single scalar
#' value or a vector with two positive numbers values must be specified for `nu`
#' slot. If there are some observed efficacy responses available, in the output,
#' `nu` will display the updated value of the precision or the updated values
#' for the parameters of the gamma distribution.
#' The `Effloglog` inherits all slots from [`ModelEff`] class.
#'
#' @details The prior of this model is specified in form of pseudo data. First,
#'   at least two dose levels are fixed. Then, using e.g. experts' opinion, the
#'   efficacy values that correspond to these dose levels can be obtained,
#'   The `eff` and `eff_dose` arguments represent the prior in form of the pseudo
#'   data. The `eff` represents the pseudo efficacy values. The `eff_dose`
#'   represents the dose levels at which these pseudo efficacy values are
#'   observed. Hence, the positions of the elements specified in `eff` and
#'   `eff_dose` must correspond to each other between these vectors.
#'   Since at least 2 pseudo efficacy values are needed to obtain modal
#'   estimates of the intercept and slope parameters, both `eff` and `eff_dose`
#'   must be vectors of length at least 2.
#'
#'   The joint prior distribution of the intercept \eqn{theta1} and the slope
#'   \eqn{theta2} of this model follows bivariate normal distribution with mean
#'   \eqn{mu} and covariance matrix \eqn{(nu * Q)^{-1}}.
#'   The mean \eqn{mu} is a \eqn{2 x 1} column vector that contains the prior
#'   modal estimates of the intercept and the slope.
#'   Scalar \eqn{nu} is the precision of the pseudo efficacy responses and
#'   \eqn{Q} is the prior or posterior (given that observed, no DLT data is
#'   available) precision matrix.
#'   It is specified as \eqn{Q = X0^T * X0 + X^T * X}, where \eqn{X0} is a
#'   design matrix that is based on pseudo dose levels only, and \eqn{X} is a
#'   design matrix that is based on dose levels corresponding to the no DLT
#'   efficacy responses observed only (if any).
#'   Hence, the \eqn{X0} (or \eqn{X}) will be of size \eqn{r x 2}, if
#'   there are \eqn{r >= 2} pseudo efficacy responses specified (or
#'   if there are \eqn{r} no DLT efficacy responses observed in the `data`).
#'
#' @slot eff (`numeric`)\cr the pseudo efficacy responses. Each element here
#'   must represent responses treated based on one subject.
#'   It must be a vector of length at least 2 and the order of its elements must
#'   correspond to values specified in `eff_dose`.
#' @slot eff_dose (`numeric`)\cr the pseudo efficacy dose levels at which the
#'   pseudo efficacy responses are observed.
#'   It must be a vector of length at least 2 and the order of its elements must
#'   correspond to values specified in `eff`.
#' @slot nu (`numeric`)\cr parameter of the prior precision of pseudo efficacy
#'   responses. This is either a fixed value or a named vector with two positive
#'   numbers, the shape (`a`), and the rate (`b`) parameters for the gamma
#'   distribution.
#' @slot use_fixed (`flag`)\cr indicates whether `nu` specified is a fixed value
#'   or a vector with two parameters for gamma distribution. This slot is for
#'   internal purposes only and must not be used by the user.
#' @slot theta1 (`number`)\cr the intercept in this efficacy log-log model. This
#'   slot is used in output to display the resulting prior or posterior modal
#'   estimates obtained based on the pseudo and observed (if any) data.
#' @slot theta2 (`number`)\cr the slope in this efficacy log-log model. This
#'   slot is used in output to display the resulting prior or posterior modal
#'   estimates obtained based on the pseudo and observed (if any) data.
#' @slot Pcov (`matrix`)\cr refers to the \eqn{2 x 2} covariance matrix of the
#'   estimators of the intercept \eqn{theta1} and the slope \eqn{theta2}
#'   parameters in this model.
#'   This is used in output to display the resulting prior and posterior
#'   covariance matrix of \eqn{theta1} and \eqn{theta2} obtained, based on the
#'   pseudo and observed (if any) data. This slot is needed for internal purposes.
#' @slot X (`matrix`)\cr is the design matrix that is based on either the pseudo
#'   dose levels or observed dose levels (without DLT). This is used
#'   in the output to display the design matrix for the pseudo or the observed
#'   efficacy responses.
#' @slot Y (`numeric`)\cr is a vector that either contains the pseudo efficacy
#'   responses or observed efficacy responses (without DLT).
#' @slot mu (`numeric`)\cr a vector of the prior or the posterior modal estimates
#'   of the intercept (\eqn{theta1}) and the slope (\eqn{theta2}).
#'   This slot is used in output to display as the mean of the prior or posterior
#'   bivariate normal distribution for \eqn{theta1} and \eqn{theta2}.
#' @slot Q (`matrix`)\cr is the prior or posterior (given that observed, no DLT
#'   data is available) precision matrix. It is specified as
#'   \eqn{Q = X0^T * X0 + X^T * X}, where \eqn{X0} is a design matrix that is
#'   based on pseudo dose levels only, and \eqn{X} is a design matrix that is
#'   based on dose levels corresponding to the observed, no DLT efficacy values
#'   only (if any).
#' @slot const (`number`)\cr a non-negative number (default to 0), leading to the
#'   model form described above. In general, the model has the form
#'   \eqn{y_i = theta1 + theta2 * log(log(x_i + const)) + epsilon_i}, such that
#'   dose levels greater than \eqn{1 - const} can be considered as described in
#'   \insertCite{YeungWhiteheadReignerBeyerDiackJaki2015;textual}{crmPack}.
#'
#' @aliases Effloglog
#' @export
#' @references
#'   \insertAllCited{}
#'
.Effloglog <- setClass(
  Class = "Effloglog",
  slots = c(
    eff = "numeric",
    eff_dose = "numeric",
    nu = "numeric",
    use_fixed = "logical",
    theta1 = "numeric",
    theta2 = "numeric",
    Pcov = "matrix",
    X = "matrix",
    Y = "numeric",
    mu = "numeric",
    Q = "matrix",
    const = "numeric"
  ),
  prototype = prototype(
    eff = c(0, 0),
    eff_dose = c(1, 1),
    nu = 1 / 0.025,
    use_fixed = TRUE,
    const = 0
  ),
  contains = "ModelEff",
  validity = v_model_eff_log_log
)

## constructor ----

#' @rdname Effloglog-class
#'
#' @param eff (`numeric`)\cr the pseudo efficacy responses.
#'   Elements of `eff` must correspond to the elements of `eff_dose`.
#' @param eff_dose (`numeric`)\cr dose levels that correspond to pseudo efficacy
#'   responses in `eff`.
#' @param nu (`numeric`)\cr the precision (inverse of the variance) of the
#'   efficacy responses. This is either a fixed value or a named vector with two
#'   positive numbers, the shape (`a`), and the rate (`b`) parameters for the
#'   gamma distribution.
#' @param data (`DataDual`)\cr observed data to update estimates of the model
#'   parameters.
#' @param const (`number`)\cr the constant value added to the dose level when
#'   the dose level value is less than or equal to 1 and a special form of the
#'   linear log-log has to be applied
#'   \insertCite{YeungWhiteheadReignerBeyerDiackJaki2015}{crmPack}.
#'
#' @export
#' @example examples/Model-class-Effloglog.R
#'
Effloglog <- function(eff, eff_dose, nu, data, const = 0) {
  assert_numeric(eff)
  assert_numeric(eff_dose, len = length(eff))
  assert_numeric(nu, min.len = 1, max.len = 2)
  assert_class(data, "Data")
  assert_number(const, finite = TRUE)

  use_fixed <- length(nu) == 1L

  eff_dose <- eff_dose + const
  # Get observed efficacy data without DLT (if any).
  eff_obsrv_w_x <- getEff(data, no_dlt = TRUE)
  eff_obsrv <- eff_obsrv_w_x$w_no_dlt
  eff_obsrv_dose <- eff_obsrv_w_x$x_no_dlt + const

  # Fit pseudo and observed (if any) efficacy.
  w <- c(eff, eff_obsrv)
  x <- c(eff_dose, eff_obsrv_dose)
  fit_eff <- suppressWarnings(lm(w ~ log(log(x))))
  X <- model.matrix(fit_eff)
  Y <- w
  mu <- coef(fit_eff) # This is [theta1, theta2]^T est.
  Q <- crossprod(X)
  Pcov <- vcov(fit_eff)

  nobs_no_dlt <- length(eff_obsrv)
  if (nobs_no_dlt > 0L) {
    # Observed data available.
    # Set X, Y to observed data only.
    X <- model.matrix(fit_eff)[-seq_along(eff), ]
    Y <- eff_obsrv

    fit_eff0 <- lm(eff ~ log(log(eff_dose))) # Pseudo only.
    X0 <- model.matrix(fit_eff0)
    mu0 <- coef(fit_eff0)
    Q0 <- crossprod(X0)
    # Note that mu = (Q0 + X^T * X)^{-1} * (Q0 * mu0 + X^T * X * (X^T * X)^{-1} X^T * Y),
    # given that (X^T * X) is invertible and X, Y, mu0, Q0, are specified in this else block.
    if (!use_fixed) {
      nu["a"] <- nu["a"] + (nobs_no_dlt) / 2
      nu["b"] <- nu["b"] +
        (crossprod(Y) + t(mu0) %*% Q0 %*% mu0 - t(mu) %*% Q %*% mu) / 2
    }
  }

  .Effloglog(
    eff = eff,
    eff_dose = eff_dose,
    nu = nu,
    use_fixed = use_fixed,
    theta1 = mu[["(Intercept)"]],
    theta2 = mu[["log(log(x))"]],
    Pcov = Pcov,
    X = X,
    Y = Y,
    mu = as.vector(mu),
    Q = Q,
    const = const,
    data = data
  )
}

## default constructor ----

#' @rdname Effloglog-class
#' @note Typically, end users will not use the `.DefaultEffloglog()` function.
#' @export
.DefaultEffloglog <- function() {
  emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

  my_data <- DataDual(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = emptydata@doseGrid,
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3, 4, 4, 5, 6))
  )

  Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = my_data
  )
}

# EffFlexi ----

## class ----

#' `EffFlexi`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`EffFlexi`] is the class for the efficacy model in flexible form of prior
#' expressed in form of pseudo data. In this class, a flexible form is used to
#' describe the relationship between the efficacy responses and the dose levels
#' and it is specified as
#' \deqn{(W | betaW, sigma2W) ~ Normal(X * betaW, sigma2W * I),}
#' where \eqn{W} is a vector of the efficacy responses, \eqn{betaW} is a column
#' vector of the mean efficacy responses for all dose levels, and \eqn{X} is
#' the design matrix with entries \eqn{I_i,j} that are equal to 1 if subject
#' \eqn{i} is allocated to dose \eqn{j}, and \eqn{0} otherwise. The \eqn{sigma2W}
#' is the variance of the efficacy responses which can be either a fixed number
#' or a number from an inverse gamma distribution.
#' This flexible form aims to capture different shapes of the dose-efficacy
#' curve. In addition, the first (RW1) or second order (RW2) random walk model
#' can be used for smoothing data. That is the random walk model is used to model
#' the first or the second order differences of the mean efficacy responses to
#' its neighboring dose levels of their mean efficacy responses.
#'
#' The RW1 model is given as
#' \deqn{betaW_j - betaW_j-1) ~ Normal(0, sigma2betaW),}
#' and for RW2 as
#' \deqn{betaW_j-2 - 2 * betaW_j-1 + beta_j ~ Normal(0, sigma2betaW),}
#' where \eqn{betaW_j} is the vector of mean efficacy responses at dose j, and
#' the \eqn{sigma2betaW} is the prior variance which can be either a fixed
#' number or a number from an inverse gamma distribution.
#'
#' The `eff` and `eff_dose` are the pseudo efficacy responses and dose levels at
#' which these pseudo efficacy responses are observed. Both, `eff` and `eff_dose`
#' must be vectors of length at least 2. The positions of the elements specified
#' in `eff` and `eff_dose` must correspond to each other between these vectors.
#'
#' @details This model will output the updated value or the updated values of the
#'   parameters of the inverse gamma distributions for \eqn{sigma2W} and
#'   \eqn{sigma2betaW}. The `EffFlexi` inherits all slots from [`ModelEff`] class.
#'
#' @slot eff (`numeric`)\cr the pseudo efficacy responses. Each element here
#'   must represent responses treated based on one subject.
#'   It must be a vector of length at least 2 and the order of its elements must
#'   correspond to values specified in `eff_dose`.
#' @slot eff_dose (`numeric`)\cr the pseudo efficacy dose levels at which the
#'   pseudo efficacy responses are observed.
#'   It must be a vector of length at least 2 and the order of its elements must
#'   correspond to values specified in `eff`.
#' @slot sigma2W (`numeric`)\cr the prior variance of the flexible efficacy form.
#'   This is either a fixed value or a named vector with two positive numbers,
#'   the shape (`a`), and the rate (`b`) parameters for the gamma distribution.
#' @slot sigma2betaW (`numeric`)\cr the prior variance of the random walk model
#'   for the mean efficacy responses. This is either a fixed value or a named
#'   vector with two positive numbers, the shape (`a`), and the rate (`b`)
#'   parameters for the gamma distribution.
#' @slot use_fixed (`logical`)\cr indicates whether a fixed value for
#'   `sigma2W` and `sigma2betaW` (for each parameter separately) is used or not.
#'   This slot is needed for internal purposes and must not be touched by the user.
#' @slot rw1 (`flag`)\cr used for smoothing data for this efficacy model. If it
#'   is `TRUE`, the first-order random walk model is used for the mean efficacy
#'   responses. Otherwise, the random walk of second order is used.
#' @slot X (`matrix`)\cr the design matrix for the efficacy responses. It is
#'   based on both the pseudo and the observed efficacy responses.
#' @slot RW (`matrix`)\cr the difference matrix for the random walk model. This
#'   slot is needed for internal purposes and must not be used by the user.
#' @slot RW_rank (`integer`)\cr is the rank of the difference matrix. This
#'   slot is needed for internal purposes and must not be used by the user.
#'
#' @aliases EffFlexi
#' @export
#'
.EffFlexi <- setClass(
  Class = "EffFlexi",
  slots = c(
    eff = "numeric",
    eff_dose = "numeric",
    sigma2W = "numeric",
    sigma2betaW = "numeric",
    use_fixed = "logical",
    rw1 = "logical",
    X = "matrix",
    RW = "matrix",
    RW_rank = "integer"
  ),
  prototype = prototype(
    eff = c(0, 0),
    eff_dose = c(1, 1),
    sigma2W = 0.025,
    sigma2betaW = 1,
    rw1 = TRUE,
    use_fixed = c(sigma2W = TRUE, sigma2betaW = TRUE)
  ),
  contains = "ModelEff",
  validity = v_model_eff_flexi
)

## constructor ----

#' @rdname EffFlexi-class
#'
#' @param eff (`numeric`)\cr the pseudo efficacy responses.
#'   Elements of `eff` must correspond to the elements of `eff_dose`.
#' @param eff_dose (`numeric`)\cr dose levels that correspond to pseudo efficacy
#'   responses in `eff`.
#' @param sigma2W (`numeric`)\cr the prior variance of the efficacy responses.
#'   This is either a fixed value or a named vector with two positive numbers,
#'   the shape (`a`), and the rate (`b`) parameters for the inverse gamma
#'   distribution.
#' @param sigma2betaW (`numeric`)\cr the prior variance of the random walk model
#'   used for smoothing. This is either a fixed value or a named vector with two
#'   positive numbers, the shape (`a`), and the rate (`b`) parameters for the
#'   inverse gamma distribution.
#' @param rw1 (`flag`)\cr used for smoothing data for this efficacy model. If it
#'   is `TRUE`, the first-order random walk model is used for the mean efficacy
#'   responses. Otherwise, the random walk of second order is used.
#' @param data (`DataDual`)\cr observed data to update estimates of the model
#'   parameters.
#'
#' @export
#' @example examples/Model-class-EffFlexi.R
#'
EffFlexi <- function(eff, eff_dose, sigma2W, sigma2betaW, rw1 = TRUE, data) {
  assert_numeric(eff)
  assert_numeric(eff_dose)
  assert_numeric(sigma2W, min.len = 1, max.len = 2)
  assert_numeric(sigma2betaW, min.len = 1, max.len = 2)
  assert_flag(rw1)
  assert_class(data, "DataDual")

  use_fixed <- c(
    sigma2W = test_number(sigma2W),
    sigma2betaW = test_number(sigma2betaW)
  )

  x <- c(eff_dose, getEff(data, no_dlt = TRUE)$x_no_dlt)
  x_level <- match_within_tolerance(x, data@doseGrid)
  X <- model.matrix(~ -1L + factor(x_level, levels = seq_len(data@nGrid)))
  X <- matrix(as.integer(X), ncol = ncol(X)) # To remove some obsolete attributes.

  # Set up the random walk penalty matrix and its rank.
  # D1: difference matrix of order 1.
  D1 <- cbind(0, diag(data@nGrid - 1)) - cbind(diag(data@nGrid - 1), 0)
  if (rw1) {
    # the rank-deficient prior precision for the RW1 prior.
    RW <- crossprod(D1)
    RW_rank <- data@nGrid - 1L # rank = dimension - 1. # nolintr
  } else {
    # Second-order difference.
    D2 <- D1[-1, -1] %*% D1
    RW <- crossprod(D2)
    RW_rank <- data@nGrid - 2L # nolintr
  }

  .EffFlexi(
    eff = eff,
    eff_dose = eff_dose,
    sigma2W = sigma2W,
    sigma2betaW = sigma2betaW,
    use_fixed = use_fixed,
    rw1 = rw1,
    X = X,
    RW = RW,
    RW_rank = RW_rank,
    data = data
  )
}

## default constructor ----

#' @rdname EffFlexi-class
#' @note Typically, end users will not use the `.DefaultEffFlexi()` function.
#' @export
.DefaultEffFlexi <- function() {
  empty_data <- DataDual(doseGrid = seq(25, 300, 25))
  EffFlexi(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    sigma2W = c(a = 0.1, b = 0.1),
    sigma2betaW = c(a = 20, b = 50),
    rw1 = FALSE,
    data = empty_data
  )

  data <- DataDual(
    x = c(25, 50, 50, 75, 100, 100, 225, 300),
    y = c(0, 0, 0, 0, 1, 1, 1, 1),
    w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
    doseGrid = empty_data@doseGrid,
    ID = 1L:8L,
    cohort = as.integer(c(1, 2, 2, 3, 4, 4, 5, 6))
  )

  EffFlexi(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    sigma2W = c(a = 0.1, b = 0.1),
    sigma2betaW = c(a = 20, b = 50),
    rw1 = FALSE,
    data = data
  )
}

# DALogisticLogNormal ----

## class ----

#' `DALogisticLogNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DALogisticLogNormal`] is the class for the logistic model with bivariate
#' (log) normal prior and data augmentation. This class inherits from the
#' [`LogisticLogNormal`] class.
#'
#' @note We still need to include here formula for the lambda prior.
#'
#' @slot npiece (`number`)\cr the number of pieces in the `PEM`.
#' @slot l (`numeric`)\cr a vector used in the lambda prior.
#' @slot c_par (`numeric`)\cr a parameter used in the lambda prior; according to
#'   Liu's paper, `c_par = 2` is recommended.
#' @slot cond_pem (`flag`)\cr is a conditional piecewise-exponential model used?
#'   (default). Otherwise an unconditional model is used.
#'
#' @seealso [`ModelLogNormal`], [`LogisticNormal`], [`LogisticLogNormal`].
#'
#' @aliases DALogisticLogNormal
#' @export
#'
.DALogisticLogNormal <- setClass(
  Class = "DALogisticLogNormal",
  slots = c(
    npiece = "integer",
    l = "numeric",
    c_par = "numeric",
    cond_pem = "logical"
  ),
  prototype = prototype(
    npiece = 3L,
    l = 0.5,
    c_par = 2,
    cond_pem = TRUE
  ),
  contains = "LogisticLogNormal",
  validity = v_model_da_logistic_log_normal
)

## constructor ----

#' @rdname DALogisticLogNormal-class
#'
#' @param npiece (`number`)\cr the number of pieces in the `PEM`.
#' @param l (`numeric`)\cr a vector used in the lambda prior.
#' @param c_par (`numeric`)\cr a parameter used in the lambda prior; according to
#'   Liu's paper, `c_par = 2` is recommended.
#' @param cond_pem (`flag`)\cr is a conditional piecewise-exponential model used?
#'   (default). Otherwise an unconditional model is used.
#' @inheritDotParams LogisticLogNormal
#'
#' @export
#' @example examples/Model-class-DALogisticLogNormal.R
#'
DALogisticLogNormal <- function(
  npiece = 3,
  l,
  c_par = 2,
  cond_pem = TRUE,
  ...
) {
  assert_flag(cond_pem)

  start <- LogisticLogNormal(...)

  datamodel <- function() {
    for (i in 1:nObs) {
      # Part I: describe the logistic model of DLTs vs dose.
      logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)

      # Part II: describe the piecewise exponential.
      # Notice that:
      # when y=1             -> DLT=1 and u=<T;
      # when y=0 & T<t (u=T) -> DLT=0;
      # when y=0 & T>t (u<T) -> DLT=NA/missing;
      # when indx=0 -> censored, i.e u<T and event=0;
      # when indx=1 -> not censored, i.e. u>=T or event=1;
      indx[i] <- 1 - step(Tmax - u[i] - eps) * (1 - y[i])

      for (j in 1:npiece) {
        # When not censored, i.e DLT!=NA & t[i]=u[i];
        # if t[i]<h[j], d[i,j]=0;
        # if h[j]<t[i]=<h[j+1], d[i,j]=1
        # if h[j+1]<t[i], d[i,j]=0
        # When censored t[i]>u[i] -> d[i,j]=0
        d[i, j] <- y[i] * step(u[i] - h[j] - eps) * step(h[j + 1] - u[i])

        # DLT free survival(time) for patient i in interval I(j);
        # if t[i]<h[j], s[i,j]=0;
        # if h[j]<t[i]<=h[j+1], s[i,j]=t[i]-h[j]
        # if h[j+1]<=t[i], s[i,j]=h[j+1]-h[j]
        s[i, j] <- min(u[i] - h[j], h[j + 1] - h[j]) * step(u[i] - h[j])

        # piecewise exponential hazard rate lambda[j];
        mu_u[i, j] <- lambda[j] * s[i, j]
        mu[i, j] <- d[i, j] * log(lambda[j]) - y[i] * mu_u[i, j]
      }

      # The likelihood function.
      # nolint start
      L_obs[i] <- exp(sum(mu[i, ])) *
        pow(p[i] / A, y[i]) *
        pow(1 - p[i], 1 - y[i]) # Not censored.
      # nolint end
      L_cnsr[i] <- 1 - p[i] * (1 - exp(-sum(mu_u[i, ]))) / A # Censored. # nolintr
      L[i] <- pow(L_obs[i], indx[i]) * pow(L_cnsr[i], 1 - indx[i])

      # Apply zero trick in JAGS.
      phi[i] <- -log(L[i]) + cadj
      zeros[i] ~ dpois(phi[i])
    }
  }

  priormodel <- h_jags_join_models(
    start@priormodel,
    function() {
      g_beta <- 1 / c_par
      for (j in 1:npiece) {
        g_alpha[j] <- l[j] / c_par
        lambda[j] ~ dgamma(g_alpha[j], g_beta)
        mu_T[j] <- lambda[j] * (h[j + 1] - h[j]) # nolintr
      }
      # If cond = 1, then conditional PEM is used and A is defined as
      # the probability to have DLT, i.e. t<T, otherwise
      # cond = 0 and A is just 1 (so no impact in likelihood).
      A <- cond * (1 - exp(-sum(mu_T))) + (1 - cond)
    }
  )

  modelspecs <- function(nObs, Tmax, from_prior) {
    ms <- list(
      prec = start@params@prec,
      mean = start@params@mean,
      npiece = npiece,
      l = l,
      c_par = c_par,
      h = seq(from = 0L, to = Tmax, length = npiece + 1),
      cond = as.integer(cond_pem)
    )
    if (!from_prior) {
      ms <- c(
        list(
          ref_dose = start@ref_dose,
          zeros = rep(0, nObs),
          eps = 1e-10,
          cadj = 1e10
        ),
        ms
      )
    }
    ms
  }

  assert_integerish(npiece, lower = 1)

  .DALogisticLogNormal(
    start,
    npiece = as.integer(npiece),
    l = l,
    c_par = c_par,
    cond_pem = cond_pem,
    datamodel = datamodel,
    priormodel = priormodel,
    modelspecs = modelspecs,
    datanames = c("nObs", "y", "x", "u", "Tmax"),
    sample = c("alpha0", "alpha1", "lambda")
  )
}

## default constructor ----

#' @rdname DALogisticLogNormal-class
#' @note Typically, end users will not use the `.DefaultDALogisticLogNormal()` function.
#' @export
.DefaultDALogisticLogNormal <- function() {
  npiece <- 10
  Tmax <- 60

  lambda_prior <- function(k) {
    npiece / (Tmax * (npiece - k + 0.5))
  }

  DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece,
    l = as.numeric(t(apply(
      as.matrix(c(1:npiece), 1, npiece),
      2,
      lambda_prior
    ))),
    c_par = 2
  )
}

# TITELogisticLogNormal ----

## class ----

#' `TITELogisticLogNormal`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`TITELogisticLogNormal`] is the class for TITE-CRM based on a logistic
#' regression model using a bivariate normal prior on the intercept and log
#' slope parameters.
#'
#' This class inherits from the [`LogisticLogNormal`].
#'
#' @slot weight_method (`string`)\cr the weight function method: either linear
#'   or adaptive. This was used in Liu, Yin and Yuan's paper.
#'
#' @seealso [`DALogisticLogNormal`].
#'
#' @aliases TITELogisticLogNormal
#' @export
#'
.TITELogisticLogNormal <- setClass(
  Class = "TITELogisticLogNormal",
  slots = c(weight_method = "character"),
  prototype = prototype(weight_method = "linear"),
  contains = "LogisticLogNormal",
  validity = v_model_tite_logistic_log_normal
)

## constructor ----

#' @rdname TITELogisticLogNormal-class
#'
#' @param weight_method (`string`)\cr the weight function method: either linear
#'   or adaptive. This was used in Liu, Yin and Yuan's paper.
#' @inheritDotParams LogisticLogNormal
#'
#' @export
#' @example examples/Model-class-TITELogisticLogNormal.R
#'
TITELogisticLogNormal <- function(weight_method = "linear", ...) {
  assert_character(
    weight_method,
    min.len = 1L,
    max.len = 2L,
    any.missing = FALSE
  )

  start <- LogisticLogNormal(...)

  datamodel <- function() {
    for (i in 1:nObs) {
      logit(p[i]) <- alpha0 + alpha1 * log(x[i] / ref_dose)

      # The piecewise exponential likelihood. Notice that:
      # when y=1             -> DLT=1 and u=<T;
      # when y=0 & T<t (u=T) -> DLT=0;
      # when y=0 & T>t (u<T) -> DLT=NA/missing;
      # when indx=0 -> censored, i.e u<T and event=0;
      # when indx=1 -> not censored, i.e. u>=T or event=1;
      L[i] <- pow(p[i], y[i]) * pow((1 - w[i] * p[i]), (1 - y[i]))

      # Apply zero trick in JAGS.
      phi[i] <- -log(L[i]) + cadj
      zeros[i] ~ dpois(phi[i])
    }
  }

  modelspecs <- function(nObs, u, Tmax, y, from_prior) {
    ms <- list(prec = start@params@prec, mean = start@params@mean)
    # Calculate weights `w` based on the input data.
    if (!from_prior && nObs > 0L) {
      if (weight_method == "linear") {
        w <- u / Tmax
      } else if (weight_method == "adaptive") {
        nDLT <- sum(y)
        if (nDLT > 0) {
          u_dlt <- sort(u[y == 1])
          w <- sapply(u, function(u_i) {
            m <- sum(u_i >= u_dlt)
            w_i <- if (m == 0) {
              u_i / u_dlt[1]
            } else if (m < nDLT) {
              m + (u_i - u_dlt[m]) / (u_dlt[m + 1] - u_dlt[m])
            } else {
              # m == nDLT. nolintr
              m + (u_i - u_dlt[m]) / (Tmax + 0.00000001 - u_dlt[m])
            }
            w_i / (nDLT + 1)
          })
        } else {
          w <- u / Tmax
        }
      }
      w[y == 1] <- 1
      w[u == Tmax] <- 1

      ms <- c(
        list(
          ref_dose = start@ref_dose,
          zeros = rep(0, nObs),
          cadj = 1e10,
          w = w
        ),
        ms
      )
    }
    ms
  }

  .TITELogisticLogNormal(
    start,
    weight_method = weight_method,
    datamodel = datamodel,
    modelspecs = modelspecs,
    datanames = c("nObs", "y", "x")
  )
}

## default constructor ----

#' @rdname TITELogisticLogNormal-class
#' @note Typically, end users will not use the `.DefaultTITELogisticLogNormal()` function.
#' @export
.DefaultTITELogisticLogNormal <- function() {
  TITELogisticLogNormal(
    mean = c(0, 1),
    cov = diag(2),
    ref_dose = 1,
    weight_method = "linear"
  )
}

# OneParLogNormalPrior ----

## class ----

#' `OneParLogNormalPrior`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`OneParLogNormalPrior`] is the class for a standard CRM with a normal prior on
#' the log power parameter for the skeleton prior probabilities.
#'
#' @slot skel_fun (`function`)\cr function to calculate the prior DLT probabilities.
#' @slot skel_fun_inv (`function`)\cr inverse function of `skel_fun`.
#' @slot skel_probs (`numeric`)\cr skeleton prior probabilities. This is a vector
#'   of unique and sorted probability values between 0 and 1.
#' @slot sigma2 (`number`)\cr prior variance of log power parameter alpha.
#'
#' @seealso [`ModelLogNormal`].
#'
#' @aliases OneParLogNormalPrior
#' @export
#'
.OneParLogNormalPrior <- setClass(
  Class = "OneParLogNormalPrior",
  slots = c(
    skel_fun = "function",
    skel_fun_inv = "function",
    skel_probs = "numeric",
    sigma2 = "numeric"
  ),
  contains = "GeneralModel",
  validity = v_model_one_par_exp_normal_prior
)

## constructor ----

#' @rdname OneParLogNormalPrior-class
#'
#' @param skel_probs (`numeric`)\cr skeleton prior probabilities. This is a vector
#'   of unique and sorted probability values between 0 and 1.
#' @param dose_grid (`numeric`)\cr dose grid. It must be must be a sorted vector
#'   of the same length as `skel_probs`.
#' @param sigma2 (`number`)\cr prior variance of log power parameter alpha.
#'
#' @export
#' @example examples/Model-class-OneParLogNormalPrior.R
#'
OneParLogNormalPrior <- function(skel_probs, dose_grid, sigma2) {
  assert_probabilities(skel_probs, unique = TRUE, sorted = TRUE) # So that skel_fun_inv exists.
  assert_numeric(
    dose_grid,
    len = length(skel_probs),
    any.missing = FALSE,
    unique = TRUE,
    sorted = TRUE
  )

  skel_fun <- approxfun(x = dose_grid, y = skel_probs, rule = 2)
  skel_fun_inv <- approxfun(x = skel_probs, y = dose_grid, rule = 2)

  .OneParLogNormalPrior(
    skel_fun = skel_fun,
    skel_fun_inv = skel_fun_inv,
    skel_probs = skel_probs,
    sigma2 = sigma2,
    datamodel = function() {
      for (i in 1:nObs) {
        p[i] <- skel_probs[xLevel[i]]^exp(alpha)
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      alpha ~ dnorm(0, 1 / sigma2)
    },
    modelspecs = function(from_prior) {
      ms <- list(sigma2 = sigma2)
      if (!from_prior) {
        ms$skel_probs <- skel_probs
      }
      ms
    },
    init = function() {
      list(alpha = 1)
    },
    datanames = c("nObs", "y", "xLevel"),
    sample = "alpha"
  )
}

## default constructor ----

#' @rdname OneParLogNormalPrior-class
#' @return an instance of the `OneParLogNormalPrior` class
#' @export
.DefaultOneParLogNormalPrior <- function() {
  OneParLogNormalPrior(
    skel_probs = seq(from = 0.1, to = 0.9, length = 5),
    dose_grid = 1:5,
    sigma2 = 2
  )
}

# OneParExpPrior ----

## class ----

#' `OneParExpPrior`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`OneParExpPrior`] is the class for a standard CRM with an exponential prior
#' on the power parameter for the skeleton prior probabilities. It is an
#' implementation of a version of the one-parameter CRM
#' \insertCite{OQuigleyPepeFisher1990}{crmPack}.
#'
#' @note Typically, end users will not use the `.DefaultOneparExpPrior()` function.
#'
#' @slot skel_fun (`function`)\cr function to calculate the prior DLT probabilities.
#' @slot skel_fun_inv (`function`)\cr inverse function of `skel_fun`.
#' @slot skel_probs (`numeric`)\cr skeleton prior probabilities. This is a vector
#'   of unique and sorted probability values between 0 and 1.
#' @slot lambda (`number`)\cr rate parameter of prior exponential distribution
#'   for theta.
#'
#' @aliases OneParExpPrior
#' @export
#' @references
#'   \insertAllCited{}
#'
.OneParExpPrior <- setClass(
  Class = "OneParExpPrior",
  slots = c(
    skel_fun = "function",
    skel_fun_inv = "function",
    skel_probs = "numeric",
    lambda = "numeric"
  ),
  contains = "GeneralModel",
  validity = v_model_one_par_exp_prior
)

## constructor ----

#' @rdname OneParExpPrior-class
#'
#' @param skel_probs see slot definition.
#' @param dose_grid (`numeric`)\cr dose grid. It must be must be a sorted vector
#'   of the same length as `skel_probs`.
#' @param lambda see slot definition.
#'
#' @export
#' @example examples/Model-class-OneParExpPrior.R
#'
OneParExpPrior <- function(skel_probs, dose_grid, lambda) {
  assert_probabilities(skel_probs, unique = TRUE, sorted = TRUE) # So that skel_fun_inv exists.
  assert_numeric(
    dose_grid,
    len = length(skel_probs),
    any.missing = FALSE,
    unique = TRUE,
    sorted = TRUE
  )

  skel_fun <- approxfun(x = dose_grid, y = skel_probs, rule = 2)
  skel_fun_inv <- approxfun(x = skel_probs, y = dose_grid, rule = 2)

  .OneParExpPrior(
    skel_fun = skel_fun,
    skel_fun_inv = skel_fun_inv,
    skel_probs = skel_probs,
    lambda = lambda,
    datamodel = function() {
      for (i in 1:nObs) {
        p[i] <- skel_probs[xLevel[i]]^theta
        y[i] ~ dbern(p[i])
      }
    },
    priormodel = function() {
      theta ~ dexp(lambda)
    },
    modelspecs = function(from_prior) {
      ms <- list(lambda = lambda)
      if (!from_prior) {
        ms$skel_probs <- skel_probs
      }
      ms
    },
    init = function() {
      list(theta = 1)
    },
    datanames = c("nObs", "y", "xLevel"),
    sample = "theta"
  )
}

## default constructor ----

#' @rdname OneParExpPrior-class
#' @note Typically, end users will not use the `.DefaultOneParLogNormalPrior()` function.
#' @export
.DefaultOneParExpPrior <- function() {
  OneParExpPrior(
    skel_probs = c(0.1, 0.3, 0.5, 0.7, 0.9),
    dose_grid = 1:5,
    lambda = 2
  )
}

# FractionalCRM ----

## class ----

#' `FractionalCRM`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`FractionalCRM`] is the class for a fractional CRM model based on a one
#' parameter CRM (with normal prior on the log-power parameter) as well as
#' Kaplan-Meier based estimation of the conditional probability to experience a
#' DLT for non-complete observations.
#'
#' This fractional CRM model follows the paper and code by \insertCite{YinZhengXu2013;textual}{crmPack}.
#'
#' @seealso [`TITELogisticLogNormal`].
#'
#' @aliases FractionalCRM
#' @export
#' @references
#'   \insertAllCited{}
#'
.FractionalCRM <- setClass(
  Class = "FractionalCRM",
  contains = "OneParLogNormalPrior"
)

## constructor ----

#' @rdname FractionalCRM-class
#'
#' @inheritDotParams OneParLogNormalPrior
#'
#' @export
#' @example examples/Model-class-FractionalCRM.R
#'
FractionalCRM <- function(...) {
  start <- OneParLogNormalPrior(...)

  # This is adapted from the TITELogisticLogNormal class.
  datamodel <- function() {
    for (i in 1:nObs) {
      p[i] <- skel_probs[xLevel[i]]^exp(alpha)

      # The piecewise exponential likelihood. Notice that:
      # when y=1             -> DLT=1 and u=<T;
      # when y=0 & T<t (u=T) -> DLT=0;
      # when y=0 & T>t (u<T) -> DLT=NA/missing.
      # Therefore, `yhat` is used instead of `y` for the likelihood f. (see `modelspecs`).
      L[i] <- pow(p[i], yhat[i]) * pow((1 - p[i]), (1 - yhat[i]))

      # Apply zero trick in JAGS.
      phi[i] <- -log(L[i]) + cadj
      zeros[i] ~ dpois(phi[i])
    }
  }

  modelspecs <- function(nObs, u, Tmax, y, from_prior) {
    ms <- list(sigma2 = start@sigma2)
    if (!from_prior) {
      # Calculate fractional contribution `yhat`
      # based on the input data using the Kaplan-Meier method.
      yhat <- if (nObs > 0) {
        km <- survival::survfit(survival::Surv(u, y) ~ 1)
        s_tau <- tail(km$surv[km$time <= Tmax], 1) # Survival probability = S(Tmax).
        ifelse(
          u < Tmax & y == 0L, # Within the assessment window and so far no DLT.
          yes = 1 -
            s_tau / sapply(u, function(u_i) tail(km$surv[km$time <= u_i], 1)),
          no = y
        )
      } else {
        1L
      }
      ms <- c(
        list(
          skel_probs = start@skel_probs,
          zeros = rep(0, nObs),
          cadj = 1e10,
          yhat = yhat
        ),
        ms
      )
    }
    ms
  }

  .FractionalCRM(
    start,
    datamodel = datamodel,
    modelspecs = modelspecs,
    datanames = c("nObs", "xLevel")
  )
}

## default constructor ----

#' @rdname FractionalCRM-class
#' @note Typically, end users will not use the `.DefaultTITELogisticLogNormal()` function.
#' @export
.DefaultFractionalCRM <- function() {
  FractionalCRM(
    skel_probs = c(0.1, 0.2, 0.3, 0.4),
    dose_grid = c(10, 30, 50, 100),
    sigma2 = 2
  )
}

## class ----

#' `LogisticLogNormalOrdinal`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`LogisticLogNormalOrdinal`] is the class for a logistic lognormal CRM model
#' using an ordinal toxicity scale.
#'
#' @aliases LogisticLogNormalOrdinal
#' @export
.LogisticLogNormalOrdinal <- setClass(
  Class = "LogisticLogNormalOrdinal",
  contains = "ModelLogNormal",
  validity = v_logisticlognormalordinal
)

## constructor ----

#' @rdname LogisticLogNormalOrdinal-class
#' @inheritParams ModelLogNormal
#' @export
#' @example examples/Model-class-LogisticLogNormalOrdinal.R
LogisticLogNormalOrdinal <- function(mean, cov, ref_dose) {
  params <- ModelParamsNormal(mean, cov)
  .LogisticLogNormalOrdinal(
    params = params,
    ref_dose = positive_number(ref_dose),
    priormodel = function() {
      alpha[1] ~ dnorm(mean[1], prec[1, 1])
      for (i in 2:(k - 1)) {
        alpha[i] ~ dnorm(mean[i], prec[i, i]) %_% T(, alpha[i - 1])
      }
      gamma ~ dnorm(mean[k], prec[k, k])
      beta <- exp(gamma)
    },
    datamodel = function() {
      for (i in 1:nObs) {
        xhat[i] <- log(x[i] / ref_dose)
        for (j in 1:(k - 1)) {
          z[i, j] <- alpha[j] + beta * xhat[i]
          p[i, j] <- exp(z[i, j]) / (1 + exp(z[i, j]))
          tox[i, j] ~ dbern(p[i, j])
        }
      }
    },
    modelspecs = function(y, from_prior) {
      ms <- list(
        mean = params@mean,
        prec = params@prec,
        k = length(mean)
      )
      if (!from_prior) {
        ms$tox <- array(dim = c(length(y), length(mean) - 1))
        for (i in seq_along(y)) {
          for (j in 1:(ms$k - 1)) {
            ms$tox[i, j] <- y[i] >= j
          }
        }
        ms$ref_dose <- ref_dose
      }
      ms
    },
    init = function() {
      list(
        alpha = sapply(1:(length(mean) - 1), function(x) -(x + 1)),
        gamma = 1
      )
    },
    datanames = c("nObs", "x"),
    # Need to provide JAGS column names here
    sample = c(paste0("alpha[", 1:(length(mean) - 1), "]"), "beta")
  )
}

## default constructor ----

#' @rdname LogisticLogNormalOrdinal-class
#' @note Typically, end users will not use the `.DefaultLogisticLogNormalOrdinal()` function.
#' @export
.DefaultLogisticLogNormalOrdinal <- function() {
  LogisticLogNormalOrdinal(
    mean = c(-3, -4, 1),
    cov = diag(c(3, 4, 1)),
    ref_dose = 50
  )
}
