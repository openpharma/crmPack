#' @include Model-class.R
#' @include Samples-class.R
NULL

# doseFunction ----

## generic ----

#' Getting the Dose Function for a Given Model Type
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function that returns a [dose()] method that computes the dose reaching a
#' specific target value of a given independent variable, based on the model
#' specific parameters.
#'
#' @param model (`GeneralModel` or `ModelPseudo`)\cr the model.
#' @param ... model specific parameters.
#'
#' @return A [dose()] method that computes doses.
#'
#' @seealso [dose()], [probFunction()].
#'
#' @export
#' @example examples/Model-method-doseFunction.R
#'
setGeneric(
  name = "doseFunction",
  def = function(model, ...) {
    standardGeneric("doseFunction")
  },
  valueClass = "function"
)

## GeneralModel ----

#' @describeIn doseFunction
#'
#' @aliases doseFunction-GeneralModel
#' @export
#'
setMethod(
  f = "doseFunction",
  signature = "GeneralModel",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_subset(names(model_params), model@sample, empty.ok = FALSE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(x) {
      dose(x, model, samples)
    }
  }
)

## ModelPseudo ----

#' @describeIn doseFunction
#'
#' @aliases doseFunction-ModelPseudo
#' @export
#'
setMethod(
  f = "doseFunction",
  signature = "ModelPseudo",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(names(model_params), len = length(model_params), any.missing = FALSE, unique = TRUE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(x) {
      dose(x, model, samples)
    }
  }
)

# probFunction ----

## generic ----

#' Getting the Prob Function for a Given Model Type
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function that returns a [prob()] function that computes the toxicity
#' probabilities for a given dose level, based on the model specific parameters.
#'
#' @param model (`GeneralModel` or `ModelTox`)\cr the model.
#' @param ... model specific parameters.
#'
#' @return A [prob()] function that computes toxicity probabilities.
#'
#' @seealso [prob()], [doseFunction()].
#'
#' @export
#' @example examples/Model-method-probFunction.R
#'
setGeneric(
  name = "probFunction",
  def = function(model, ...) {
    standardGeneric("probFunction")
  },
  valueClass = "function"
)

## GeneralModel ----

#' @describeIn probFunction
#'
#' @aliases probFunction-GeneralModel
#' @export
#'
setMethod(
  f = "probFunction",
  signature = "GeneralModel",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_subset(names(model_params), model@sample, empty.ok = FALSE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(dose) {
      prob(dose, model, samples)
    }
  }
)

## ModelTox ----

#' @describeIn probFunction
#'
#' @aliases probFunction-ModelTox
#' @export
#'
setMethod(
  f = "probFunction",
  signature = "ModelTox",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(names(model_params), len = length(model_params), any.missing = FALSE, unique = TRUE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(dose) {
      prob(dose, model, samples)
    }
  }
)

# efficacyFunction ----

## generic ----

#' Getting the Efficacy Function for a Given Model Type
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function that returns an [efficacy()] function that computes expected
#' efficacy for a given dose level, based on the model specific parameters.
#'
#' @param model (`ModelEff`)\cr the model.
#' @param ... model specific parameters.
#'
#' @return A [efficacy()] function that computes expected efficacy.
#'
#' @seealso [efficacy()].
#'
#' @export
#' @example examples/Model-method-efficacyFunction.R
#'
setGeneric(
  name = "efficacyFunction",
  def = function(model, ...) {
    standardGeneric("efficacyFunction")
  },
  valueClass = "function"
)

## ModelEff ----

#' @describeIn efficacyFunction
#'
#' @aliases efficacyFunction-ModelEff
#' @export
#'
setMethod(
  f = "efficacyFunction",
  signature = "ModelEff",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(names(model_params), len = length(model_params), any.missing = FALSE, unique = TRUE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(dose) {
      efficacy(dose, model, samples)
    }
  }
)

# dose ----

## generic ----

#' Computing the Doses for a given independent variable, Model and Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that computes the dose reaching a specific target value of a
#' given variable that dose depends on. The meaning of this variable depends
#' on the type of the model. For instance, for single agent dose escalation
#' model or pseudo DLE (dose-limiting events)/toxicity model, this variable
#' represents the a probability of the occurrence of a DLE. For efficacy models,
#' it represents expected efficacy.
#' The doses are computed based on the samples of the model parameters (samples).
#'
#' @details The `dose()` function computes the doses corresponding to a value of
#'   a given independent variable, using samples of the model parameter(s).
#'   If you work with multivariate model parameters, then assume that your model
#'   specific `dose()` method receives a samples matrix where the rows
#'   correspond to the sampling index, i.e. the layout is then
#'   `nSamples x dimParameter`.
#'
#' @note The [dose()] and [prob()] methods are the inverse of each other, for
#'   all [dose()] methods for which its first argument, i.e. a given independent
#'   variable that dose depends on, represents toxicity probability.
#'
#' @param x (`proportion` or `numeric`)\cr a value of an independent variable
#'   on which dose depends.
#'   The following recycling rule applies when `samples` is not missing: vectors
#'   of size 1 will be recycled to the size of the sample
#'   (i.e. `sampleSize(samples@options)`). Otherwise, `x` must have the same size
#'   as the sample.
#' @param model (`GeneralModel` or `ModelPseudo`)\cr the model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute the resulting doses. Can also be missing for some models.
#' @param ... model specific parameters when `samples` are not used.
#'
#' @return A `number` or `numeric` vector with the doses.
#'   If non-scalar `samples` were used, then every element in the returned vector
#'   corresponds to one element of a sample. Hence, in this case, the output
#'   vector is of the same length as the sample vector. If scalar `samples` were
#'   used or no `samples` were used, e.g. for pseudo DLE/toxicity `model`,
#'   then the output is of the same length as the length of the `prob`.
#'
#' @seealso [doseFunction()], [prob()], [efficacy()].
#'
#' @export
#' @example examples/Model-method-dose.R
#'
setGeneric(
  name = "dose",
  def = function(x, model, samples, ...) {
    standardGeneric("dose")
  },
  valueClass = "numeric"
)

## LogisticNormal ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticNormal",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticLogNormal ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticLogNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticLogNormal",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticLogNormalSub ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticLogNormalSub
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticLogNormalSub",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- model@ref_dose
    ((logit(x) - alpha0) / alpha1) + ref_dose
  }
)

## ProbitLogNormal ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-ProbitLogNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "ProbitLogNormal",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((probit(x) - alpha0) / alpha1) * ref_dose
  }
)

## ProbitLogNormalRel ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-ProbitLogNormalRel
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "ProbitLogNormalRel",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    ((probit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticKadane ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticKadane
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticKadane",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("rho0", "gamma"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma * (logit(x) - logit(rho0)) + xmin * (logit(theta) - logit(x))
    num / (logit(theta) - logit(rho0))
  }
)

## LogisticKadaneBetaGamma ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticKadaneBetaGamma
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticKadaneBetaGamma",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("rho0", "gamma"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma * (logit(x) - logit(rho0)) + xmin * (logit(theta) - logit(x))
    num / (logit(theta) - logit(rho0))
  }
)

## LogisticNormalMixture ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticNormalMixture
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticNormalMixture",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticNormalFixedMixture ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticNormalFixedMixture
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticNormalFixedMixture",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticLogNormalMixture ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticLogNormalMixture
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticLogNormalMixture",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    stop("not implemented")
  }
)

## DualEndpoint ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-DualEndpoint
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "DualEndpoint",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset("betaZ", names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    betaZ <- samples@data$betaZ
    ref_dose <- as.numeric(model@ref_dose)
    dose_temp <- (qnorm(x) - betaZ[, 1]) / betaZ[, 2]
    if (model@use_log_dose) {
      exp(dose_temp) * ref_dose
    } else {
      dose_temp * ref_dose
    }
  }
)

## LogisticIndepBeta ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-LogisticIndepBeta
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticIndepBeta",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset(c("phi1", "phi2"), names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    phi1 <- samples@data$phi1
    phi2 <- samples@data$phi2
    log_dose <- (log(x / (1 - x)) - phi1) / phi2
    exp(log_dose)
  }
)

## LogisticIndepBeta-noSamples ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'   All model parameters (except `x`) should be present in the `model` object.
#'
#' @aliases dose-LogisticIndepBeta-noSamples
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticIndepBeta",
    samples = "missing"
  ),
  definition = function(x, model) {
    assert_probabilities(x)
    model_params <- h_slots(model, c("phi1", "phi2"))
    nsamples <- length(model_params[[1]])
    samples <- Samples(data = model_params, options = McmcOptions(samples = nsamples))
    assert_common_length(x, y_len = nsamples)

    dose(x, model, samples)
  }
)

## Effloglog-noSamples ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'   All model parameters (except `x`) should be present in the `model` object.
#'
#' @aliases dose-Effloglog-noSamples
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "Effloglog",
    samples = "missing"
  ),
  definition = function(x, model) {
    assert_numeric(x, min.len = 1L, any.missing = FALSE)
    theta1 <- model@theta1
    theta2 <- model@theta2
    constant <- model@const
    assert_length(theta1, 1L)
    assert_length(theta2, 1L)
    assert_length(constant, 1L)

    exp(exp((x - theta1) / theta2)) - constant
  }
)

## EffFlexi ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`). For this method `x` must
#'   be a scalar.
#'
#' @aliases dose-EffFlexi
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "EffFlexi",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_number(x)
    assert_subset("ExpEff", names(samples@data))

    samples_efficacy <- samples@data$ExpEff
    dose_grid <- model@data@doseGrid

    # Find dose level for a given expected efficacy level using linear interpolation.
    apply(samples_efficacy, 1, function(se) {
      se_leq_x <- se <= x
      dose_level0 <- max(which(se_leq_x))
      dose_level1 <- min(which(!se_leq_x))
      eff0 <- se[dose_level0]
      eff1 <- se[dose_level1]
      dose0 <- dose_grid[dose_level0]
      dose1 <- dose_grid[dose_level1]
      dose0 + (dose1 - dose0) * ((x - eff0) / (eff1 - eff0))
    })
  }
)

## OneParLogNormalPrior ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLT (`x`).
#'
#' @aliases dose-OneParLogNormalPrior
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "OneParLogNormalPrior",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset("alpha", names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    alpha <- samples@data$alpha
    skel_fun_inv <- model@skel_fun_inv
    skel_fun_inv(x^(1 / exp(alpha)))
  }
)

## OneParExpPrior ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLT (`x`).
#'
#' @aliases dose-OneParExpPrior
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "OneParExpPrior",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_probabilities(x)
    assert_subset("theta", names(samples@data))
    assert_common_length(x, y_len = sampleSize(samples@options))

    theta <- samples@data$theta
    skel_fun_inv <- model@skel_fun_inv
    assert_numeric(theta, lower = .Machine$double.xmin, finite = TRUE)
    skel_fun_inv(x^(1 / theta))
  }
)

# prob ----

## generic ----

#' Computing Toxicity Probabilities for a Given Dose, Model and Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that computes the probability of the occurrence of a DLE at a
#' specified dose level, based on the model parameters (samples).
#'
#' @details The `prob()` function computes the probability of toxicity for given
#'   doses, using samples of the model parameter(s).
#'   If you work with multivariate model parameters, then assume that your model
#'   specific `prob()` method receives a samples matrix where the rows
#'   correspond to the sampling index, i.e. the layout is then
#'   `nSamples x dimParameter`.
#'
#' @note The [prob()] and [dose()] functions are the inverse of
#'   each other, for all [dose()] methods for which its first argument, i.e. a
#'   given independent variable that dose depends on, represents toxicity
#'   probability.
#'
#' @param dose (`number` or `numeric`)\cr the dose which is targeted.
#'   The following recycling rule applies when `samples` is not missing: vectors
#'   of size 1 will be recycled to the size of the sample
#'   (i.e. `sampleSize(samples@options)`). Otherwise, `dose` must have the same
#'   size as the sample.
#' @param model (`GeneralModel` or `ModelTox`)\cr the model for single agent
#'   dose escalation or pseudo DLE (dose-limiting events)/toxicity model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute toxicity probabilities. Can also be missing for some models.
#' @param ... model specific parameters when `samples` are not used.
#'
#' @return A `proportion` or `numeric` vector with the toxicity probabilities.
#'   If non-scalar `samples` were used, then every element in the returned vector
#'   corresponds to one element of a sample. Hence, in this case, the output
#'   vector is of the same length as the sample vector. If scalar `samples` were
#'   used or no `samples` were used, e.g. for pseudo DLE/toxicity `model`,
#'   then the output is of the same length as the length of the `dose`.
#'
#' @seealso [probFunction()], [dose()], [efficacy()].
#'
#' @export
#' @example examples/Model-method-prob.R
#'
setGeneric(
  name = "prob",
  def = function(dose, model, samples, ...) {
    standardGeneric("prob")
  },
  valueClass = "numeric"
)

## LogisticNormal ----

#' @describeIn prob
#'
#' @aliases prob-LogisticNormal
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticNormal",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormal ----

#' @describeIn prob
#'
#' @aliases prob-LogisticLogNormal
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticLogNormal",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormalSub ----

#' @describeIn prob
#'
#' @aliases prob-LogisticLogNormalSub
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticLogNormalSub",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- model@ref_dose
    plogis(alpha0 + alpha1 * (dose - ref_dose))
  }
)

## ProbitLogNormal ----

#' @describeIn prob
#'
#' @aliases prob-ProbitLogNormal
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "ProbitLogNormal",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    pnorm(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## ProbitLogNormalRel ----

#' @describeIn prob
#'
#' @aliases prob-ProbitLogNormalRel
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "ProbitLogNormalRel",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    pnorm(alpha0 + alpha1 * (dose / ref_dose))
  }
)

## LogisticKadane ----

#' @describeIn prob
#'
#' @aliases prob-LogisticKadane
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticKadane",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("rho0", "gamma"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma * logit(rho0) - xmin * logit(theta) + (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

## LogisticKadaneBetaGamma ----

#' @describeIn prob
#'
#' @aliases prob-LogisticKadaneBetaGamma
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticKadaneBetaGamma",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("rho0", "gamma"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma * logit(rho0) - xmin * logit(theta) + (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

## LogisticNormalMixture ----

#' @describeIn prob
#'
#' @aliases prob-LogisticNormalMixture
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticNormalMixture",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticNormalFixedMixture ----

#' @describeIn prob
#'
#' @aliases prob-LogisticNormalFixedMixture
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticNormalFixedMixture",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormalMixture ----

#' @describeIn prob
#'
#' @aliases prob-LogisticLogNormalMixture
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticLogNormalMixture",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    comp <- samples@data$comp
    ref_dose <- as.numeric(model@ref_dose)
    sel <- cbind(seq_along(comp), comp)
    plogis(alpha0[sel] + alpha1[sel] * log(dose / ref_dose))
  }
)

## DualEndpoint ----

#' @describeIn prob
#'
#' @aliases prob-DualEndpoint
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "DualEndpoint",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("betaZ", names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    betaZ <- samples@data$betaZ
    ref_dose <- as.numeric(model@ref_dose)
    stand_dose <- if (model@use_log_dose) {
      log(dose / ref_dose)
    } else {
      dose / ref_dose
    }
    pnorm(betaZ[, 1] + betaZ[, 2] * stand_dose)
  }
)

## LogisticIndepBeta ----

#' @describeIn prob compute toxicity probabilities of the occurrence of a DLE at
#' a specified dose level, based on the samples of [`LogisticIndepBeta`] model
#' parameters.
#'
#' @aliases prob-LogisticIndepBeta
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticIndepBeta",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("phi1", "phi2"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    phi1 <- samples@data$phi1
    phi2 <- samples@data$phi2
    log_dose <- log(dose)
    exp(phi1 + phi2 * log_dose) / (1 + exp(phi1 + phi2 * log_dose))
  }
)

## LogisticIndepBeta-noSamples ----

#' @describeIn prob compute toxicity probabilities of the occurrence of a DLE at
#' a specified dose level, based on the [`LogisticIndepBeta`] model parameters.
#' All model parameters (except `dose`) should be present in the `model` object.
#'
#' @aliases prob-LogisticIndepBeta-noSamples
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticIndepBeta",
    samples = "missing"
  ),
  definition = function(dose, model) {
    model_params <- h_slots(model, c("phi1", "phi2"))
    nsamples <- length(model_params[[1]])
    samples <- Samples(data = model_params, options = McmcOptions(samples = nsamples))

    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_common_length(dose, y_len = nsamples)

    prob(dose, model, samples)
  }
)

## OneParLogNormalPrior ----

#' @describeIn prob
#'
#' @aliases prob-OneParLogNormalPrior
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "OneParLogNormalPrior",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("alpha", names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    alpha <- samples@data$alpha
    skel_fun <- model@skel_fun
    skel_fun(dose)^exp(alpha)
  }
)

## OneParExpPrior ----

#' @describeIn prob
#'
#' @aliases prob-OneParExpPrior
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "OneParExpPrior",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("theta", names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    theta <- samples@data$theta
    skel_fun <- model@skel_fun
    skel_fun(dose)^theta
  }
)

# efficacy ----

## generic ----

#' Computing Expected Efficacy for a Given Dose, Model and Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that computes the value of expected efficacy at a specified dose
#' level, based on the model specific parameters. The model parameters (samples)
#' are obtained based on prior specified in form of pseudo data combined with
#' observed responses (if any).
#'
#' @details The `efficacy()` function computes the expected efficacy for given
#'   doses, using samples of the model parameter(s).
#'   If you work with multivariate model parameters, then assume that your model
#'   specific `efficacy()` method receives a samples matrix where the rows
#'   correspond to the sampling index, i.e. the layout is then
#'   `nSamples x dimParameter`.
#'
#' @param dose (`number` or `numeric`)\cr the dose which is targeted.
#'   The following recycling rule applies when `samples` is not missing: vectors
#'   of size 1 will be recycled to the size of the sample
#'   (i.e. `sampleSize(samples@options)`). Otherwise, `dose` must have the same
#'   size as the sample.
#' @param model (`ModelEff`)\cr the efficacy model with pseudo data prior.
#' @param samples (`Samples`)\cr samples of model's parameters that will be
#'   used to compute expected efficacy values. Can also be missing for some
#'   models.
#' @param ... model specific parameters when `samples` are not used.
#'
#' @return A `numeric` vector with the values of expected efficacy.
#'   If non-scalar `samples` were used, then every element in the returned vector
#'   corresponds to one element of a sample. Hence, in this case, the output
#'   vector is of the same length as the sample vector. If scalar `samples` were
#'   used or no `samples` were used, e.g. for pseudo DLE/toxicity `model`,
#'   then the output is of the same length as the length of the `dose`.
#'
#' @seealso [dose()], [prob()].
#'
#' @export
#' @example examples/Model-method-efficacy.R
setGeneric(
  name = "efficacy",
  def = function(dose, model, samples, ...) {
    standardGeneric("efficacy")
  },
  valueClass = "numeric"
)

## Effloglog ----

#' @describeIn efficacy compute the expected efficacy at a specified dose level,
#' based on the samples of [`Effloglog`] model parameters.
#'
#' @aliases efficacy-Effloglog
#' @export
#'
setMethod(
  f = "efficacy",
  signature = signature(
    dose = "numeric",
    model = "Effloglog",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("theta1", "theta2"), names(samples@data))
    assert_common_length(dose, y_len = sampleSize(samples@options))

    theta1 <- samples@data$theta1
    theta2 <- samples@data$theta2
    constant <- model@const
    theta1 + theta2 * log(log(dose + constant))
  }
)

## Effloglog-noSamples ----

#' @describeIn efficacy compute the expected efficacy at a specified dose level,
#' based on the [`Effloglog`] model parameters.
#' All model parameters (except `dose`) should be present in the `model` object.
#'
#' @aliases efficacy-Effloglog-noSamples
#' @export
#'
setMethod(
  f = "efficacy",
  signature = signature(
    dose = "numeric",
    model = "Effloglog",
    samples = "missing"
  ),
  definition = function(dose, model) {
    model_params <- h_slots(model, c("theta1", "theta2"))
    nsamples <- length(model_params[[1]])
    samples <- Samples(data = model_params, options = McmcOptions(samples = nsamples))

    assert_numeric(dose, lower = 0L, min.len = 1L, any.missing = FALSE, min.len = 1L)
    assert_common_length(dose, y_len = nsamples)

    efficacy(dose, model, samples)
  }
)

## EffFlexi ----

#' @describeIn efficacy compute the expected efficacy at a specified dose level,
#' based on the samples of [`EffFlexi`] model parameters. For this method,
#' the `dose` argument must be a scalar.
#'
#' @aliases efficacy-EffFlexi
#' @export
#'
setMethod(
  f = "efficacy",
  signature = signature(
    dose = "numeric",
    model = "EffFlexi",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_number(dose, lower = 0L)

    samples_efficacy <- samples@data$ExpEff
    dose_grid <- model@data@doseGrid
    dose_level <- matchTolerance(dose, dose_grid)

    if (!is.na(dose_level) == TRUE) {
      samples_efficacy[, dose_level]
    } else {
      # If dose not in doseGrid, do linear interpolation, given that dose is within doseGrid range.
      dose_level0 <- findInterval(dose, dose_grid)
      stopifnot(all(dose_level0) > 0 && all(dose_level0) < model@data@nGrid)
      dose_level1 <- dose_level0 + 1L

      eff0 <- samples_efficacy[, dose_level0]
      eff1 <- samples_efficacy[, dose_level1]
      dose0 <- dose_grid[dose_level0]
      dose1 <- dose_grid[dose_level1]
      eff0 + (eff1 - eff0) * ((dose - dose0) / (dose1 - dose0))
    }
  }
)

# biomarker ----

## generic ----

#' Get the Biomarker Levels for a Given Dual-Endpoint Model, Given Dose Levels and Samples
#'
#' @details This function simply returns a specific columns (with the indices equal
#' to `xLevel`) of the biomarker samples matrix, which is included in the the
#' `samples` object.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param xLevel (`integer`)\cr the levels for the doses the patients have been
#'   given w.r.t dose grid. See [`Data`] for more details.
#' @param model (`DualEndpoint`)\cr the model.
#' @param samples (`Samples`)\cr the samples of model's parameters that store
#'   the value of biomarker levels for all doses on the dose grid.
#' @param ... not used.
#'
#' @return The biomarker levels.
#'
#' @export
#' @example examples/Model-method-biomarker.R
#'
setGeneric(
  name = "biomarker",
  def = function(xLevel, model, samples, ...) {
    standardGeneric("biomarker")
  },
  valueClass = c("numeric", "array")
)

## DualEndpoint ----

#' @describeIn biomarker
#'
#' @aliases biomarker-DualEndpoint
#' @export
#'
setMethod(
  f = "biomarker",
  signature = signature(
    xLevel = "integer",
    model = "DualEndpoint",
    samples = "Samples"
  ),
  def = function(xLevel, model, samples, ...) {
    assert_integer(
      xLevel,
      lower = 1,
      upper = ncol(samples@data$betaW),
      any.missing = FALSE,
      min.len = 1
    )

    samples@data$betaW[, xLevel]
  }
)

# gain ----

## generic ----

#' Compute Gain Values based on Pseudo DLE and a Pseudo Efficacy Models and
#' Using Optional Samples.
#'
#' @details This function computes the gain values for a given dose level,
#' pseudo DLE and Efficacy models as well as a given DLE and Efficacy samples.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param dose (`number` or `numeric`)\cr the dose which is targeted.
#'   The following recycling rule applies when samples are not missing: vectors
#'   of size 1 will be recycled to the size of the sample. Otherwise, `dose`
#'   must have the same size as the sample.
#' @param model_dle (`ModelTox`)\cr pseudo DLE (dose-limiting events)/toxicity
#'   model.
#' @param samples_dle (`Samples`)\cr the samples of model's
#'   parameters that will be used to compute toxicity probabilities. Can also be
#'   missing for some models.
#' @param model_eff (`ModelEff`)\cr the efficacy model with pseudo data prior.
#' @param samples_eff (`Samples`)\cr samples of model's parameters that will be
#'   used to compute expected efficacy values. Can also be missing for some
#'   models.
#' @param ... not used.
#'
#' @return The gain values.
#'
#' @export
#' @example examples/Model-method-gain.R
#'
setGeneric(
  name = "gain",
  def = function(dose, model_dle, samples_dle, model_eff, samples_eff, ...) {
    standardGeneric("gain")
  },
  valueClass = "numeric"
)

## ModelTox-ModelEff ----

#' @describeIn gain
#'
#' @aliases gain-ModelTox-ModelEff
#' @export
#'
setMethod(
  f = "gain",
  signature = signature(
    dose = "numeric",
    model_dle = "ModelTox",
    samples_dle = "Samples",
    model_eff = "ModelEff",
    samples_eff = "Samples"
  ),
  definition = function(dose, model_dle, samples_dle, model_eff, samples_eff, ...) {
    dle <- prob(dose, model_dle, samples_dle)
    eff <- efficacy(dose, model_eff, samples_eff)
    assert_common_length(dle, eff)
    eff / (1 + (dle / (1 - dle)))
  }
)

## ModelTox-ModelEff-noSamples----

#' @describeIn gain Compute the gain value for a given dose level, pseudo DLE
#'   and Efficacy models without DLE and the Efficacy samples.
#'
#' @aliases gain-ModelTox-Effloglog-noSamples
#' @export
#' @example examples/Model-method-gainNoSamples.R
#'
setMethod(
  f = "gain",
  signature = signature(
    dose = "numeric",
    model_dle = "ModelTox",
    samples_dle = "missing",
    model_eff = "Effloglog",
    samples_eff = "missing"
  ),
  definition = function(dose, model_dle, model_eff, ...) {
    dle <- prob(dose, model_dle)
    eff <- efficacy(dose, model_eff)
    assert_common_length(dle, eff)
    eff / (1 + (dle / (1 - dle)))
  }
)

# update ----

## ModelPseudo ----

#' Update method for the [`ModelPseudo`] model class. This is a method to update
#' the model class slots (estimates, parameters, variables and etc.), when the
#' new data (e.g. new observations of responses) are available. This method is
#' mostly used to obtain new modal estimates for pseudo model parameters.
#'
#' @param object (`ModelPseudo`)\cr the model to update.
#' @param data (`Data`)\cr all currently available of data.
#' @param ... not used.
#'
#' @return the new [`ModelPseudo`] class object.
#'
#' @aliases update-ModelPseudo
#' @export
#' @example examples/Model-method-update.R
#'
setMethod(
  f = "update",
  signature = signature(
    object = "ModelPseudo"
  ),
  definition = function(object, data, ...) {
    assert_class(data, "Data")

    constructor_name <- class(object)
    arg_names <- setdiff(formalArgs(constructor_name), "data")
    do.call(
      constructor_name,
      c(h_slots(object, arg_names), list(data = data))
    )
  }
)
