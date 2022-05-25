#' @include Model-class.R
#' @include Samples-class.R
NULL

# generic ----

## dose ----

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
#'   This must be a scalar if number of samples in `samples` is greater than
#'   one (i.e. `sampleSize(samples@options) > 1`). It can be a vector of any
#'   finite length, if there is only one sample in `samples`, or `samples` are
#'   not used at all, as e.g. in case of pseudo DLE
#'   (dose-limiting events)/toxicity model.
#' @param model (`GeneralModel` or `ModelPseudo`)\cr the model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute the resulting doses.
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

## doseFunction ----

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

## prob ----

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
#'   This must be a scalar if number of samples in `samples` is greater than
#'   one (i.e. `sampleSize(samples@options) > 1`). It can be a vector of any
#'   finite length, if there is only one sample in `samples`, or `samples` are
#'   not used at all, as e.g. in case of pseudo DLE
#'   (dose-limiting events)/toxicity model.
#' @param model (`GeneralModel` or `ModelTox`)\cr the model for single agent
#'   dose escalation or pseudo DLE (dose-limiting events)/toxicity model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute toxicity probabilities.
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

## probFunction ----

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

## efficacy ----

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
#'   This must be a scalar if number of samples in `samples` is greater than
#'   one (i.e. `sampleSize(samples@options) > 1`). It can be a vector of any
#'   finite length, if there is only one sample in `samples`, or `samples` are
#'   not used at all, as e.g. in case of pseudo DLE
#'   (dose-limiting events)/toxicity model.
#' @param model (`ModelEff`)\cr the efficacy model with pseudo data prior.
#' @param samples (`Samples`)\cr samples of model's parameters that will be
#'   used to compute toxicity probabilities.
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

## efficacyFunction ----

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

# GeneralModel ----

## doseFunction ----

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

## probFunction ----

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

# ModelPseudo ----

## doseFunction ----

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

# ModelTox ----

## probFunction ----

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

# ModelEff ----

## efficacyFunction ----

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

# Model (TO REMOVE SOON) ----

## dose ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-Model
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "Model",
    samples = "Samples"
  ),
  definition = function(x, model, samples, ...) {
    assert_number(x, lower = 0L, upper = 1L)

    dose_fun <- model@dose
    dose_args_names <- setdiff(formalArgs(dose_fun), "x")
    dose_args <- c(samples@data[dose_args_names], x = x)
    do.call(dose_fun, dose_args)
  }
)

## prob ----

#' @describeIn prob
#'
#' @aliases prob-Model
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "Model",
    samples = "Samples"
  ),
  definition = function(dose, model, samples, ...) {
    assert_number(dose, lower = 0L)
    prob_fun <- model@prob
    prob_args_names <- setdiff(formalArgs(prob_fun), "dose")
    prob_args <- c(samples@data[prob_args_names], dose = dose)
    do.call(prob_fun, prob_args)
  }
)

# LogisticNormal ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticLogNormal ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticLogNormalSub ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- model@ref_dose
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    ((logit(x) - alpha0) / alpha1) + ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- model@ref_dose
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * (dose - ref_dose))
  }
)

# ProbitLogNormal ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((probit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    pnorm(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# ProbitLogNormalRel ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    ((probit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    pnorm(alpha0 + alpha1 * (dose / ref_dose))
  }
)

# LogisticKadane ----

## dose ----

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
    assert_subset(c("rho0", "gamma"), names(samples@data))
    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(rho0))

    num <- gamma * (logit(x) - logit(rho0)) + xmin * (logit(theta) - logit(x))
    num / (logit(theta) - logit(rho0))
  }
)

## prob ----

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
    assert_subset(c("rho0", "gamma"), names(samples@data))
    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(rho0))

    num <- gamma * logit(rho0) - xmin * logit(theta) + (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

# LogisticKadaneBetaGamma ----

## dose ----

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
    assert_subset(c("rho0", "gamma"), names(samples@data))
    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(rho0))

    num <- gamma * (logit(x) - logit(rho0)) + xmin * (logit(theta) - logit(x))
    num / (logit(theta) - logit(rho0))
  }
)

## prob ----

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
    assert_subset(c("rho0", "gamma"), names(samples@data))
    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(rho0))

    num <- gamma * logit(rho0) - xmin * logit(theta) + (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

# LogisticNormalMixture ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticNormalFixedMixture ----

## dose ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticLogNormalMixture ----

## dose ----

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

## prob ----

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
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    comp <- samples@data$comp
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    sel <- cbind(seq_len(nrow(alpha0)), comp)
    plogis(alpha0[sel] + alpha1[sel] * log(dose / ref_dose))
  }
)

# DualEndpoint ----

## dose ----

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
    assert_subset("betaZ", names(samples@data))
    betaZ <- samples@data$betaZ
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(betaZ))

    dose_temp <- (qnorm(x) - betaZ[, 1]) / betaZ[, 2]
    if (model@use_log_dose) {
      exp(dose_temp) * ref_dose
    } else {
      dose_temp * ref_dose
    }
  }
)

## prob ----

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
    assert_subset("betaZ", names(samples@data))
    betaZ <- samples@data$betaZ
    ref_dose <- as.numeric(model@ref_dose)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(betaZ))

    stand_dose <- if (model@use_log_dose) {
      log(dose / ref_dose)
    } else {
      dose / ref_dose
    }
    pnorm(betaZ[, 1] + betaZ[, 2] * stand_dose)
  }
)

# LogisticIndepBeta ----

## dose ----

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
    assert_subset(c("phi1", "phi2"), names(samples@data))
    phi1 <- samples@data$phi1
    phi2 <- samples@data$phi2
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(phi1))

    log_dose <- (log(x / (1 - x)) - phi1) / phi2
    exp(log_dose)
  }
)

## dose-noSamples ----

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
    assert_numeric(x, lower = 0L, upper = 1L, min.len = 1L, any.missing = FALSE)

    model_params <- h_slots(model, c("phi1", "phi2"))
    assert_subset(c("phi1", "phi2"), names(model_params))
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    dose(x, model, samples)
  }
)

## prob ----

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
    assert_subset(c("phi1", "phi2"), names(samples@data))
    phi1 <- samples@data$phi1
    phi2 <- samples@data$phi2
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(phi1))

    log_dose <- log(dose)
    exp(phi1 + phi2 * log_dose) / (1 + exp(phi1 + phi2 * log_dose))
  }
)

## prob-noSamples ----

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
    assert_numeric(dose, lower = 0L, min.len = 1L, any.missing = FALSE)

    model_params <- h_slots(model, c("phi1", "phi2"))
    assert_subset(c("phi1", "phi2"), names(model_params))
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    prob(dose, model, samples)
  }
)

# Effloglog ----

## dose-noSamples ----

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
    theta1 <- model@theta1
    theta2 <- model@theta2
    constant <- model@const
    assert_numeric(x, min.len = 1L, any.missing = FALSE, len = h_null_if_scalar(theta1))

    exp(exp((x - theta1) / theta2)) - constant
  }
)

## efficacy ----

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
    assert_subset(c("theta1", "theta2"), names(samples@data))
    theta1 <- samples@data$theta1
    theta2 <- samples@data$theta2
    constant <- model@const
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(theta1))

    theta1 + theta2 * log(log(dose + constant))
  }
)

## efficacy-noSamples ----

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
    assert_numeric(dose, lower = 0L, min.len = 1L, any.missing = FALSE)

    model_params <- h_slots(model, c("theta1", "theta2"))
    assert_subset(c("theta1", "theta2"), names(model_params))
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    efficacy(dose, model, samples)
  }
)

# EffFlexi ----

## dose ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
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

## efficacy ----

#' @describeIn efficacy compute the expected efficacy at a specified dose level,
#' based on the samples of [`EffFlexi`] model parameters.
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

# OneParExpNormalPrior ----

## dose ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' @aliases dose-OneParExpNormalPrior
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "OneParExpNormalPrior",
    samples = "Samples"
  ),
  definition = function(x, model, samples) {
    assert_subset("alpha", names(samples@data))
    alpha <- samples@data$alpha
    skel_fun_inv <- model@skel_fun_inv
    assert_numeric(x, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha))

    skel_fun_inv(x^(1 / exp(alpha)))
  }
)

## prob ----

#' @describeIn prob
#'
#' @aliases prob-OneParExpNormalPrior
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "OneParExpNormalPrior",
    samples = "Samples"
  ),
  definition = function(dose, model, samples) {
    assert_subset("alpha", names(samples@data))
    alpha <- samples@data$alpha
    skel_fun <- model@skel_fun
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha))

    skel_fun(dose)^exp(alpha)
  }
)

# NOT CLEANED UP YET! ----

# nolint start

##' Compute the biomarker level for a given dose, given model and samples
##'
##' @param dose the dose
##' @param model the \code{\linkS4class{DualEndpoint}} object
##' @param samples the \code{\linkS4class{Samples}} object
##' @param \dots unused
##'
##' @export
##' @keywords methods
setGeneric("biomLevel",
           def=
             function(dose, model, samples, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("biomLevel")
             },
           valueClass="numeric")

##' @param xLevel the grid index of \code{dose}
##' @describeIn biomLevel Here it is very easy, we just return the corresponding
##' column (index \code{xLevel}) of the biomarker samples matrix, since we save
##' that in the samples
##' @example examples/Model-method-biomLevel.R
setMethod("biomLevel",
          signature=
            signature(dose="numeric",
                      model="DualEndpoint",
                      samples="Samples"),
          def=
            function(dose, model, samples, xLevel, ...){

              return(samples@data$betaW[, xLevel])

            })

## ---------------------------------------------------------------------------------
## Compute gain value using a Pseudo DLE and a pseduo Efficacy log-log model
## -------------------------------------------------------------------------------

##' Compute the gain value with a given dose level, given a pseudo DLE model, a DLE sample,
##' a pseudo Efficacy log-log model and a Efficacy sample
##'
##' @param dose the dose
##' @param DLEmodel the \code{\linkS4class{ModelTox}} object
##' @param DLEsamples the \code{\linkS4class{Samples}} object (can also be missing)
##' @param Effmodel the \code{\linkS4class{Effloglog}} or the \code{\linkS4class{EffFlexi}} object
##' @param Effsamples the \code{\linkS4class{Samples}} object (can also be missing)
##' @param \dots unused
##'
##' @export
##' @keywords methods
setGeneric("gain",
           def=
             function(dose,DLEmodel,DLEsamples,Effmodel,Effsamples,...){
               standardGeneric("gain")
             },
           valueClass="numeric")

##' @rdname gain
##' @example examples/Model-method-gain.R
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      DLEsamples="Samples",
                      Effmodel="Effloglog",
                      Effsamples="Samples"),
          def=
            function(dose,DLEmodel,DLEsamples, Effmodel,Effsamples,...){

              DLEret <- prob(dose, DLEmodel, DLEsamples)
              Effret <- efficacy(dose, Effmodel, Effsamples)


              ## return the resulting vector
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })

## ===================================================================


##' @describeIn gain Compute the gain given a dose level, a pseduo DLE model, a DLE sample,
##' the pseudo EffFlexi model and an Efficacy sample
##' @example examples/Model-method-gainFlexi.R
setMethod(
  f = "gain",
  signature = signature(
    dose = "numeric",
    DLEmodel = "ModelTox",
    DLEsamples = "Samples",
    Effmodel = "EffFlexi",
    Effsamples = "Samples"
  ),
  definition = function(dose,
                        DLEmodel,
                        DLEsamples,
                        Effmodel,
                        Effsamples,
                        ...) {
    DLEret <- prob(dose, DLEmodel, DLEsamples)
    Effret <- efficacy(dose, Effmodel, Effsamples)
    Effret / (1 + (DLEret / (1 - DLEret)))
  }
)

##' @describeIn gain Compute the gain value given a dose level, a pseudo DLE model and a pseudo
##' efficacy model of \code{\linkS4class{Effloglog}} class object without DLE and the efficacy sample
##' @example examples/Model-method-gainNoSamples.R
setMethod(
  f = "gain",
  signature = signature(
    dose = "numeric",
    DLEmodel = "ModelTox",
    DLEsamples = "missing",
    Effmodel = "Effloglog",
    Effsamples = "missing"
  ),
  definition = function(dose,
                        DLEmodel,
                        Effmodel,
                        ...) {
    DLEret <- prob(dose, DLEmodel)
    Effret <- efficacy(dose, Effmodel)
    Effret / (1 + (DLEret / (1 - DLEret)))
  }
)

## ------------------------------------------------------------------------------------
## Update Pseduo models object to obtain new modal estimates for pseudo model parameters
## ------------------------------------------------------------------------------------
## Update the 'LogisticIndepBeta' model
## -----------------------------------------------------------------

##' Update method for the 'LogisticIndepBeta'Model class. This is a method to update the modal
##' estimates of the model parameters \eqn{\phi_1} (phi1) and \eqn{\phi_2} (phi2) when new data
##' or new observations of responses are available and added in.
##'
##' @param object the model of \code{\linkS4class{LogisticIndepBeta}} class object
##' @param data all currently available of \code{\linkS4class{Data}} class object
##' @param \dots unused
##' @return the new \code{\linkS4class{LogisticIndepBeta}} class object
##'
##' @example examples/Model-method-updateLogisticIndepBeta.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
            signature(object="LogisticIndepBeta"),
          def=
            function(object,
                     data,
                     ...){
              ##Get Pseudo DLE responses (prior) of the model

              PseudoDLE<-object@binDLE

              ##Get Pseudo DLE weights of the DLE responses of the model
              PseudoDLEweight<-object@DLEweights


              ##Get the corresponding dose levels for the Pseudo DLE responses from the model
              PseudoDLEdose<- object@DLEdose

              ##update the model estimates with data
              model<- LogisticIndepBeta(binDLE=PseudoDLE,DLEweights=PseudoDLEweight,DLEdose=PseudoDLEdose,data=data)

              ##return the updated model
              return(model)
            })

# update-Effloglog ----

#' Updating `Effloglog` Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A method that updates existing [`Effloglog`] object with new data.
#'
#' @param object (`Effloglog`)\cr object you want to update.
#' @param data (`DataDual`)\cr all currently available data or responses.
#' @param ... not used.
#'
#' @return The new, updated [`Effloglog`] object.
#'
#' @aliases update-Effloglog
#' @export
#' @example examples/Model-method-update-Effloglog.R
#'
setMethod(
  f = "update",
  signature = signature(
    object = "Effloglog"
  ),
  definition = function(object,
                        data,
                        ...) {
    Effloglog(
      eff = object@eff,
      eff_dose = object@eff_dose,
      nu = object@nu,
      const = object@const,
      data = data
    )
  }
)

## =================================================================================
## ------------------------------------------------------------------------------------
## Update the 'EffFlexi' model
## -----------------------------------------------------------------

##' Update method for the 'EffFlexi' Model class. This is a method to update
##' estimates both for the flexible form model and the random walk model (see details in
##' \code{\linkS4class{EffFlexi}} class object) when new data
##' or new observations of responses are available and added in.
##'
##' @param object is the model which follow \code{\linkS4class{EffFlexi}} class object
##' @param data all currently available data and responses of \code{\linkS4class{DataDual}}
##' class object
##' @param \dots unused
##' @return the new \code{\linkS4class{EffFlexi}} class object
##'
##' @example examples/Model-method-updateEffFlexi.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
            signature(object="EffFlexi"),
          def=
            function(object,
                     data,
                     ...){
              ##Get Pseudo Eff responses (prior) of the model

              PseudoEff<-object@eff

              ##Get the corresponding dose levels for the Pseudo DLE responses from the model
              PseudoEffdose<- object@eff_dose

              ## Get the initial values of parameters for Sigma2 (if it is not fixed)
              ##OR get the fixed value of sigma2W
              PseudoSigma2<- object@sigma2W


              ## Get the initial values of parameters for Sigma2betaW (if it is not fixed)
              ##OR get the fixed value of sigma2betaW
              PseudoSigma2betaW<- object@sigma2betaW

              ##update the model estimates with data
              model<- EffFlexi(eff=PseudoEff,eff_dose=PseudoEffdose,sigma2W=PseudoSigma2,sigma2betaW=PseudoSigma2betaW,data=data)

              ##return the updated model
              return(model)
            })

# nolint end
