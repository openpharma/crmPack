#' @include Model-class.R
#' @include Samples-class.R
NULL

# generic ----

## dose ----

#' Computing the Doses for a Given Probability, Model and Samples
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that computes the dose reaching a specific target probability of
#' the occurrence of a DLE. The doses are computed based on the samples
#' of the model parameters.
#'
#' @details The `dose` function computes the doses for given toxicity
#'   probabilities, using samples of the model parameter(s).
#'   If you work with multivariate model parameters, then assume that your model
#'   specific `dose` method receives a samples matrix where the rows correspond
#'   to the sampling index, i.e. the layout is then `nSamples x dimParameter`.
#'
#' @note The [`dose`] and [`prob`] functions are the inverse of each other.
#'
#' @param prob (`number` or `numeric`)\cr the toxicity probability which is
#'   targeted. This must be a scalar if non-scalar `samples` are used.
#'   It can be a vector of any finite length, if `samples` are scalars or
#'   `samples` are not used, as e.g. in case of pseudo DLE
#'   (dose-limiting events)/toxicity model.
#' @param model (`GeneralModel` or `ModelTox`)\cr the model for single agent
#'   dose escalation or pseudo DLE/toxicity model.
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
#' @seealso [`doseFunction`], [`prob`].
#'
#' @example examples/Model-method-dose.R
#' @export
#'
setGeneric(
  name = "dose",
  def = function(prob, model, samples, ...) {
    standardGeneric("dose")
  },
  valueClass = "numeric"
)

## doseFunction ----

#' Getting the Dose Function for a Given Model Type
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function that returns a function that computes the dose reaching a
#' specific target probability, based on the model specific parameters.
#'
#' @param model (`GeneralModel` or `ModelTox`)\cr the model.
#' @param ... model specific parameters.
#'
#' @return A `function` that computes doses for a given toxicity probability
#'   and the model.
#'
#' @seealso [`dose`], [`probFunction`].
#'
#' @example examples/Model-method-doseFunction.R
#' @export
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
#' specified dose level, based on the model parameters.
#'
#' @details The `prob` function computes the probability of toxicity for given
#'   doses, using samples of the model parameter(s).
#'   If you work with multivariate model parameters, then assume that your model
#'   specific `prob` method receives a samples matrix where the rows correspond
#'   to the sampling index, i.e. the layout is then `nSamples x dimParameter`.
#'
#' @note The [`prob`] and [`dose`] functions are the inverse of each other.
#'
#' @param dose (`number` or `numeric`)\cr the dose which is targeted.
#'   This must be a scalar if non-scalar `samples` are used.
#'   It can be a vector of any finite length, if `samples` are scalars or
#'   `samples` are not used, as e.g. in case of pseudo DLE
#'   (dose-limiting events)/toxicity model.
#' @param model (`GeneralModel` or `ModelTox`)\cr the model for single agent
#'   dose escalation or pseudo DLE (dose-limiting events)/toxicity model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute toxicity probabilities.
#' @param ... model specific parameters when `samples` are not used.
#'
#' @return A `number` or `numeric` vector with the toxicity probabilities.
#'   If non-scalar `samples` were used, then every element in the returned vector
#'   corresponds to one element of a sample. Hence, in this case, the output
#'   vector is of the same length as the sample vector. If scalar `samples` were
#'   used or no `samples` were used, e.g. for pseudo DLE/toxicity `model`,
#'   then the output is of the same length as the length of the `dose`.
#'
#' @seealso [`probFunction`], [`dose`].
#'
#' @example examples/Model-method-prob.R
#' @export
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
#' A function that returns a function that computes the toxicity probabilities
#' for a given dose, model and the model parameters.
#'
#' @param model (`GeneralModel` or `ModelTox`)\cr the model.
#' @param ... model specific parameters.
#'
#' @return A `function` that computes toxicity probabilities for a given dose
#'   and the model.
#'
#' @seealso [`prob`], [`doseFunction`].
#'
#' @example examples/Model-method-probFunction.R
#' @export
#'
setGeneric(
  name = "probFunction",
  def = function(model, ...) {
    standardGeneric("probFunction")
  },
  valueClass = "function"
)

# GeneralModel ----

## doseFunction ----

#' @rdname doseFunction
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
    function(prob) {
      dose(prob, model, samples)
    }
  }
)

## probFunction ----

#' @rdname probFunction
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

# ModelTox ----

## doseFunction ----

#' @rdname doseFunction
#'
#' @aliases doseFunction-ModelTox
#' @export
#'
setMethod(
  f = "doseFunction",
  signature = "ModelTox",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(names(model_params), len = length(model_params), any.missing = FALSE, unique = TRUE)

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(prob) {
      dose(prob, model, samples)
    }
  }
)

## probFunction ----

#' @rdname probFunction
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

# Model (TO REMOVE SOON) ----

## dose ----

#' @rdname dose
#'
#' @aliases dose-Model
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "Model",
    samples = "Samples"
  ),
  definition = function(prob, model, samples, ...) {
    assert_number(prob, lower = 0L, upper = 1L)

    dose_fun <- model@dose
    dose_args_names <- setdiff(formalArgs(dose_fun), "prob")
    dose_args <- c(samples@data[dose_args_names], prob = prob)
    do.call(dose_fun, dose_args)
  }
)

## prob ----

#' @rdname prob
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

#' @rdname dose
#'
#' @aliases dose-LogisticNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticNormal",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(prob) - alpha0) / alpha1) * model@ref_dose
  }
)

## prob ----

#' @rdname prob
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
    ref_dose <- model@ref_dose
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticLogNormal ----

## dose ----

#' @rdname dose
#'
#' @aliases dose-LogisticLogNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticLogNormal",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((logit(prob) - alpha0) / alpha1) * model@ref_dose
  }
)

## prob ----

#' @rdname prob
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
    ref_dose <- model@ref_dose
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# LogisticLogNormalSub ----

## dose ----

#' @rdname dose
#'
#' @aliases dose-LogisticLogNormalSub
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticLogNormalSub",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    ((logit(prob) - alpha0) / alpha1) + model@ref_dose
  }
)

## prob ----

#' @rdname prob
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

#' @rdname dose
#'
#' @aliases dose-ProbitLogNormal
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "ProbitLogNormal",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    exp((probit(prob) - alpha0) / alpha1) * model@ref_dose
  }
)

## prob ----

#' @rdname prob
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
    ref_dose <- model@ref_dose
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    pnorm(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

# ProbitLogNormalRel ----

## dose ----

#' @rdname dose
#'
#' @aliases dose-ProbitLogNormalRel
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "ProbitLogNormalRel",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("alpha0", "alpha1"), names(samples@data))
    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    ((probit(prob) - alpha0) / alpha1) * model@ref_dose
  }
)

## prob ----

#' @rdname prob
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
    ref_dose <- model@ref_dose
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    pnorm(alpha0 + alpha1 * (dose / ref_dose))
  }
)

# LogisticLogNormalMixture ----

## dose ----

#' @rdname dose
#'
#' @aliases dose-LogisticLogNormalMixture
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticLogNormalMixture",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    stop("not implemented")
  }
)

## prob ----

#' @rdname prob
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
    ref_dose <- model@ref_dose
    comp <- samples@data$comp
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = h_null_if_scalar(alpha0))

    sel <- cbind(seq_len(nrow(alpha0)), comp)
    plogis(alpha0[sel] + alpha1[sel] * log(dose / ref_dose))
  }
)

# LogisticIndepBeta ----

## dose ----

#' @rdname dose
#'
#' @description Compute the dose level reaching a specific target probability of
#' the occurrence of a DLE, based on the samples of [`LogisticIndepBeta`] model
#' parameters.
#' The [`LogisticIndepBeta`] model is a Pseudo DLE (dose-limiting events)/toxicity.
#'
#' @aliases dose-LogisticIndepBeta
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticIndepBeta",
    samples = "Samples"
  ),
  definition = function(prob, model, samples) {
    assert_subset(c("phi1", "phi2"), names(samples@data))
    phi1 <- samples@data$phi1
    phi2 <- samples@data$phi2
    assert_numeric(prob, lower = 0L, upper = 1, any.missing = FALSE, len = h_null_if_scalar(phi1))

    log_dose <- (log(prob / (1 - prob)) - phi1) / phi2
    exp(log_dose)
  }
)

## prob ----

#' @rdname prob
#'
#' @description Compute toxicity probabilities of the occurrence of a DLE at a
#' specified dose level, based on the samples of [`LogisticIndepBeta`] model
#' parameters.
#' The [`LogisticIndepBeta`] model is a Pseudo DLE (dose-limiting events)/toxicity.
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

# LogisticIndepBeta-noSamples ----

## dose ----

#' @rdname dose
#'
#' @description Compute the dose level reaching a specific target probability of
#' the occurrence of a DLE, based on the [`LogisticIndepBeta`] model
#' parameters. All model parameters (except `prob`) should be present in the `model` object.
#' The [`LogisticIndepBeta`] model is a Pseudo DLE (dose-limiting events)/toxicity.
#'
#' @aliases dose-LogisticIndepBeta-noSamples
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    prob = "numeric",
    model = "LogisticIndepBeta",
    samples = "missing"
  ),
  definition = function(prob, model) {
    assert_numeric(prob, lower = 0L, upper = 1L, min.len = 1L, any.missing = FALSE)

    model_params <- h_slots(model, c("phi1", "phi2"))
    assert_subset(c("phi1", "phi2"), names(model_params))
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    dose(prob, model, samples)
  }
)

## prob ----

#' @rdname prob
#'
#' @description Compute toxicity probabilities of the occurrence of a DLE at a
#' specified dose level, based on the [`LogisticIndepBeta`] model parameters.
#' The [`LogisticIndepBeta`] model is a Pseudo DLE (dose-limiting events)/toxicity.
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
  definition = function(dose, model, ...) {
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

# nolint start

## =============================================================================

## --------------------------------------------------
## Compute the biomarker level for a given dose, given model and samples
## --------------------------------------------------

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

## =============================================================================================
## ---------------------------------------------------------------------------------------------
## Compute the Expected Efficacy based on a given dose, a given pseduo Efficacy log-log model and a given
## efficacy sample
## -----------------------------------------------------------------------------------------------
##' Compute the expected efficacy based on a given dose, a given pseudo Efficacy log-log model and a given
##' efficacy sample
##'
##' @param dose the dose
##' @param model the \code{\linkS4class{Effloglog}} class object
##' @param samples the \code{\linkS4class{Samples}} class object
##' (can also be missing)
##' @param \dots unused
##'
##' @example examples/Model-method-ExpEff.R
##' @export
##' @keywords methods
setGeneric("ExpEff",
           def=
             function(dose,model,samples,...){
               standardGeneric("ExpEff")
             },
           valueClass="numeric")

##' @describeIn ExpEff Method for the Effloglog class
setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="Effloglog",
                      samples="Samples"),
          def=
            function(dose, model, samples, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(EffFun)),
                                  "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              ret <- do.call(EffFun,
                             c(list(dose=dose),
                               samples@data[argNames]))
              ## return the resulting vector
              return(ret)
            })
##======================================================================================

##' @describeIn ExpEff Compute the Expected Efficacy based a given dose and a given Pseudo Efficacy log log model without
##' samples
##' @example examples/Model-method-ExpEffNoSamples.R
setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="Effloglog",
                      samples="missing"),
          def=
            function(dose, model, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ## which arguments, besides the dose, does it need?
              argNames <- setdiff(names(formals(EffFun)),
                                  "dose")
              ## now call the function with dose
              values<-c()
              for (parName in argNames){
                values <- c(values, slot(model,parName))}

              ret <- do.call(EffFun,
                             c(list(dose=dose),values))
              ## return the resulting vector
              return(ret)
            })

##' @describeIn ExpEff Compute the Expected Efficacy based a given dose, Efficacy
##' Flexible model with samples
##' @example examples/Model-method-ExpEffFlexi.R
setMethod("ExpEff",
          signature=
            signature(dose="numeric",
                      model="EffFlexi",
                      samples="Samples"),
          def=
            function(dose, model, samples, ...){
              ## extract the ExpEff function from the model
              EffFun <- slot(model, "ExpEff")
              ret <- EffFun(dose,data=model@data,Effsamples=samples)
              ## return the resulting vector
              return(ret)
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

              ## extract the ExpEff function from the model
              EffFun <- slot(Effmodel, "ExpEff")
              ## which arguments, besides the dose, does it need?
              EffargNames <- setdiff(names(formals(EffFun)),
                                     "dose")
              ## now call the function with dose and with
              ## the arguments taken from the samples
              Effret <- do.call(EffFun,
                                c(list(dose=dose),
                                  Effsamples@data[EffargNames]))

              ## return the resulting vector
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })

## ===================================================================


##' @describeIn gain Compute the gain given a dose level, a pseduo DLE model, a DLE sample,
##' the pseudo EffFlexi model and an Efficacy sample
##' @example examples/Model-method-gainFlexi.R
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      DLEsamples="Samples",
                      Effmodel="EffFlexi",
                      Effsamples="Samples"),
          def=
            function(dose,DLEmodel,DLEsamples, Effmodel,Effsamples,...){

              DLEret <- prob(dose, DLEmodel, DLEsamples)

              ## extract the ExpEff function from the model
              EffFun <- slot(Effmodel, "ExpEff")

              ## now call the function with dose and with
              ## the arguments taken from the samples
              Effret <- EffFun(dose,Effmodel@data,Effsamples)

              ## return the resulting vector
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })


##' @describeIn gain Compute the gain value given a dose level, a pseudo DLE model and a pseudo
##' efficacy model of \code{\linkS4class{Effloglog}} class object without DLE and the efficacy sample
##' @example examples/Model-method-gainNoSamples.R
setMethod("gain",
          signature=
            signature(dose="numeric",
                      DLEmodel="ModelTox",
                      DLEsamples="missing",
                      Effmodel="Effloglog",
                      Effsamples="missing"),
          def=
            function(dose,DLEmodel,Effmodel,...){

              DLEret <- prob(dose, DLEmodel)

              ##extract the ExpEff function from the Eff model
              EffFun <- slot(Effmodel,"ExpEff")
              ##which arguments besides the dose dose it need?
              EffargNames <- setdiff(names(formals(EffFun)),"dose")
              ##now call the function with dose
              Effvalues<-c()
              for (EffparName in EffargNames){
                Effvalues <- c(Effvalues, slot(Effmodel,EffparName))}

              Effret <- do.call(EffFun,
                                c(list(dose=dose),Effvalues))
              Gainret <- Effret/(1+(DLEret/(1-DLEret)))
              return(Gainret)
            })


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

## ------------------------------------------------------------------------------------
## Update the 'Effloglog' model
## -----------------------------------------------------------------

##' Update method for the 'Effloglog' Model class. This is a method to update the modal
##' estimates of the model parameters \eqn{\theta_1} (theta1), \eqn{\theta_2} (theta2)  and \eqn{\nu}
##' (nu, the precision of the efficacy responses) when new data
##' or new observations of responses are available and added in.
##'
##' @param object the \code{\linkS4class{Effloglog}} class object
##' @param data all currently available data or responses of \code{\linkS4class{DataDual}}
##' class object
##' @param \dots unused
##' @return the new \code{\linkS4class{Effloglog}} class object
##'
##' @example examples/Model-method-updateEffloglog.R
##' @export
##' @keywords methods
setMethod("update",
          signature=
            signature(object="Effloglog"),
          def=
            function(object,
                     data,
                     ...){

              ##update the model estimates with data
              model <- Effloglog(Eff=object@Eff,
                                 Effdose=object@Effdose,
                                 nu=object@nu,
                                 c=object@c,
                                 data=data)

              ##return the updated model
              return(model)
            })
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

              PseudoEff<-object@Eff

              ##Get the corresponding dose levels for the Pseudo DLE responses from the model
              PseudoEffdose<- object@Effdose

              ## Get the initial values of parameters for Sigma2 (if it is not fixed)
              ##OR get the fixed value of sigma2
              PseudoSigma2<- object@sigma2


              ## Get the initial values of parameters for Sigma2betaW (if it is not fixed)
              ##OR get the fixed value of sigma2betaW
              PseudoSigma2betaW<- object@sigma2betaW

              ##update the model estimates with data
              model<- EffFlexi(Eff=PseudoEff,Effdose=PseudoEffdose,sigma2=PseudoSigma2,sigma2betaW=PseudoSigma2betaW,data=data)

              ##return the updated model
              return(model)
            })

# nolint end
