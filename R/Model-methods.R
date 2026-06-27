#' @include Model-class.R
#' @include Samples-class.R
NULL

# show ----

## show-HierarchicalModel ----

#' Show `HierarchicalModel` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Display a brief representation of the [`HierarchicalModel`] object.
#'
#' @param object (`HierarchicalModel`)\cr the object we want to print.
#'
#' @return Invisibly returns the object itself.
#'
#' @aliases show-HierarchicalModel
#' @export
setMethod(
  f = "show",
  signature = signature(object = "HierarchicalModel"),
  def = function(object) {
    arm_names <- names(object@models_to_arms)
    model_classes <- vapply(
      object@models_to_arms,
      function(model) class(model)[1L],
      character(1L)
    )
    pool_names <- names(object@parameter_pools)

    cat(
      "An object of class 'HierarchicalModel'\n",
      "Arms (",
      length(arm_names),
      "): ",
      h_show_hierarchical_names(arm_names),
      "\n",
      sep = ""
    )
    if (length(model_classes) > 0L) {
      cat(
        "Arm models: ",
        paste(
          paste0(names(model_classes), " = ", model_classes),
          collapse = ", "
        ),
        "\n",
        sep = ""
      )
    }
    cat(
      "Exchangeable parameter pools (",
      length(pool_names),
      "): ",
      h_show_hierarchical_names(pool_names),
      "\n",
      sep = ""
    )

    invisible(object)
  }
)

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

#' @describeIn doseFunction Return a dose function for a `GeneralModel`.
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
      options = McmcOptions(samples = NROW(model_params[[1]]))
    )
    function(x, ...) {
      dose(x = x, model = model, samples = samples, ...)
    }
  }
)

## ModelPseudo ----

#' @describeIn doseFunction Return a dose function for a `ModelPseudo`.
#'
#' @aliases doseFunction-ModelPseudo
#' @export
#'
setMethod(
  f = "doseFunction",
  signature = "ModelPseudo",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(
      names(model_params),
      len = length(model_params),
      any.missing = FALSE,
      unique = TRUE
    )

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(x) {
      dose(x = x, model = model, samples = samples)
    }
  }
)

## LogisticLogNormalOrdinal ----

#' @describeIn doseFunction Return a grade-specific dose function for a `LogisticLogNormalOrdinal` model.
#'
#' @param grade (`integer`)\cr the toxicity grade for which the dose function is
#' required
#'
#' @aliases doseFunction-LogisticLogNormalOrdinal
#' @example examples/Model-method-doseFunctionLogisticLogNormalOrdinal.R
#' @export
setMethod(
  f = "doseFunction",
  signature = "LogisticLogNormalOrdinal",
  definition = function(model, grade, ...) {
    model_params <- list(...)
    assert_character(
      names(model_params),
      len = length(model_params),
      any.missing = FALSE,
      unique = TRUE
    )
    assert_integer(grade, lower = 1, len = 1)
    coll <- makeAssertCollection()
    if (!(paste0("alpha", grade) %in% names(model_params))) {
      coll$push(
        paste0(
          "Since grade = ",
          grade,
          ", a parameter named 'alpha",
          grade,
          "' must appear the call"
        )
      )
    }
    reportAssertions(coll)
    # Create dummy intercept columns if necessary
    for (g in seq_along(grade)) {
      if (!(paste0("alpha", g) %in% names(model_params))) {
        model_params[[paste0("alpha", g)]] <- rep(0, length(model_params[[1]]))
      }
    }

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(x) {
      dose(x = x, model = model, samples = samples, grade = grade)
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

#' @describeIn probFunction Return a probability function for a `GeneralModel`.
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
      options = McmcOptions(samples = NROW(model_params[[1]]))
    )
    function(dose, ...) {
      prob(dose = dose, model = model, samples = samples, ...)
    }
  }
)

## ModelTox ----

#' @describeIn probFunction Return a probability function for a `ModelTox`.
#'
#' @aliases probFunction-ModelTox
#' @export
#'
setMethod(
  f = "probFunction",
  signature = "ModelTox",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(
      names(model_params),
      len = length(model_params),
      any.missing = FALSE,
      unique = TRUE
    )

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(dose) {
      prob(dose = dose, model = model, samples = samples)
    }
  }
)

## LogisticLogNormalOrdinal ----

#' @describeIn probFunction Return a grade-specific probability function for a `LogisticLogNormalOrdinal` model.
#'
#' @param grade (`integer`)\cr the toxicity grade for which the dose function is
#' required
#'
#' @aliases probFunction-LogisticLogNormalOrdinal
#' @example examples/Model-method-probFunctionLogisticLogNormalOrdinal.R
#' @export
setMethod(
  f = "probFunction",
  signature = "LogisticLogNormalOrdinal",
  definition = function(model, grade, ...) {
    model_params <- list(...)
    assert_character(
      names(model_params),
      len = length(model_params),
      any.missing = FALSE,
      unique = TRUE
    )
    assert_integer(grade, lower = 1, len = 1)
    coll <- makeAssertCollection()
    if (!(paste0("alpha", grade) %in% names(model_params))) {
      coll$push(
        paste0(
          "Since grade = ",
          grade,
          ", a parameter named 'alpha",
          grade,
          "' must appear the call"
        )
      )
    }
    reportAssertions(coll)
    # Create dummy intercept columns if necessary
    for (g in seq_along(grade)) {
      if (!(paste0("alpha", g) %in% names(model_params))) {
        model_params[[paste0("alpha", g)]] <- rep(0, length(model_params[[1]]))
      }
    }

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = length(model_params[[1]]))
    )
    function(dose) {
      prob(dose = dose, model = model, samples = samples, grade = grade)
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

#' @describeIn efficacyFunction Return an efficacy function for a `ModelEff`.
#'
#' @aliases efficacyFunction-ModelEff
#' @export
#'
setMethod(
  f = "efficacyFunction",
  signature = "ModelEff",
  definition = function(model, ...) {
    model_params <- list(...)
    assert_character(
      names(model_params),
      len = length(model_params),
      any.missing = FALSE,
      unique = TRUE
    )

    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = NROW(model_params[[1]]))
    )
    function(dose) {
      efficacy(dose = dose, model = model, samples = samples)
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
#'   (i.e. `size(samples)`). Otherwise, `x` must have the same size
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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticLogNormalOrdinal ----

#' @describeIn dose compute the dose level reaching a specific target
#'   probability of the occurrence of a DLE (`x`).
#'
#' In the case of a `LogisticLogNormalOrdinal` model, `dose` returns only the
#' probability of toxicity at the given grade or higher
#'
#' @param grade (`integer`)\cr The toxicity grade for which probabilities are required
#'
#' @aliases dose-LogisticLogNormalOrdinal
#' @example examples/Model-method-doseLogisticLogNormalOrdinal.R
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticLogNormalOrdinal",
    samples = "Samples"
  ),
  definition = function(x, model, samples, grade) {
    assert_probabilities(x)
    assert_length(x, len = size(samples))
    assert_integer(
      grade,
      len = 1,
      lower = 1,
      upper = (length(names(samples@data)) - 1)
    )
    a <- paste0("alpha", grade)
    assert_subset(c(a, "beta"), names(samples))

    alpha <- samples@data[[a]]
    beta <- samples@data$beta
    ref_dose <- as.numeric(model@ref_dose)
    exp((logit(x) - alpha) / beta) * ref_dose
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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    ((probit(x) - alpha0) / alpha1) * ref_dose
  }
)

## LogisticLogNormalGrouped ----

#' @describeIn dose method for [`LogisticLogNormalGrouped`] which needs `group`
#'   argument in addition.
#' @param group (`character` or `factor`)\cr for [`LogisticLogNormalGrouped`],
#'   indicating whether to calculate the dose for the `mono` or for
#'   the `combo` arm.
#' @aliases dose-LogisticLogNormalGrouped
#' @export
#'
setMethod(
  f = "dose",
  signature = signature(
    x = "numeric",
    model = "LogisticLogNormalGrouped",
    samples = "Samples"
  ),
  definition = function(x, model, samples, group) {
    assert_probabilities(x)
    assert_subset(c("alpha0", "delta0", "alpha1", "delta1"), names(samples))
    assert_length(x, len = size(samples))
    assert_multi_class(group, c("character", "factor"))
    assert_subset(as.character(group), choices = c("mono", "combo"))
    assert_length(group, len = size(samples))

    alpha0 <- samples@data$alpha0
    delta0 <- samples@data$delta0
    alpha1 <- samples@data$alpha1
    delta1 <- samples@data$delta1
    ref_dose <- as.numeric(model@ref_dose)
    is_combo <- as.integer(group == "combo")
    exp(
      (logit(x) - (alpha0 + is_combo * delta0)) / (alpha1 + is_combo * delta1)
    ) *
      ref_dose
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
    assert_subset(c("rho0", "gamma"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("rho0", "gamma"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset("betaZ", names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset(c("phi1", "phi2"), names(samples))
    assert_length(x, len = size(samples))

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
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = nsamples)
    )
    assert_length(x, len = nsamples)

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
    assert_numeric(x, min.len = 1L, any.missing = FALSE, finite = TRUE)
    theta1 <- model@theta1
    theta2 <- model@theta2
    constant <- model@const
    assert_scalar(theta1)
    assert_scalar(theta2)
    assert_scalar(constant)

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
    assert_subset("ExpEff", names(samples))

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
    assert_subset("alpha", names(samples))
    assert_length(x, len = size(samples))

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
    assert_subset("theta", names(samples))
    assert_length(x, len = size(samples))

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
#'   (i.e. `size(samples)`). Otherwise, `dose` must have the same
#'   size as the sample.
#' @param model (`GeneralModel` or `ModelTox`)\cr the model for single agent
#'   dose escalation or pseudo DLE (dose-limiting events)/toxicity model.
#' @param samples (`Samples`)\cr the samples of model's parameters that will be
#'   used to compute toxicity probabilities. Can also be missing for some models.
#' @param ... model specific parameters when `samples` are not used.
#'
#' @return A `proportion` or `numeric` vector with the toxicity probabilities,
#'   or a numeric matrix for methods that evaluate multiple dose combinations at
#'   once. If non-scalar `samples` were used, then every element in the returned
#'   vector corresponds to one element of a sample. Hence, in this case, the
#'   output vector is of the same length as the sample vector. If scalar
#'   `samples` were used or no `samples` were used, e.g. for pseudo
#'   DLE/toxicity `model`, then the output is of the same length as the length
#'   of the `dose`. For matrix-valued dose inputs, the returned matrix contains
#'   one column per dose combination. In the case of `LogisticLogNormalOrdinal`,
#'   the probabilities relate to toxicities of grade given by `grade`.
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
  valueClass = c("numeric", "list", "matrix")
)

## LogisticNormal ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticNormal` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormal ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticLogNormal` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormalSub ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticLogNormalSub` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- model@ref_dose
    plogis(alpha0 + alpha1 * (dose - ref_dose))
  }
)

## ProbitLogNormal ----

#' @describeIn prob Calculate toxicity probabilities for a `ProbitLogNormal` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    pnorm(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## ProbitLogNormalRel ----

#' @describeIn prob Calculate toxicity probabilities for a `ProbitLogNormalRel` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    pnorm(alpha0 + alpha1 * (dose / ref_dose))
  }
)

## LogisticLogNormalGrouped ----

#' @describeIn prob method for [`LogisticLogNormalGrouped`] which needs `group`
#'   argument in addition.
#' @param group (`character` or `factor`)\cr for [`LogisticLogNormalGrouped`],
#'   indicating whether to calculate the probability for the `mono` or for
#'   the `combo` arm.
#' @aliases prob-LogisticLogNormalGrouped
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticLogNormalGrouped",
    samples = "Samples"
  ),
  definition = function(dose, model, samples, group, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "delta0", "alpha1", "delta1"), names(samples))
    assert_length(dose, len = size(samples))
    assert_multi_class(group, c("character", "factor"))
    assert_subset(as.character(group), choices = c("mono", "combo"))
    assert_length(group, len = size(samples))

    alpha0 <- samples@data$alpha0
    delta0 <- samples@data$delta0
    alpha1 <- samples@data$alpha1
    delta1 <- samples@data$delta1
    ref_dose <- as.numeric(model@ref_dose)
    is_combo <- as.integer(group == "combo")
    plogis(
      (alpha0 + is_combo * delta0) +
        (alpha1 + is_combo * delta1) * log(dose / ref_dose)
    )
  }
)

## TwoDrugsCombo ----

#' Extract Single-Agent Samples from Combo Samples
#'
#' @description
#' Converts posterior draws from a [`TwoDrugsCombo`] fit back to the sample
#' shape expected by one of its single-agent models. Shared scalar sample names,
#' such as `alpha0` and `alpha1`, are stored as matrices in combo samples and
#' sliced by drug. Parameters that are already model-specific matrix-valued
#' samples, such as mixture-model parameters, are kept intact.
#'
#' @param samples (`Samples`)\cr combo model samples.
#' @param model (`TwoDrugsCombo`)\cr combo model.
#' @param drug_index (`integer`)\cr index of the single-agent model to extract.
#'
#' @return A [`Samples`] object for the requested single-agent model.
#' @keywords internal
h_prob_two_drugs_combo_single_samples <- function(samples, model, drug_index) {
  single_samples <- lapply(
    model@single_models[[drug_index]]@sample,
    function(sample_name) {
      sample_value <- samples@data[[sample_name]]
      sample_model_indices <- which(vapply(
        model@single_models,
        function(single_model) sample_name %in% single_model@sample,
        logical(1L)
      ))
      sample_index <- match(drug_index, sample_model_indices)

      if (
        is.matrix(sample_value) &&
          ncol(sample_value) == length(sample_model_indices)
      ) {
        sample_value[, sample_index]
      } else {
        sample_value
      }
    }
  )
  names(single_samples) <- model@single_models[[drug_index]]@sample
  Samples(data = single_samples, options = samples@options)
}

#' Evaluate Single-Agent Toxicity Probabilities in a Combo Model
#'
#' @description
#' Computes the monotherapy toxicity contribution for one drug by delegating to
#' that drug's own [prob()] method. This keeps the combo probability calculation
#' aligned with each single-agent link function and dose transformation.
#'
#' @inheritParams h_prob_two_drugs_combo_single_samples
#' @param dose (`matrix`)\cr combo dose combinations, one row per combination.
#'
#' @return Numeric matrix with one row per posterior sample and one column per
#'   dose combination.
#' @keywords internal
#'
h_prob_two_drugs_combo_single_prob <- function(
  dose,
  model,
  samples,
  drug_index
) {
  single_model <- model@single_models[[drug_index]]
  single_samples <- h_prob_two_drugs_combo_single_samples(
    samples = samples,
    model = model,
    drug_index = drug_index
  )

  vapply(
    dose[, drug_index],
    function(single_dose) {
      prob(
        dose = rep(single_dose, size(single_samples)),
        model = single_model,
        samples = single_samples
      )
    },
    numeric(size(single_samples))
  )
}

#' Evaluate a Single-Agent Dose Normalization
#'
#' @description
#' Reuses the dose-normalization expression inferred from a single-agent JAGS
#' data model, such as `x / ref_dose` or `x - ref_dose`, and evaluates it for R
#' dose inputs. The result is used for the combo interaction term so runtime
#' probabilities match the JAGS model used for MCMC.
#'
#' @param dose (`numeric`)\cr single-agent doses.
#' @param single_model (`GeneralModel`)\cr single-agent model.
#'
#' @return Numeric vector of normalized doses.
#' @keywords internal
#'
h_prob_two_drugs_combo_normalized_dose <- function(dose, single_model) {
  normalized_expr <- h_two_drugs_combo_normalized_dose_expr(
    body(single_model@datamodel),
    list()
  )
  specs <- h_two_drugs_combo_single_model_specs(
    single_model,
    from_prior = FALSE
  )
  eval_env <- list2env(
    c(
      specs,
      list(
        x = dose,
        i = seq_along(dose)
      )
    ),
    parent = baseenv()
  )
  normalized <- eval(normalized_expr, envir = eval_env)
  assert_numeric(normalized, any.missing = FALSE, len = length(dose))
  normalized
}

#' Calculate Two-Drug Combo Toxicity Probabilities
#'
#' @description
#' Combines delegated single-agent toxicity probabilities with the combo
#' interaction term. Both the monotherapy probabilities and the interaction
#' covariate follow the corresponding single-agent model definitions.
#'
#' @param dose (`numeric` or `matrix`)\cr one or more combo dose combinations.
#' @inheritParams h_prob_two_drugs_combo_single_samples
#'
#' @return Numeric vector for one dose combination, otherwise numeric matrix
#'   with one row per posterior sample and one column per dose combination.
#' @keywords internal
#'
h_prob_two_drugs_combo <- function(dose, model, samples) {
  assert_subset("eta", names(samples))

  eta <- samples@data$eta

  if (!is.matrix(dose)) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, len = 2L)
    assert_names(names(dose), identical.to = model@drug_names)
    dose <- matrix(dose, nrow = 1L, dimnames = list(NULL, model@drug_names))
  } else {
    assert_matrix(dose, mode = "numeric", any.missing = FALSE, ncols = 2L)
    if (is.null(colnames(dose))) {
      colnames(dose) <- model@drug_names
    } else {
      assert_names(colnames(dose), permutation.of = model@drug_names)
      dose <- dose[, model@drug_names, drop = FALSE]
    }
    assert_true(all(dose >= 0))
  }

  p1 <- h_prob_two_drugs_combo_single_prob(
    dose = dose,
    model = model,
    samples = samples,
    drug_index = 1L
  )
  p2 <- h_prob_two_drugs_combo_single_prob(
    dose = dose,
    model = model,
    samples = samples,
    drug_index = 2L
  )
  p0 <- p1 + p2 - p1 * p2
  normalized_dose <- do.call(
    cbind,
    lapply(seq_along(model@single_models), function(drug_index) {
      h_prob_two_drugs_combo_normalized_dose(
        dose = dose[, drug_index],
        single_model = model@single_models[[drug_index]]
      )
    })
  )
  interaction <- eta %o% apply(normalized_dose, 1L, prod)
  odds <- (p0 / (1 - p0)) * exp(interaction)
  odds <- pmin(odds, .Machine$double.xmax / 2)
  probs <- odds / (1 + odds)

  assert_true(all(is.finite(probs)))

  if (ncol(probs) == 1L) {
    as.numeric(probs)
  } else {
    probs
  }
}

#' @describeIn prob method for [`TwoDrugsCombo`] for a single dose
#'   combination provided as a named numeric vector.
#' @aliases prob-TwoDrugsCombo
#' @export
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "TwoDrugsCombo",
    samples = "Samples"
  ),
  definition = function(dose, model, samples, ...) {
    h_prob_two_drugs_combo(
      dose = dose,
      model = model,
      samples = samples
    )
  }
)

#' @describeIn prob method for [`TwoDrugsCombo`] for one or more dose
#'   combinations provided in the rows of a numeric matrix.
#' @aliases prob-TwoDrugsCombo-matrix
#' @export
setMethod(
  f = "prob",
  signature = signature(
    dose = "matrix",
    model = "TwoDrugsCombo",
    samples = "Samples"
  ),
  definition = function(dose, model, samples, ...) {
    h_prob_two_drugs_combo(
      dose = dose,
      model = model,
      samples = samples
    )
  }
)

## LogisticKadane ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticKadane` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("rho0", "gamma"), names(samples))
    assert_length(dose, len = size(samples))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma *
      logit(rho0) -
      xmin * logit(theta) +
      (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

## LogisticKadaneBetaGamma ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticKadaneBetaGamma` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("rho0", "gamma"), names(samples))
    assert_length(dose, len = size(samples))

    rho0 <- samples@data$rho0
    gamma <- samples@data$gamma
    theta <- model@theta
    xmin <- model@xmin
    num <- gamma *
      logit(rho0) -
      xmin * logit(theta) +
      (logit(theta) - logit(rho0)) * dose
    plogis(num / (gamma - xmin))
  }
)

## LogisticNormalMixture ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticNormalMixture` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticNormalFixedMixture ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticNormalFixedMixture` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    ref_dose <- as.numeric(model@ref_dose)
    plogis(alpha0 + alpha1 * log(dose / ref_dose))
  }
)

## LogisticLogNormalMixture ----

#' @describeIn prob Calculate toxicity probabilities for a `LogisticLogNormalMixture` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("alpha0", "alpha1"), names(samples))
    assert_length(dose, len = size(samples))

    alpha0 <- samples@data$alpha0
    alpha1 <- samples@data$alpha1
    comp <- samples@data$comp
    ref_dose <- as.numeric(model@ref_dose)
    sel <- cbind(seq_along(comp), comp)
    plogis(alpha0[sel] + alpha1[sel] * log(dose / ref_dose))
  }
)

## DualEndpoint ----

#' @describeIn prob Calculate toxicity probabilities for a `DualEndpoint` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("betaZ", names(samples))
    assert_length(dose, len = size(samples))

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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset(c("phi1", "phi2"), names(samples))
    assert_length(dose, len = size(samples))

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
  definition = function(dose, model, ...) {
    model_params <- h_slots(model, c("phi1", "phi2"))
    nsamples <- length(model_params[[1]])
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = nsamples)
    )

    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_length(dose, len = nsamples)

    prob(dose, model, samples)
  }
)

## OneParLogNormalPrior ----

#' @describeIn prob Calculate toxicity probabilities for a `OneParLogNormalPrior` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("alpha", names(samples))
    assert_length(dose, len = size(samples))

    alpha <- samples@data$alpha
    skel_fun <- model@skel_fun
    skel_fun(dose)^exp(alpha)
  }
)

## OneParExpPrior ----

#' @describeIn prob Calculate toxicity probabilities for a `OneParExpPrior` model.
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
  definition = function(dose, model, samples, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_subset("theta", names(samples))
    assert_length(dose, len = size(samples))

    theta <- samples@data$theta
    skel_fun <- model@skel_fun
    skel_fun(dose)^theta
  }
)

## LogisticLogNormalOrdinal ----

#' Calculate a grade-specific probability of toxicity for a given dose.
#' @describeIn prob Calculate grade-specific toxicity probabilities for a `LogisticLogNormalOrdinal` model.
#'
#' @param grade (`integer` or `integer_vector`)\cr The toxicity grade for which probabilities are required
#' @param cumulative (`flag`)\cr Should the returned probability be cumulative
#' (the default) or grade-specific?
#' @aliases prob-LogisticLogNormalOrdinal
#' @export
#'
setMethod(
  f = "prob",
  signature = signature(
    dose = "numeric",
    model = "LogisticLogNormalOrdinal",
    samples = "Samples"
  ),
  definition = function(dose, model, samples, grade, cumulative = TRUE, ...) {
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_integer(
      grade,
      min.len = 1,
      max.len = length(model@params@mean) - 1,
      lower = 0,
      upper = length(model@params@mean) - 1
    )
    assert_subset(
      names(samples),
      c(paste0("alpha", 0:(length(model@params@mean) - 1)), "beta")
    )
    assert_length(dose, len = size(samples))
    assert_flag(cumulative)

    rv <- lapply(
      grade,
      function(g) {
        alpha <- samples@data[[paste0("alpha", g)]]
        beta <- samples@data$beta
        ref_dose <- as.numeric(model@ref_dose)

        cumulative_prob <- plogis(alpha + beta * log(dose / ref_dose))
        if (cumulative | g == as.integer(length(model@params@mean) - 1)) {
          return(cumulative_prob)
        }

        # Calculate grade-specific probabilities
        alpha0 <- samples@data[[paste0("alpha", g + 1)]]
        grade_prob <- cumulative_prob -
          plogis(alpha0 + beta * log(dose / ref_dose))
        grade_prob
      }
    )
    if (length(rv) == 1) {
      rv[[1]]
    } else {
      names(rv) <- as.character(grade)
      rv
    }
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
#' @param dose (`numeric`)\cr the dose which is targeted.
#'   The following recycling rule applies when `samples` is not missing: vectors
#'   of size 1 will be recycled to the size of the sample
#'   (i.e. `size(samples)`). Otherwise, `dose` must have the same
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
    assert_subset(c("theta1", "theta2"), names(samples))
    assert_length(dose, len = size(samples))

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
    samples <- Samples(
      data = model_params,
      options = McmcOptions(samples = nsamples)
    )

    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_length(dose, len = nsamples)

    efficacy(dose, model, samples)
  }
)

## EffFlexi ----

#' @describeIn efficacy compute the expected efficacy at a specified dose level,
#' based on the samples of [`EffFlexi`] model parameters. If a given dose in
#' the `dose` vector is from outside of the dose grid range, the `NA_real` is
#' returned for this dose and the warning is thrown.
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
    n_samples <- size(samples)
    assert_numeric(dose, lower = 0L, any.missing = FALSE, min.len = 1L)
    assert_true(model@data@nGrid >= 1L)
    assert_subset("ExpEff", names(samples))
    assert_length(dose, len = n_samples)

    dose_grid <- model@data@doseGrid
    dose_level <- match_within_tolerance(dose, dose_grid)
    dose[which(!is.na(dose_level))] <- dose_grid[stats::na.omit(dose_level)]

    # linear interpolation, NA for doses that are outside of the dose_grid range.
    samples_eff <- samples@data$ExpEff
    eff <- if (length(dose) == n_samples) {
      sapply(seq_len(n_samples), function(s) {
        stats::approx(dose_grid, samples_eff[s, ], xout = dose[s])$y
      })
    } else {
      eff <- apply(samples_eff, 1, function(s) {
        stats::approx(dose_grid, s, xout = dose)$y
      })
      as.vector(eff)
    }

    if (any(is.na(eff))) {
      warning(
        paste(
          "At least one dose out of",
          paste(dose, collapse = ", "),
          "is outside of the dose grid range"
        )
      )
    }
    eff
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

#' @describeIn biomarker Extract biomarker values for a `DualEndpoint` model.
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

#' @describeIn gain Compute gain values from toxicity and efficacy model samples.
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
  definition = function(
    dose,
    model_dle,
    samples_dle,
    model_eff,
    samples_eff,
    ...
  ) {
    dle <- prob(dose, model_dle, samples_dle)
    eff <- efficacy(dose, model_eff, samples_eff)
    assert_length(dle, len = length(eff))
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
    assert_length(dle, len = length(eff))
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

# tidy ----

# HierarchicalModel

#' Tidy Method for the [`HierarchicalModel`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that tidies a [`HierarchicalModel`] object.
#'
#' @return The [`list`] of [`tibble`] objects.
#'
#' @aliases tidy-HierarchicalModel
#' @rdname tidy
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "HierarchicalModel"),
  definition = function(x, ...) {
    models_to_arms <- lapply(
      names(x@models_to_arms),
      function(arm_name) {
        model <- x@models_to_arms[[arm_name]]
        tibble::tibble(
          Arm = arm_name,
          ModelClass = class(model)[1L],
          Model = list(tidy(model))
        )
      }
    ) %>%
      dplyr::bind_rows()

    parameter_pools <- lapply(
      names(x@parameter_pools),
      function(pool_name) {
        pool <- x@parameter_pools[[pool_name]]
        tibble::tibble(
          Pool = pool_name,
          Arm = names(pool),
          Parameter = unname(unlist(pool, use.names = FALSE))
        )
      }
    ) %>%
      dplyr::bind_rows()

    pool_correlations <- tibble::tibble(
      PoolCorrelation = names(x@pool_correlations),
      Pools = unname(x@pool_correlations)
    )

    pool_priors <- tibble::tibble(
      Pool = names(x@pool_priors),
      Prior = unname(x@pool_priors)
    )

    list(
      models_to_arms = models_to_arms,
      parameter_pools = parameter_pools,
      pool_correlations = pool_correlations,
      pool_priors = pool_priors
    ) %>%
      h_tidy_class(x)
  }
)

# LogisticIndepBeta

#' Tidy Method for the [`LogisticIndepBeta`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that tidies a [`LogisticIndepBeta`] object.
#'
#' @return The [`list`] of [`tibble`] objects.
#'
#' @aliases tidy-LogisticIndepBeta
#' @rdname tidy
#' @method tidy LogisticIndepBeta
#' @export
#' @example examples/LogisticIndepBeta-method-tidy.R
#'
setMethod(
  f = "tidy",
  signature = signature(x = "LogisticIndepBeta"),
  definition = function(x, ...) {
    start <- callNextMethod()
    # N$DLEweights Dose$DLEdose Tox$binDLE
    pseudoData <- tibble::tibble(
      Dose = dplyr::pull(start$DLEdose),
      N = dplyr::pull(start$DLEweights),
      Tox = dplyr::pull(start$binDLE)
    )
    params <- tibble::tibble(
      Param = c("Phi1", "Phi2"),
      mean = c(dplyr::pull(start$phi1), dplyr::pull(start$phi2)),
      cov = as.list(start$Pcov)
    )
    list(
      pseudoData = pseudoData,
      data = start$data,
      params = params
    ) %>%
      h_tidy_class(x)
  }
)

# Effloglog

#' Tidy Method for the [`Effloglog`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that tidies a [`Effloglog`] object.
#'
#' @return The [`list`] of [`tibble`] objects.
#'
#' @aliases tidy-Effloglog
#' @rdname tidy
#' @method tidy Edffloglog
#' @export
#' @example examples/Effloglog-method-tidy.R
#'
setMethod(
  f = "tidy",
  signature = signature(x = "Effloglog"),
  definition = function(x, ...) {
    start <- callNextMethod()
    pseudoData <- tibble::tibble(
      Dose = dplyr::pull(start$eff_dose),
      Response = dplyr::pull(start$eff)
    )
    params <- tibble::tibble(
      Param = c("theta1", "theta2"),
      mean = c(dplyr::pull(start$theta1), dplyr::pull(start$theta2)),
      cov = as.list(start$Pcov)
    )
    list(
      pseudoData = pseudoData,
      data = start$data,
      params = params
    ) %>%
      h_tidy_class(x)
  }
)
