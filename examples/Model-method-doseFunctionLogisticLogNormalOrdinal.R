data_ordinal <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()
suppressWarnings({
  samples <- mcmc(data_ordinal, model, options)
})

doseFunction(model, alpha1 = samples@data$alpha2, beta = samples@data$beta, grade = 1L)(x = 0.75)
doseFunction(model, alpha2 = samples@data$alpha2, beta = samples@data$beta, grade = 2L)(x = 0.25)
