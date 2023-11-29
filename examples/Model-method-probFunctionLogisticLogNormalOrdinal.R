ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
opts <- .DefaultMcmcOptions()
samples <- mcmc(ordinal_data, ordinal_model, opts)

probFunction(
  ordinal_model,
  grade = 2L,
  alpha2 = samples@data$alpha1,
  beta = samples@data$beta
)(50)
