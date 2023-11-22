ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
mcmc_options <- .DefaultMcmcOptions()

samples <- mcmc(ordinal_data, ordinal_model, mcmc_options)
