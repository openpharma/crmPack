model <- .DefaultLogisticLogNormalOrdinal()
ordinal_data <- .DefaultDataOrdinal()
options <- .DefaultMcmcOptions()
samples <- mcmc(ordinal_data, model, options)

grade1_fit <- fit(samples, model, ordinal_data, grade = 1L)
grade2_fit <- fit(samples, model, ordinal_data, grade = 2L)
