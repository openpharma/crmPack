ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()

\donttest{
ordinal_samples <- mcmc(ordinal_data, ordinal_model, options)

recommend(
  nextBest = NextBestOrdinal(2L, .DefaultNextBestNCRM()),
  samples = ordinal_samples,
  doselimit = Inf,
  model = ordinal_model,
  data = ordinal_data
)
}
