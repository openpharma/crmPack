h_get_mcmc_options <- function(samples = 4L, burnin = 50L, fixed = TRUE) {
  rng_kind <- "Mersenne-Twister"
  if (fixed) {
    rng_seed <- 1L
  } else {
    rng_seed <- as.integer(floor(runif(1, max = .Machine$integer.max)))
  }

  McmcOptions(
    burnin = burnin,
    samples = samples,
    rng_kind = rng_kind,
    rng_seed = rng_seed
  )
}
