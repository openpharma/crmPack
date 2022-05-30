h_get_mcmc_options <- function(samples = 4, burnin = 50, fixed = TRUE) {
  if (fixed) {
    rng_kind <- "Mersenne-Twister"
    rng_seed <- 1
  } else {
    rng_kind <- NA
    rng_seed <- NA
  }

  McmcOptions(
    burnin = burnin,
    samples = samples,
    rng_kind = rng_kind,
    rng_seed = rng_seed
  )
}
