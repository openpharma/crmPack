h_get_mcmc_options <- function(small = FALSE, fixed = FALSE) {
  if (fixed) {
    rng_kind <- "Mersenne-Twister"
    rng_seed <- 1
  } else {
    rng_kind <- NA
    rng_seed <- NA
  }

  if (small) {
    burnin <- 50
    samples <- 4
  } else {
    burnin <- 1000
    samples <- 1000
  }

  McmcOptions(
    burnin = burnin,
    step = 2,
    samples = samples,
    rng_kind = rng_kind,
    rng_seed = rng_seed
  )
}
