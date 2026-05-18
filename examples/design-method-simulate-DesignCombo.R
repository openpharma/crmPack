# nolint start

design_combo <- .DefaultDesignCombo()

true_tox_combo <- function(dose) {
  plogis(-6 + 0.08 * dose[1] + 0.06 * dose[2] + 0.001 * dose[1] * dose[2])
}

options <- McmcOptions(
  burnin = 50,
  step = 1,
  samples = 50,
  rng_kind = "Mersenne-Twister",
  rng_seed = 1
)

my_sims_combo <- simulate(
  design_combo,
  truth = true_tox_combo,
  nsim = 1,
  seed = 819,
  mcmcOptions = options,
  parallel = FALSE
)

# nolint end
