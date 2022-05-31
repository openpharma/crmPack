h_as_samples <- function(x, burnin = 50, fixed = TRUE) {
  dim_x <- dim(x[[1]])
  n_samples <- if (is.null(dim_x)) {
    length(x[[1]])
  } else {
    dim_x[1]
  }

  mcmc_options <- h_get_mcmc_options(
    samples = n_samples,
    burnin = burnin,
    fixed = fixed
  )

  Samples(
    data = x,
    options = mcmc_options
  )
}
