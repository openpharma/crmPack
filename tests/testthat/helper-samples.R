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

h_samples_dual_endpoint_rw <- function() {
  betaW <- matrix( # nolintr
    c(
      50.6, 50.1, 50.1, 49.8, 49.2, 48, 48.4, 48.8, 48.5, 48.8, 48.9, 49.4,
      50, 50.1, 50.4, 50.8, 51, 51, 51.3, 51.3, 50.6, 50, 49.4, 50.2,
      49.7, 50.8, 51.4, 51.7, 51.9, 51, 51.3, 50.9, 49.9, 51.1, 52.0, 52.0,
      50.4, 50.2, 50.6, 50.4, 50, 50, 49.7, 49.4, 49.1, 48.9, 48.8, 48.3
    ),
    nrow = 4,
    byrow = TRUE
  )

  betaZ <- matrix( # nolintr
    c(-2.639, 0.049, -1.739, 0.037, -2.162, 0.051, -2.671, 0.049),
    nrow = 4,
    byrow = TRUE
  )

  delta <- matrix(
    c(
      -0.155, 1.164, 0.189, 0.210, -0.834, -0.212, -0.008, 0.473, 0.086, 0.222, -0.033,
      0.406, 0.699, 0.204, -0.588, 0.352, -0.682, -0.672, 0.339, -0.520, -0.276, 0.195,
      -0.335, -0.131, 0.711, -0.643, 0.347, -0.515, 0.526, -0.551, -0.730, -0.335, 0.180,
      -0.150, 0.029, 1.291, -0.095, -0.703, -0.133, -0.221, 0.527, 0.368, -0.039, 1.214
    ),
    nrow = 4,
    byrow = TRUE
  )

  h_as_samples(list(betaW = betaW, betaZ = betaZ, delta = delta))
}

h_samples_dual_endpoint_beta <- function(fixed = TRUE) {
  betaW <- matrix( # nolintr
    rep(c(10.022, rep(10, 11)), 4),
    nrow = 4, byrow = TRUE
  )
  betaZ <- matrix( # nolintr
    c(-3.040, 0.062, -2.522, 0.040, -2.392, 0.062, -3.348, 0.081),
    nrow = 4,
    byrow = TRUE
  )

  if (fixed) {
    h_as_samples(list(betaW = betaW, betaZ = betaZ))
  } else {
    E0 <- c(5.73, 3.96, 1.22, 4.24) # nolintr
    Emax <- c(7.91, 8.98, 7.91, 7.02) # nolintr
    precW <- c(0, 0, 0, 0) # nolintr
    rho <- c(-0.48, -0.57, -0.34, -0.11)
    precW <- c(0.0003944, 0.0002646, 0.0005014, 0.0003893) # nolintr
    h_as_samples(
      list(E0 = E0, Emax = Emax, betaW = betaW, betaZ = betaZ, precW = precW, rho = rho)
    )
  }
}
