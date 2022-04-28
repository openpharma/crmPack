my_model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  sigma2betaW = 0.01,
  rw1 = TRUE
)
