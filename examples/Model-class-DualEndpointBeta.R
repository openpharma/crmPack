my_model <- DualEndpointBeta(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  ref_dose = 10,
  use_log_dose = TRUE,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  E0 = c(0, 100),
  Emax = c(0, 500),
  delta1 = c(0, 5),
  mode = c(1, 15),
  ref_dose_beta = 1000
)
