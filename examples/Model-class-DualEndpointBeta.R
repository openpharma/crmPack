
model <- DualEndpointBeta(E0 = c(0, 100),
                          Emax = c(0, 500),
                          delta1 = c(0, 5),
                          mode = c(1, 15),
                          refDose = 1000,
                          mu = c(0, 1),
                          Sigma = matrix(c(1, 0, 0, 1), nrow=2),
                          sigma2W = c(a=0.1, b=0.1),
                          rho = c(a=1, b=1))


