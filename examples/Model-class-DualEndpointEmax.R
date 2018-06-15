
model <- DualEndpointEmax(E0 = c(0, 100),
                          Emax = c(0, 500),
                          ED50 = c(10,200),
                          refDoseEmax = 1000,
                          mu = c(0, 1),
                          Sigma = matrix(c(1, 0, 0, 1), nrow=2),
                          sigma2W = c(a=0.1, b=0.1),
                          rho = c(a=1, b=1))


