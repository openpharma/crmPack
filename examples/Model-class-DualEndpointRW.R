

model <- DualEndpointRW(mu = c(0, 1),
                        Sigma = matrix(c(1, 0, 0, 1), nrow=2),
                        sigma2betaW = 0.01,
                        sigma2W = c(a=0.1, b=0.1),
                        rho = c(a=1, b=1),
                        smooth="RW1")


