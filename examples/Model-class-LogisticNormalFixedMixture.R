
model <- LogisticNormalFixedMixture(components = 
                                      list(comp1 = list(mean = c(-0.85, 1),
                                                        cov = matrix(c(1, -0.5, -0.5, 1), 
                                                                     nrow = 2)),
                                           comp2 = list(mean = c(1, 1.5),
                                                        cov = matrix(c(1.2, -0.45, -0.45, 0.6), 
                                                                     nrow = 2))),
                                    weights = c(0.3,0.7),
                                    refDose = 50)


