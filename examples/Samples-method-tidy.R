model <- LogisticLogNormal(
           mean = c(-0.85, 1),
           cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
           ref_dose = 56
         )
data <- Data(
          x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
          y = c(0, 0, 0, 0, 0, 0, 1, 0),
          ID = 1:8,
          cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
          doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from=10, to=80, by=2))
        )
samples <- mcmc(data, model, options)

samples %>% tidy()
