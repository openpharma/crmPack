# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))


model <- LogisticNormal(mean = c(-0.85, 1),
                        cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
                        refDose = 50)

options <- McmcOptions(burnin=100,
                       step=2,
                       samples=1000)

options(error=recover)
mcmc(emptydata, model, options)
