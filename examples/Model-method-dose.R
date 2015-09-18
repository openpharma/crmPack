
# create some data
data <- Data(x =c (0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
             y = c(0, 0, 0, 0, 0, 0, 1, 0),
             cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
             doseGrid = c(0.1, 0.5, 1.5, 3, 6,
                          seq(from=10, to=80, by=2)))

# Initialize a  model
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=matrix(c(1, -0.5, -0.5, 1),
                                      nrow=2),
                           refDose=56)

# Get samples from posterior
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Posterior for the dose achieving Prob(DLE) = 0.45
TD45 <- dose(prob=0.45,model=model,samples=samples)

