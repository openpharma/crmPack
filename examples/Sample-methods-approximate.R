
# Create some data
data <- Data(x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
             y = c(0, 0, 0, 0, 0, 0, 1, 0),
             cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
             doseGrid = c(0.1, 0.5, 1.5, 3, 6,
                          seq(from = 10, to = 80, by=2)))

# Initialize a model 
model <- LogisticLogNormal(mean = c(-0.85, 1),
                           cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
                           refDose = 56)

# Get posterior for all model parameters
##For illustration purpose, we will use 50 burn-ins to generate 200 samples 
options <- McmcOptions(burnin = 50,
                       step = 2,
                       samples = 200)
set.seed(94)
samples <- mcmc(data, model, options)

# Approximate the posterior distribution with a bivariate normal
# Do not set the 'threshold.stop' and 'max.time' as in the example below since
# these were just set to get a result fast. Start instead with default option
set.seed(94)
posterior <- approximate(object = samples,
                         model = model,
                         data = data,
                         logNormal=TRUE,
                         control = list(threshold.stop = 0.1,
                                        max.time = 10))
             


