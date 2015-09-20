
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
options <- McmcOptions(burnin = 100,
                       step = 2,
                       samples = 2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Extract the posterior mean  (and empirical 2.5 and 97.5 percentile)
# for the prob(DLT) by doses
fitted <- fit(object = samples,
              model = model,
              data = data,
              quantiles=c(0.025, 0.975),
              middle=mean)


# ----------------------------------------------
# A different example using a different model
model <- LogisticIndepBeta(binDLE=c(1.05,1.8),
                           DLEweights=c(3,3),
                           DLEdose=c(25,300),
                           data=data)
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=200)
## samples must be from 'Samples' class (object slot in fit)
samples <- mcmc(data,model,options)

fitted <- fit(object=samples, model=model, data=data)


