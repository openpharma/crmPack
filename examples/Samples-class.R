# The mcmc options that were used to generate the samples.
my_options <- McmcOptions(
  burnin = 1000,
  step = 2,
  samples = 1000
)

# Create an object of class 'Samples'
# Here the parameters 'alpha' and 'beta' are randomly generated. Of course in
# a real example these would come as an output from mcmc procedures
my_samples <- Samples(
  data = list(alpha = rnorm(1000), beta = rnorm(1000)),
  options = my_options
)
