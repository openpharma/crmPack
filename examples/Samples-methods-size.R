# Set up the MCMC option in order to have a burn-in of 100 iterations and
# then take every other iteration up to a collection of 200 samples.
my_options <- McmcOptions(burnin = 100, step = 2, samples = 200)

my_samples <- Samples(
  data = list(alpha = rnorm(200), beta = rnorm(200)),
  options = my_options
)

size(my_samples)
