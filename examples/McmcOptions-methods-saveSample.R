# Set up the MCMC option in order to have a burn-in of 10000 iterations and
# then take every other iteration up to a collection of 10000 samples.
my_options <- McmcOptions(burnin = 10000, step = 2, samples = 10000)

size(my_options)
saveSample(my_options, iteration = 5)
