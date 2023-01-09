my_samples <- Samples(
  data = list(alpha = 1:5, beta = 15:19),
  options = McmcOptions(burnin = 2, step = 2, samples = 5)
)

names(my_samples)
