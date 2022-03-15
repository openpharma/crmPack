# Decide on the dose grid and MCMC options.
dose_grid <- 1:80
my_options <- McmcOptions()

# Classic model.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

empty_data <- Data(doseGrid = dose_grid)
prior_samples <- mcmc(empty_data, my_model, my_options)
plot(prior_samples, my_model, empty_data)

# Set up the mixture model and data share object.
model_share <- LogisticLogNormalMixture(
  share_weight = 0.1,
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

empty_data_share <- DataMixture(
  doseGrid = dose_grid,
  xshare = rep(c(10, 20, 40), each = 4),
  yshare = rep(0L, 12),
)

# Compare with the resulting prior model.
prior_samples_share <- mcmc(empty_data_share, model_share, my_options)
plot(prior_samples_share, model_share, empty_data_share)
