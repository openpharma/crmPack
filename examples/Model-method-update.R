# Update the 'LogisticIndepBeta' model with new data.
empty_data <- Data(doseGrid = seq(25, 300, 25))

my_model_lib <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = empty_data
)

# Then, we have some new observations data.
data <- Data(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  ID = 1:8,
  cohort = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L),
  doseGrid = empty_data@doseGrid
)

# Update the model to get new estimates.
new_model_lib <- update(object = my_model_lib, data = data)

# Update the 'Effloglog' model with new data.
empty_data_dual <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

my_model_eff <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = empty_data_dual,
  const = 0
)

# Data with new observations data.
my_data_dual <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  ID = 1:8,
  cohort = c(1L, 2L, 2L, 3L, 4L, 4L, 5L, 6L),
  doseGrid = empty_data_dual@doseGrid
)

# Update the model to get new estimates.
new_model_eff <- update(object = my_model_eff, data = my_data_dual)
