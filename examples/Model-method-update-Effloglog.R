# Update the 'Effloglog' model with new data.

# First define the data and the model.
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

my_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata,
  const = 0
)

# Then we have some new observations data.
my_data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = emptydata@doseGrid
)

# Update the model to get new estimates.
new_model <- update(object = my_model, data = my_data)
