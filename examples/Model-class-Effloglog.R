# Obtain prior modal estimates given the pseudo data.
# First we use an empty data set such that only the dose levels under
# investigations are given. In total, 12 dose levels are under investigation
# ranging from 25 to 300 mg with increments of 25 (i.e 25, 50, 75, ..., 300).
emptydata <- DataDual(doseGrid = seq(25, 300, 25), placebo = FALSE)

# Define the pseudo data as first by fixing two dose levels 25 and 300 mg (`eff_dose`).
# Then, the efficacy responses observed at these two dose levels are 1.223 and 2.513 (`eff`).
# We specify the prior precision of the pseudo efficacy responses (`nu`) as a vector
# with the shape (a) and the rate (b) parameters for the gamma distribution.
# Obtain modal estimates and other estimates from the model (no observations,
# only pseudo data).
my_model1 <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# Observed data.
my_data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = emptydata@doseGrid
)

# Obtain posterior modal estimates and other estimates from the model given some
# observed data.
my_model2 <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data
)
