# Obtain prior estimates for the efficacy model in flexible form, given the pseudo data.
# First define an empty data set by defining the dose levels used in the study.
# There are 12 dose levels used in the study, ranging from 25 to 300 mg with
# increments of 25.
emptydata <- DataDual(doseGrid = seq(25, 300, 25))

# Define the pseudo data, i.e.: fixed 2 dose levels 25 and 300 mg (`eff_dose`)
# and the efficacy responses 1.223 and 2.513 observed at these two dose levels (`eff`).
# The prior variance of the pseudo efficacy responses can be either a fixed value
# or two parameters for the inverse gamma distribution, the shape (a) and the
# rate (b) (`sigma2W`).
# The prior variance of the random walk model can be either a fixed value or two
# parameters for the inverse gamma distribution, the shape (a) and the rate (b)
# (`sigma2betaW`).
my_model <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = emptydata
)

# Obtain estimates from the model given some observed data is available.
data <- DataDual(
  x = c(25, 50, 50, 75, 100, 100, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = emptydata@doseGrid
)

my_model1 <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)
