# nolint start

# Define the dose-grid
emptydata <- Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80))

# inizialing a 3+3 design with constant cohort size of 3 and
# starting dose equal 5
myDesign <- RuleDesign(
  nextBest = NextBestThreePlusThree(),
  cohort_size = CohortSizeConst(size = 3L),
  data = emptydata,
  startingDose = 5
)

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

## define the true function
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Perform the simulation
## For illustration purpose only 10 simulation is produced (nsim=10).
threeSims <- simulate(
  myDesign,
  nsim = 10,
  seed = 35,
  truth = myTruth,
  parallel = FALSE
)

# nolint end
