# nolint start

# Define the dose-grid
emptydata <- DataDual(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  rw1 = TRUE
)

# Choose the rule for selecting the next dose
myNextBest <- NextBestDualEndpoint(
  target = c(0.9, 1),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size
mySize1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
mySize2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping4 <- StoppingTargetBiomarker(
  target = c(0.9, 1),
  prob = 0.5
)
# only 10 patients here for illustration!
myStopping <- myStopping4 | StoppingMinPatients(10) | StoppingMissingDose()

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design
design <- DualDesign(
  model = model,
  data = emptydata,
  nextBest = myNextBest,
  stopping = myStopping,
  increments = myIncrements,
  cohortSize = CohortSizeConst(3),
  startingDose = 3
)

# define scenarios for the TRUE toxicity and efficacy profiles
betaMod <- function(dose, e0, eMax, delta1, delta2, scal) {
  maxDens <- (delta1^delta1) * (delta2^delta2) / ((delta1 + delta2)^(delta1 + delta2))
  dose <- dose / scal
  e0 + eMax / maxDens * (dose^delta1) * (1 - dose)^delta2
}

trueBiomarker <- function(dose) {
  betaMod(dose, e0 = 0.2, eMax = 0.6, delta1 = 5, delta2 = 5 * 0.5 / 0.5, scal = 100)
}

trueTox <- function(dose) {
  pnorm((dose - 60) / 10)
}

# Draw the TRUE profiles
par(mfrow = c(1, 2))
curve(trueTox(x), from = 0, to = 80)
curve(trueBiomarker(x), from = 0, to = 80)

# Run the simulation on the desired design
# We only generate 1 trial outcome here for illustration, for the actual study
## For illustration purpose we will use 5 burn-ins to generate 20 samples
# this should be increased of course
mySims <- simulate(design,
                   trueTox=trueTox,
                   trueBiomarker=trueBiomarker,
                   sigma2W=0.01,
                   rho=0,
                   nsim=1,
                   parallel=FALSE,
                   seed=3,
                   startingDose=6,
                   mcmcOptions =
                     McmcOptions(burnin=5,
                                 step=1,
                                 samples=20))

# Plot the summary of the Simulations
plot(summary(mySims,
             trueTox = trueTox,
             trueBiomarker = trueBiomarker))

# nolint end
