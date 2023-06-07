options <- McmcOptions(burnin = 1000, step = 2, samples = 2000)

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2), ref_dose = 56
)

data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

stopping_efficacy1 <- StoppingMinCohorts(nCohorts = 3)
stopping_efficacy2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
stopping_futility <- StoppingMinPatients(nPatients = 40)
trial_stopping <- (stopping_efficacy1 & stopping_efficacy2) | stopping_futility

cohort_size <- CohortSizeConst(size = 3)

next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

increments <- IncrementsRelative(intervals = c(0, 50, 200), increments = c(2, 1.33, 0.67))

empty_data <- Data(doseGrid = seq(from = 25, to = 300, by = 25))

model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 150
)

design <- Design(
  model = model,
  nextBest = next_best,
  stopping = trial_stopping,
  increments = increments,
  cohortSize = cohort_size,
  data = empty_data,
  startingDose = 50
)

truth <- function(dose) {
  probFunction(model, alpha0 = -2, alpha1 = 10)(dose)
}

simulations <- simulate(
  design,
  args = NULL,
  truth = truth,
  nsim = 5,
  mcmcOptions = options,
  parallel = FALSE
)

simulations %>% tidy()
