# nolint start

# Define the dose-grid and PEM parameters
empty_data <- DataDA(doseGrid = c(
  0.1, 0.5, 1, 1.5, 3, 6,
  seq(from = 10, to = 80, by = 2)
), Tmax = 60)

# Initialize the mDA-CRM model
npiece_ <- 10
Tmax_ <- 60

lambda_prior <- function(k) {
  npiece_ / (Tmax_ * (npiece_ - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece_,
  l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
  c_par = 2
)

# Choose the rule for dose increments
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size
my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)

my_size2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)

my_size <- maxSize(my_size1, my_size2)

# Choose the rule for stopping
my_stopping1 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)

my_stopping2 <- StoppingMinPatients(nPatients = 50)

my_stopping <- (my_stopping1 | my_stopping2)

# Choose the safety window
my_safety_window <- SafetyWindowConst(c(6, 2), 7, 7)

# Initialize the design
design <- DADesign(
  model = model,
  increments = my_increments,
  nextBest = my_next_best,
  stopping = my_stopping,
  cohortSize = my_size,
  data = empty_data,
  safetyWindow = my_safety_window,
  startingDose = 3
)
