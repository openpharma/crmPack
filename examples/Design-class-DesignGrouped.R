empty_data <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the joint model.
my_model <- LogisticLogNormalGrouped(
  mean = c(-0.85, 0, 1, 0),
  cov = diag(1, 4),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort-size.
my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- maxSize(my_size1, my_size2)

# Choose the rule for stopping.
my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping1 & my_stopping2) |
  my_stopping3 |
  StoppingMissingDose()

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Rules to be used for both arms.
one_arm <- Design(
  model = .DefaultModelLogNormal(), # Ignored.
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = empty_data,
  startingDose = 3
)

# Initialize the design.
design <- DesignGrouped(
  model = my_model,
  mono = one_arm
)

# Alternative options: Here e.g.
# - use both mono in first cohort and afterwards have mono and combo in parallel,
# - in general allow different dose levels for the cohorts,
# - but for the start (i.e. second cohort) have the same dose for mono and combo.
# - Stop mono arm too, when combo arm is stopped.

design2 <- DesignGrouped(
  model = my_model,
  mono = one_arm,
  first_cohort_mono_only = TRUE,
  same_dose_for_all = FALSE,
  same_dose_for_start = TRUE,
  stop_mono_with_combo = TRUE
)
