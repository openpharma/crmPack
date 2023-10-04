empty_data <- DataDA(doseGrid = c(
  0.1, 0.5, 1, 1.5, 3, 6,
  seq(from = 10, to = 80, by = 2)
), Tmax = 60)

npiece <- 10L
t_max <- 60

lambda_prior <- function(k) {
  npiece / (t_max * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece,
  l = as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece), 2, lambda_prior))),
  c_par = 2
)

my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1L, 3L)
)

my_size2 <- CohortSizeDLT(
  intervals = c(0L, 1L),
  cohort_size = c(1L, 3L)
)

my_size <- maxSize(my_size1, my_size2)

my_stopping1 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)

my_stopping2 <- StoppingMinPatients(nPatients = 50L)

my_stopping <- (my_stopping1 | my_stopping2)

my_safety_window <- SafetyWindowConst(c(6L, 2L), 7L, 7L)

design <- DADesign(
  model = model,
  increments = my_increments,
  nextBest = my_next_best,
  stopping = my_stopping,
  cohort_size = my_size,
  data = empty_data,
  safetyWindow = my_safety_window,
  startingDose = 3
)

