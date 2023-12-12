my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- CohortSizeOrdinal(1L, maxSize(my_size1, my_size2))

my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- StoppingOrdinal(1L, (my_stopping1 & my_stopping2) | my_stopping3)

# Initialize the design.
design <- DesignOrdinal(
  model = LogisticLogNormalOrdinal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  ),
  next_best = NextBestOrdinal(
    1L,
    NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    )
  ),
  stopping = my_stopping,
  increments = IncrementsOrdinal(
    1L,
    IncrementsRelative(
      intervals = c(0, 20),
      increments = c(1, 0.33)
    )
  ),
  cohort_size = my_size,
  data = DataOrdinal(
    doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    yCategories = c("No tox" = 0, "Sub-tox AE" = 1, "DLT" = 2)
  ),
  starting_dose = 3
)
