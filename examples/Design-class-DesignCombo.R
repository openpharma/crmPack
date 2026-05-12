empty_data <- DataCombo(
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

my_model <- LogisticLogNormalCombo(
  single_models = list(
    drug1 = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    drug2 = LogisticLogNormal(
      mean = c(-0.7, 0.8),
      cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
      ref_dose = 20
    )
  ),
  gamma = 0,
  tau = 1
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

my_increments <- IncrementsMin(
  increments_list = list(
    IncrementsComboOneDrugOnly(),
    IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    )
  )
)

my_stopping <- StoppingMinPatients(nPatients = 20)

design_combo <- DesignCombo(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = CohortSizeConst(3L),
  data = empty_data,
  startingDose = c(10, 20)
)
