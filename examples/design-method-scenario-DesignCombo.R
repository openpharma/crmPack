# nolint start

# Define a hypothetical two-drug scenario.
data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0, 0, 1, 0, 0, 0),
  doseGrid = list(drug1 = c(10, 20, 30), drug2 = c(20, 40, 60))
)

model <- TwoDrugsCombo(
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

increments <- IncrementsMin(
  increments_list = list(
    IncrementsComboOneDrugOnly(),
    IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    )
  )
)

design <- DesignCombo(
  model = model,
  nextBest = NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  ),
  stopping = StoppingMinPatients(nPatients = 20),
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = DataCombo(doseGrid = data@doseGrid),
  startingDose = c(drug1 = 10, drug2 = 20)
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

\donttest{
result <- scenario(design, data, options)
result$fit
result$next_dose
result$cohort_size
result$stop
}

# nolint end
