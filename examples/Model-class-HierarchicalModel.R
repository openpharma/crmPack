mono_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)

combo_model <- LogisticLogNormalCombo(
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

my_model <- HierarchicalModel(
  my_mono = mono_model,
  my_combo = combo_model,
  exchangeable_parameters = list(
    mono_intercept = list(
      my_mono = "alpha0",
      my_combo = "alpha0[1]"
    ),
    mono_slope = list(
      my_mono = "alpha1",
      my_combo = "alpha1[1]"
    )
  )
)

my_model
