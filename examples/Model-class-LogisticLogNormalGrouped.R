my_model <- LogisticLogNormalGrouped(
  mean = c(-0.85, 0, 1, 0),
  cov = diag(1, 4),
  ref_dose = 50
)
my_model
