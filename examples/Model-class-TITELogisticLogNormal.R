my_model <- TITELogisticLogNormal(
  mean = c(0, 1),
  cov = diag(2),
  ref_dose = 1,
  weight_method = "linear"
)

my_model1 <- TITELogisticLogNormal(
  mean = c(0, 1),
  cov = diag(2),
  ref_dose = 1,
  weight_method = "adaptive"
)
