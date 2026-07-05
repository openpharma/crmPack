my_model <- TITELogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50,
  weight_method = "linear"
)

my_model1 <- TITELogisticLogNormalSub(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50,
  weight_method = "adaptive"
)
