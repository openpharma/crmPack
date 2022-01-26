my_model <- ProbitLogNormalLogDose(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  refDose = 7.2
)
