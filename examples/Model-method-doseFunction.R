my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

dose_fun <- doseFunction(my_model, alpha0 = 2, alpha1 = 3)
dose_fun(0.6)
