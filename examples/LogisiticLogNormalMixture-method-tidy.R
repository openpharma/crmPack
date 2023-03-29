LogisticNormalMixture(
  comp1 = ModelParamsNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
  ),
  comp2 = ModelParamsNormal(
    mean = c(1, 1.5),
    cov = matrix(c(1.2, -0.45, -0.45, 0.6), nrow = 2)
  ),
  weightpar = c(a = 1, b = 1),
  ref_dose = 50
) %>%
  tidy()
