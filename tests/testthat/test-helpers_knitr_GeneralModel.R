test_that("knit_print works ok for LogisticNormalMixture", {
  model <- LogisticNormalMixture(
    comp1 = ModelParamsNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    ),
    comp2 = ModelParamsNormal(
      mean = c(0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2)
    ),
    weightpar = c(a = 1, b = 1),
    ref_dose = 50
  )

  result <- knitr::knit_print(model)
  expect_snap(cat(result))
})
