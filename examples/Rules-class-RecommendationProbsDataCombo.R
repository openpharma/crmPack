RecommendationProbsDataCombo(
  value = 1,
  details = tibble::tibble(
    Dose1 = c(10, 20, 30, 10, 20, 30, 10, 20, 30),
    Dose2 = c(20, 20, 20, 40, 40, 40, 60, 60, 60),
    Target =   c(0.276, 0.186, 0.116, 0.161, 0.076, NA, NA, NA, NA),
    Overdose = c(0.082, 0.131, 0.140, 0.343, 0.188, NA, NA, NA, NA),
    Eligible = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
)
