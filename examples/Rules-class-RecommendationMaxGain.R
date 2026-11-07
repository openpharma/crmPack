RecommendationMaxGain(
  value = c(25, NA, 125),
  details = tibble::tibble(
    EstimateType = c("During trial", "End of trial", "Max gain"),
    TargetDose = c(30.5, 19.3, 36.2),
    LowerCI = c(NA, 19.3, 19.3),
    UpperCI = c(NA, 36.2, 219),
    CIRatio = c(NA, 1, 6.05)
  )
)
