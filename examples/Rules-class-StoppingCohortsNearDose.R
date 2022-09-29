# As example, here is the rule for:
#   stopping the study if at least 3 cohorts were dosed at a dose within (1 +/- 0.2)
#   of the next best dose
my_stopping <- StoppingCohortsNearDose(
  nCohorts = 3,
  percentage = 0.2
)
