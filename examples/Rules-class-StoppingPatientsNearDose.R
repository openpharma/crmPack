# As example, here is the rule for:
#   stopping the study if at least 9 patients were dosed at a dose within (1 +/- 0.2)
#   of the next best dose
my_stopping <- StoppingPatientsNearDose(
  nPatients = 9,
  percentage = 0.2
)
