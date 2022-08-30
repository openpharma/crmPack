
# Evaluate a stopping rule at specific dose=80
highest_dose_safe <- StopSpecificDose(
  rule = StoppingTargetProb(target=c(0, 0.3), prob=0.8),
  dose = 80
)
