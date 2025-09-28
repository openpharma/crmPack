# Stop if highest dose 80 is safe.
highest_dose_safe <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
  dose = 80
)

# Stop if lowest dose 10 is toxic.
lowest_dose_toxic <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0.3, 1), prob = 0.8),
  dose = 10
)
