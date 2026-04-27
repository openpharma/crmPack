# Example of usage for `IncrementsComboCartesian` maxDose method.

# Create two-drug combination data where the last cohort received
# drug1 = 10, drug2 = 20.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10),
    drug2 = c(20, 20, 20)
  ),
  y = c(0L, 0L, 0L),
  ID = 1L:3L,
  cohort = c(1L, 1L, 1L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

# Define independent increment rules for each drug.
my_increments <- IncrementsComboCartesian(
  drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
  drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
)

# Determine the maximum allowed next dose levels.
# Here, the drug1 rule allows escalation up to 20 and the drug2 rule allows
# escalation up to 40. For drug1 values above 20, the combination is not
# admissible and is represented with NA for drug2.
maxDose(my_increments, my_data)
