# Example of usage for `IncrementsComboOneDrugOnly` maxDose method.

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

# Define the one-drug-only escalation rule.
my_increments <- IncrementsComboOneDrugOnly()

# Determine the maximum allowed dose combination.
# For drug1 = 10 (not escalated): drug2 may go up to Inf (unrestricted here).
# For drug1 = 20 or 30 (escalated): drug2 is capped at the last drug2 dose (20).
maxDose(my_increments, my_data)
