# Example of usage for `IncrementsMin` maxDose with DataCombo.

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

# Rule 1: only one drug can be escalated at a time.
rule_one <- IncrementsComboOneDrugOnly()

# Rule 2: independent Cartesian increments for each drug.
rule_two <- IncrementsComboCartesian(
  drug1 = IncrementsRelative(intervals = c(0), increments = c(2)),
  drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
)

# Combine both rules and take the most conservative allowed dose per row.
my_increments <- IncrementsMin(increments_list = list(rule_one, rule_two))
maxDose(my_increments, my_data)
