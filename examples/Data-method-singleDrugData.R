# Example of extracting single-drug data from two-drug combination data.

my_combo_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 20, 20),
    drug2 = c(20, 20, 20, 40)
  ),
  y = c(0L, 1L, 0L, 0L),
  ID = 1L:4L,
  cohort = c(1L, 2L, 3L, 4L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40)
  )
)

# Extract the data for one specific drug.
singleDrugData(my_combo_data, "drug1")
