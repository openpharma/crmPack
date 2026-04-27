# Create some data of class 'DataCombo'.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0, 0, 1, 0, 0, 0),
  doseGrid = list(drug1 = c(10, 20, 30), drug2 = c(20, 40))
)

# Update the data with a new cohort.
my_data1 <- update(
  my_data,
  x = c(drug1 = 30, drug2 = 40),
  y = c(0L, 1L, 0L)
)
my_data1
