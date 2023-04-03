# Example of usage for `IncrementsRelativeDLT` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if no DLTs were yet observed,
#  - increasing the dose by 33% if 1 or 2 DLTs were already observed,
#  - increasing the dose by 20% if at least 3 DLTs were already observed.
my_increments <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# Based on the rule above, the maximum dose allowed is:
nextMaxDose <- maxDose(my_increments, data = my_data)
