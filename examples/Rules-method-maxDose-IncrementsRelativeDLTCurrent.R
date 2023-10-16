# Example of usage for `IncrementsRelativeDLTCurrent` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if no DLTs were observed in current (i.e. last) cohort,
#  - only increasing the dose by 33% if 1 or 2 DLTs were observed in current cohort,
#  - only increasing the dose by 20% if at least 3 DLTs were observed in current cohort.
my_increments <- IncrementsRelativeDLTCurrent(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# Based on the rule above, the maximum dose allowed is:
max_dose <- maxDose(my_increments, data = my_data)
