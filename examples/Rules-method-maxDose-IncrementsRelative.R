# Example of usage for `IncrementsRelative` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
  ID = 1L:8L,
  cohort = as.integer(c(0, 1, 2, 3, 4, 5, 5, 5)),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:40)
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if the last dose was below 20,
#  - increasing the dose by 33% of the last dose, only if the last dose was
#    above or equal to 20.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Based on the rule above, the maximum dose allowed is:
max_dose <- maxDose(my_increments, data = my_data)
