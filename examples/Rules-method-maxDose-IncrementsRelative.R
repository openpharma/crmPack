
# Example of usage for `IncrementsRelative` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)

# Define a rule for dose increments which allows:
#  - doubling the dose if the last dose was below 20,
#  - increasing the dose by 33% of the last dose, only if the last dose was
#    above or equal to 20.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Based on the rule above, the maximum dose allowed is:
my_next_max_dose <- maxDose(my_increments, data = my_data)
