# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 12, 12, 12, 16, 16, 16, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0),
  ID = 1:17,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)

# In this first example we define a rule for dose increments which allows for
# maximum skip one dose level, that is 2 dose levels higher than the last dose
# given.
my_increments <- IncrementsDoseLevels(levels = 2, basis_level = "last")

# Based on the rule above, we then calculate the maximum dose allowed
my_max_dose <- maxDose(my_increments, data = my_data)

# In this second example we define a rule for dose increments which allows for
# maximum skip one dose level, that is 2 dose levels higher than the max dose
# given.
my_increments <- IncrementsDoseLevels(levels = 2, basis_level = "max")

# Based on the rule above, we then calculate the maximum dose allowed.
max_dose <- maxDose(my_increments, data = my_data)