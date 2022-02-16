
# Create the data
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 6, 6, 6),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8,
      seq(from = 10, to = 80, by = 2)
    )
)

# In this example we define a rule for dose increments that limits the further
# dose escalation to doses below 6
my_increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)

# Based on the rule above, we then calculate the maximum dose allowed
my_next_max_dose <- maxDose(my_increments, data = my_data)
