
# Create the data.
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)


# In this example we define a rule for dose increments which would allow:
# - escalating by 3 dose levels before any DLTs are reported,
# - escalating by 2 dose levels if exactly one DLT has been reported,
# - escalating by 1 dose level otherwise.
my_increments <- IncrementsAbsoluteDLT(intervals = 1:3, increments = 3:1)

# Based on the rule above, we then calculate the maximum dose allowed
next_max_dose <- maxDose(my_increments, data = data)
