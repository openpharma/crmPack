
# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6,
      seq(from = 10, to = 80, by = 2)
    )
)


# In this example we define a rule for dose increments which would allow:
#   - doubling the dose if no DLTs were yet observed
#   - only increasing the dose by 1.33 if 1 or 2 DLTs were already observed
#   - only increasing the dose by 1.2 if at least 3 DLTs were already observed
my_increments <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# Based on the rule above, we then calculate the maximum dose allowed
next_max_dose <- maxDose(my_increments,
  data = data
)
