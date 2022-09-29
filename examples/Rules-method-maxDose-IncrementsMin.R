# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid =
    c(
      0.1, 0.5, 1.5, 3, 6, 8,
      seq(from = 10, to = 80, by = 2)
    )
)


# As example, here we are combining 2 different increment rules.

# The first rule is the following:
#      maximum doubling the dose if no DLTs were observed at the current dose
#      or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed at the current dose
#      or maximum increasing the dose by 1.22 if 3 or more DLTs were observed

# The second rule is the following:
#   maximum doubling the dose if the current dose is <20
#   OR only maximum increasing the dose by 1.33 if the current dose is >=20

myIncrements1 <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

myIncrements2 <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Now we combine the 2 rules
combIncrement <- IncrementsMin(
  increments_list =
    list(myIncrements1, myIncrements2)
)

# Finally we then calculate the maximum dose allowed by taking the minimum of the two rules
nextMaxDose <- maxDose(
  combIncrement,
  data
)

# nolint end
