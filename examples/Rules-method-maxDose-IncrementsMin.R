# Example of usage for `IncrementsRelativeDLTCurrent` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
  ID = 1L:8L,
  cohort = as.integer(c(0, 1, 2, 3, 4, 5, 5, 5)),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:80)
)

# Here, we combine two different increment rules.

# The first rule allows for:
#  - doubling the dose if no DLTs were observed at the current dose,
#  - increasing the dose by 33% if 1 or 2 DLTs were observed at the current dose,
#  - increasing the dose by 22% if 3 or more DLTs were observed.
my_increments_1 <- IncrementsRelativeDLT(
  intervals = c(0L, 1L, 3L),
  increments = c(1, 0.33, 0.2)
)

# The second rule allows for:
#  - doubling the dose if the current dose is <20,
#  - increasing the dose by 33% if the current dose is >=20.
my_increments_2 <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Finally, the maximum dose allowed is computed by taking the minimum dose from
# the maximum doses computed by the two rules.
my_increments <- IncrementsMin(
  increments_list = list(my_increments_1, my_increments_2)
)
max_dose <- maxDose(my_increments, my_data)
