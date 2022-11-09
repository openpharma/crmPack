
# As example, here we are combining 2 different increment rules.

# The first rule is the following:
# maximum doubling the dose if no DLTs were observed at the current dose
# or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed at the current dose
# or maximum increasing the dose by 1.22 if 3 or more DLTs were observed.
my_increments_1 <- IncrementsRelativeDLT(
  dlt_intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# The second rule is the following:
# maximum doubling the dose if the current dose is <20
# or only maximum increasing the dose by 1.33 if the current dose is >=20.
my_increments_2 <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Now we combine the 2 rules.
comb_increments <- IncrementsMin(
  increments_list = list(my_increments_1, my_increments_2)
)
