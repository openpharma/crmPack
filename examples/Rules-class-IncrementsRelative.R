# This is the example of a rule for:
# maximum doubling the dose if the current dose is <20
# or only maximum increasing the dose by 1.33 if the current dose is >=20.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
