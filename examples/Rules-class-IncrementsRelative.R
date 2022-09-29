
# As example, here is the rule for:
#   maximum doubling the dose if the current dose is <20
#   OR only maximum increasing the dose by 1.33 if the current dose is >=20

my_Increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
