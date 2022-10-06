
# This is the example of a rule for incrementing by a maximum of:
# * three dose levels if the highest dose used so far is <20
# * two dose levels if the highest dose used so far is >=20 and <50
# * one dose level otherwise
my_increments <- IncrementsAbsolute(
  intervals = c(0, 20, 50),
  increments = 3:1
)
