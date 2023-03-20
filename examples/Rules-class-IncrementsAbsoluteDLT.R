# This is the example of a rule for incrementing by a maximum of:
# * three dose levels no DLTs have been reported so far
# * two dose levels exactly one DLT has been reported so far
# * one dose level otherwise
my_increments <- IncrementsAbsoluteDLT(
  intervals = 1:3,
  increments = 3:1
)
