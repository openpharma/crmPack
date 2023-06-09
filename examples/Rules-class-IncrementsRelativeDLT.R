
# This is the example of a rule for:
# maximum doubling the dose if no DLTs were observed in the whole study so far
# or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed so far
# or maximum increasing the dose by 1.22 if 3 or more DLTs were observed so far.
my_increments <- IncrementsRelativeDLT(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)
