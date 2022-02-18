
# As example, here is the rule for:
#      maximum doubling the dose if no DLTs were observed at the current dose
#      or maximum increasing the dose by 1.33 if 1 or 2 DLTs were observed at the current dose
#      or maximum increasing the dose by 1.22 if 3 or more DLTs were observed

my_increments <- IncrementsRelativeDLTCurrent(
  DLTintervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)
