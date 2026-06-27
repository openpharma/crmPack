# Create independent increment rules for each drug in a two-drug combination
# trial.
drug1_rule <- IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
drug2_rule <- IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))

# Combine the one-dimensional rules into a Cartesian combination rule.
my_increments <- IncrementsComboCartesian(
  drug1 = drug1_rule,
  drug2 = drug2_rule
)
