# Create two opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Combine them with OR logic: at least one must be satisfied
opening_any <- OpeningAny(opening1, opening2)
print(opening_any)

# Alternative: use the | operator
opening_any_alt <- opening1 | opening2
print(opening_any_alt)
