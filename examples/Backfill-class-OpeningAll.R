# Create two opening criteria
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 3)

# Combine them with AND logic: both must be satisfied
opening_all <- OpeningAll(opening1, opening2)
print(opening_all)

# Alternative: use the & operator
opening_all_alt <- opening1 & opening2
print(opening_all_alt)
