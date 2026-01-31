# Create sample trial data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Create opening criteria:
# - opening1: open if dose >= 20
# - opening2: open if at least 5 cohorts have been treated
opening1 <- OpeningMinDose(min_dose = 20)
opening2 <- OpeningMinCohorts(min_cohorts = 5)

# Combine with OR logic: at least one must be true
opening_any <- opening1 | opening2

# Test if backfill cohort 7 can be opened at dose 10
# Cohort 6 dose is 10 (>= 20: FALSE), max cohort is 6 (>= 5: TRUE), so OR is TRUE
should_open <- openCohort(opening_any, cohort = 6, data = data, dose = 20)
print(should_open) # TRUE

# Test with a different scenario:
# It does not matter which cohort index we give, because
# the number of cohorts is already above 5.
should_open_2 <- openCohort(opening_any, cohort = 1, data = data, dose = 25)
print(should_open_2) # TRUE
