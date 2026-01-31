# Create sample trial data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Create opening criteria:
# - opening1: open if dose >= 10
# - opening2: open if at least 5 cohorts have been treated
opening1 <- OpeningMinDose(min_dose = 10)
opening2 <- OpeningMinCohorts(min_cohorts = 5)

# Combine with AND logic: both must be true
opening_all <- opening1 & opening2

# Test if backfill cohort 7 can be opened
# Cohort 6 dose is 10 (>= 10: TRUE), max cohort is 6 (>= 5: TRUE), so AND is TRUE
should_open <- openCohort(opening_all, cohort = 6, data = data, dose = 20)
print(should_open) # TRUE

# Test if backfill cohort at dose 6 can be opened
# Cohort 4 dose is 3 (>= 10: FALSE), so AND is FALSE
should_open_2 <- openCohort(opening_all, cohort = 4, data = data, dose = 20)
print(should_open_2) # FALSE
