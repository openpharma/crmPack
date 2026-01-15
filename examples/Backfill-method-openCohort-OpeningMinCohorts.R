# Create a data object with multiple cohorts
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Define the opening rule: open backfill cohorts after at least 3 cohorts
my_opening <- OpeningMinCohorts(min_cohorts = 3)

# Check if a backfill cohort can be opened
# Current number of cohorts is 6, so it should open (6 >= 3)
can_open <- openCohort(my_opening, cohort = 1, data = data, dose = 25)
can_open
