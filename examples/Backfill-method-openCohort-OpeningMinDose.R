# Create a simple data object with some dose information
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 3, 4, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Define the opening rule: open backfill cohorts when dose is at least 5
my_opening <- OpeningMinDose(min_dose = 5)

# Check if the first backfill cohort can be opened when the
# cohort has a dose 6, i.e. larger than 5.
# Note that `dose` is not used in this rule.
can_open <- openCohort(my_opening, cohort = 5, data = data, dose = 7)
can_open

# Check if the first backfill cohort can be opened when the
# cohort has a dose 3, i.e. smaller than 5.
# (should return FALSE because dose < min_dose)
can_open_low <- openCohort(my_opening, cohort = 4, data = data, dose = 15)
can_open_low
