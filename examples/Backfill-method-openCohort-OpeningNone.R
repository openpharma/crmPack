# Create an OpeningNone object which never opens backfill cohorts
opening <- OpeningNone()

# Create sample trial data
data <- Data(
  x = c(20, 30, 40, 50),
  y = c(0, 0, 1, 0),
  cohort = c(1, 2, 3, 4),
  doseGrid = seq(10, 100, by = 10)
)

# Create dose recommendation
dose <- 60

# Check if backfill cohort should be opened for cohort 5
# OpeningNone always returns FALSE
should_open <- openCohort(opening, cohort = 5, data = data, dose = dose)
print(should_open) # FALSE
