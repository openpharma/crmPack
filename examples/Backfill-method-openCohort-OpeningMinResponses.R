# Create an OpeningMinResponses object requiring 2 responses at the cohort dose
opening <- OpeningMinResponses(min_responses = 2, include_lower_doses = FALSE)

# Create sample trial data with responses at different dose levels
data <- Data(
  x = c(10, 10, 20, 20, 20, 30, 30, 30),
  y = c(0, 1, 0, 1, 0, 1, 1, 0),
  response = c(1, 1, NA, 0, 1, NA, 1, 0),
  cohort = c(1, 1, 2, 2, 2, 3, 3, 3),
  doseGrid = seq(10, 100, by = 10)
)

# Check if backfill cohort can be opened at dose 20
# At dose 20, there is 1 response
should_open <- openCohort(opening, cohort = 2, data = data, dose = 30)
print(should_open) # FALSE

# Check at dose 10 where there are 2 responses
should_open_2 <- openCohort(opening, cohort = 1, data = data, dose = 30)
print(should_open_2) # TRUE
