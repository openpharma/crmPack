# Rule for having cohorts with random cohort size between 2 and 4
mySize <- CohortSizeRandom(min_size = 2, max_size = 4)

# Determine the cohort size for the next cohort
# This will return a random integer between 2 and 4 (inclusive)
set.seed(123)
size(mySize, dose = 5)
