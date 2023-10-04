
# Example for the rule having cohort of size 1 for doses <30
# and having cohort of size 3 for doses >=30.

my_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1L, 3L))
