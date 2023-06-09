
# Rule for cohort of size 1 for doses <30 and cohort of size 3 for doses >=30.
my_size1 <- CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3))

# Rule for cohort of size 1 until no DLT were observed and cohort of size 3
# as soon as 1 DLT is observed.
my_size2 <- CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))

# Cohort size rules of class 'CohortSizeMin' which will then be combined with
# the 'min' operation.
my_size <- CohortSizeMin(cohort_sizes = list(my_size1, my_size2))
