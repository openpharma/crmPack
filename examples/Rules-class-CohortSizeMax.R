
# Rule for cohort of size 1 for doses <30 and cohort of size 3 for doses >=30.
my_size1 <- CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3))

# Rule for cohort of size 1 until no DLT were observed and cohort of size 3
# as soon as 1 DLT is observed.
my_size2 <- CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))

# Cohort size rules of class 'CohortSizeMax' which will then be combined with
# the 'max' operation.
mySize <- CohortSizeMax(cohort_size_list = list(my_size1, my_size2))
