
# Rule for having cohort of size 1 until no DLT is observed and having cohort
# of size 3 as soon as 1 DLT is observed.
my_size <- CohortSizeDLT(dlt_intervals = c(0, 1), cohort_size = c(1, 3))
