
# Rule for having cohort of size 1 for doses <30
#      and having cohort of size 3 for doses >=30
mySize1 <- CohortSizeRange(intervals = c(0, 10), cohort_size = c(1, 3))

# Rule for having cohort of size 1 until no DLT were observed
#      and having cohort of size 3 as soon as 1 DLT is observed
mySize2 <- CohortSizeDLT(DLTintervals = c(0, 1), cohortSize = c(1, 3))

# Create a list of cohort size rules of class 'CohortSizeMax' which will then be
# combined with the 'max' operation
mySize <- CohortSizeMax(cohortSizeList = list(mySize1, mySize2))
