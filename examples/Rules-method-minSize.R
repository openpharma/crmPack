# Here is the rule for:
#   having cohort of size 1 for doses <30
#   and having cohort of size 3 for doses >=30
mySize1 <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

# Here is the rule for:
#   having cohort of size 1 until no DLT were observed
#   and having cohort of size 3 as soon as 1 DLT is observed
mySize2 <- CohortSizeDLT(intervals = c(0, 1), cohort_size = c(1, 3))

# This is combining the two rules above by taking the minimum of the sample sizes of
# the single rules
mySize <- minSize(mySize1, mySize2)
