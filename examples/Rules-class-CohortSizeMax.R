
# Rule for having cohort of size 1 for doses <30
#      and having cohort of size 3 for doses >=30
my_size_1 <- CohortSizeRange(
  intervals = c(0, 10),
  cohortSize = c(1, 3)
)

# Rule for having cohort of size 1 until no DLT were observed
#      and having cohort of size 3 as soon as 1 DLT is observed
my_size_2 <- CohortSizeDLT(
  DLTintervals = c(0, 1),
  cohortSize = c(1, 3)
)

# Create a list of cohort size rules of class 'CohortSizeMax' which will then be
# combined with the 'max' operation
my_size <- CohortSizeMax(cohortSizeList = list(
  my_size_1,
  my_size_2
))
