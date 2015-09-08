
# As example, here is the rule for: 
#   having cohort of size 1 for doses <30
#   and having cohort of size 3 for doses >=30

mySize <- CohortSizeRange(intervals=c(0, 30),
                          cohortSize=c(1, 3))

