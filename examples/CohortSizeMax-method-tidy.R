mySize1 <- CohortSizeRange(intervals = c(0, 10), cohortSize = c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1), cohortSize=c(1, 3))
mySize <- CohortSizeMax(cohortSizeList=list(mySize1, mySize2))

mySize %>% tidy()
