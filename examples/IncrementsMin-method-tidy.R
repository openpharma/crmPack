myIncrements1 <- IncrementsRelativeDLT(dlt_intervals = c(0, 1, 3), increments = c(1, 0.33, 0.2))
myIncrements2 <- IncrementsRelative(intervals=c(0, 20), increments=c(1, 0.33))

myIncrements1 %>%  tidy()
myIncrements2 %>%  tidy()

IncrementsMin(increments_list=list(myIncrements1,myIncrements2)) %>%  tidy()
