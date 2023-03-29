IncrementsNumDoseLevels(max_levels = 2) %>% tidy()
IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33)) %>% tidy()
IncrementsRelativeDLT(dlt_intervals = c(0, 20), increments = c(1, 0.33)) %>% tidy()
IncrementsRelativeParts(dlt_start = 0, clean_start = 1) %>% tidy()
