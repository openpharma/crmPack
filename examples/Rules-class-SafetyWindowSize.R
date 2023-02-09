# Rule for having patient gap (7,3,3,3,...) for cohort size < 4, and
# patient gap (9,5,5,5...) for cohort size >= 4.
my_window_len <- SafetyWindowSize(
  gap = list(c(7, 3), c(9, 5)),
  size = c(1, 4),
  follow = 7,
  follow_min = 14
)
