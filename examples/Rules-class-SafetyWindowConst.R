# This is to have along the study constant parameters settings of safety window
# length, regardless of the cohort size.
my_win_len <- SafetyWindowConst(
  gap = c(7, 5, 3),
  follow = 7,
  follow_min = 14
)
