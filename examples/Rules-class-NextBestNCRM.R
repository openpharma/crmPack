# In the example below, the target toxicity interval [0.2, 0.35] while the
# overdose interval is (0.35,1]. Finally we would like to constrain the
# probability of overdosing below 25%.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
