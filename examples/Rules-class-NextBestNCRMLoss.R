# In the example below, the target toxicity interval [0.2, 0.35] while the
# overdose interval is (0.35, 1]. We would like to constrain the probability
# of overdosing below 25%. The loss function is c(1, 0, 1, 2).
my_next_best <- NextBestNCRMLoss(
  target_int = c(0.2, 0.35),
  overdose_int = c(0.35, 0.6),
  unacceptable_int = c(0.6, 1),
  max_overdose_prob = 0.25,
  losses = c(1, 0, 1, 2)
)
