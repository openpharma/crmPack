# Example: Define EWOC next best dose rule.
# Target toxicity probability is 0.30. Overdose region is any probability > 0.35.
# We restrict posterior probability of recommending an overdosing dose to ≤ 0.25.
next_best_ewoc <- NextBestEWOC(
  target = 0.30,
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
