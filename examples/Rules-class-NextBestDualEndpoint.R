# Target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
# and with a probability below 0.25 that prob(DLT) > 0.35 (safety).
my_next_best <- NextBestDualEndpoint(
  target = c(0.9, 1),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Now, using absolute target on the natural biomarker scale.
my_next_best_absolute <- NextBestDualEndpoint(
  target = c(200, 300),
  target_relative = FALSE,
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
