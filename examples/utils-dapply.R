df <- data.frame(
  dose = c(0.1, 6, 6, 5, 0.1, 5, 6, 6),
  cohort = c("B", "B", "B", "A", "A", "A", "B", "B")
)

dapply(
  df,
  f = ~cohort,
  FUN = function(coh) {
    data.frame(my_cohort = coh$cohort[1], my_max = max(coh$dose))
  }
)

dapply(
  df,
  f = ~cohort,
  FUN = function(coh) {
    coh$dose <- sort(coh$dose, decreasing = TRUE)
    coh
  }
)
