my_model <- OneParExpExpPrior(
  skel_probs = seq(from = 0.1, to = 0.9, length = 5),
  dose_grid = 1:5,
  lambda = 2
)
