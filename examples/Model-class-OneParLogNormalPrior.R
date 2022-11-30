my_model <- OneParLogNormalPrior(
  skel_probs = seq(from = 0.1, to = 0.9, length = 5),
  dose_grid = 1:5,
  sigma2 = 2
)
