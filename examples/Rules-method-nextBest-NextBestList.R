data <- Data(
  doseGrid = c(1, 3, 9, 18, 36, 54, 80, 100),
  x = c(1, 1, 1, 3, 3, 3, 9, 9, 9),
  y = c(rep(0, 8), 1),
  cohort = rep(1L:3L, each = 3),
  ID = 1L:9L
)

model <- .DefaultLogisticLogNormal()
samples <- mcmc(data, model, .DefaultMcmcOptions())

next_best_mtd <- NextBestMTD(
  0.25,
  function(mtd_samples) quantile(mtd_samples, probs = 0.25)
)

next_best_min_dist <- .DefaultNextBestMinDist()

nextBest(next_best_mtd, Inf, samples, model, data)
nextBest(next_best_min_dist, Inf, samples, model, data)

next_best_min <- NextBestMin(list(next_best_mtd, next_best_min_dist))
nextBest(next_best_min, Inf, samples, model, data)

next_best_max <- NextBestMax(list(next_best_mtd, next_best_min_dist))
nextBest(next_best_max, Inf, samples, model, data)
