# In the example below, the MTD is defined as the dose for which prob(DLE) = 0.33
# and we will use the 25th quantile of the posterior of MTD as our next best dose.
next_best_mtd <- NextBestMTD(
  target = 0.33,
  derive = function(mtd_samples) {
    quantile(mtd_samples, probs = 0.25)
  }
)
