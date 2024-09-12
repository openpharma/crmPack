my_next_best_median <- NextBestList(
  summary = median,
  rules = list(
    NextBestMTD(
      0.25,
      function(mtd_samples) {
        quantile(mtd_samples, probs = 0.25)
      }
    ),
    NextBestMinDist(target = 0.33)
  )
)

my_next_best_max <- NextBestMax(
  rules = list(
    NextBestMTD(
      0.25,
      function(mtd_samples) {
        quantile(mtd_samples, probs = 0.25)
      }
    ),
    NextBestMinDist(target = 0.33)
  )
)

my_next_best_min <- NextBestMin(
  rules = list(
    NextBestMTD(
      0.25,
      function(mtd_samples) {
        quantile(mtd_samples, probs = 0.25)
      }
    ),
    NextBestMinDist(target = 0.33)
  )
)
