NextBestMTD(
  target = 0.33,
  derive = function(mtd_samples) {
    quantile(mtd_samples, probs = 0.25)
  }
) %>% tidy()

NextBestMinDist(
  target = 0.33
) %>% tidy()
