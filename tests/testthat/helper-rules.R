h_next_best_mtd <- function(target = 0.33) {
  NextBestMTD(
    target = target,
    derive = function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )
}
