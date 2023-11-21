NextBestOrdinal(
  grade = 1L,
  rule = NextBestMTD(
    0.25,
    function(mtd_samples) {
      quantile(mtd_samples, probs = 0.25)
    }
  )
)

