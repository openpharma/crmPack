RuleDesignOrdinal(
  next_best = CohortSizeOrdinal(
    1L,
    NextBestMTD(
      target = 0.25,
      derive = function(x) median(x, na.rm = TRUE)
    )
  ),
  cohort_size = CohortSizeOrdinal(1L, CohortSizeConst(size = 3L)),
  data = DataOrdinal(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
  starting_dose = 5
)
