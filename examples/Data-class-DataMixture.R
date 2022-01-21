my_data <- DataMixture(
  xshare = c(12, 14, 16, 18.0),
  yshare = c(0L, 1L, 1L, 1L),
  nObsshare = 4L,
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  ID = 1:3,
  cohort = 1:3,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
my_data
