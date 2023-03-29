d <- DataMixture(
  xshare = c(rep(10, 4), rep(20, 4), rep(40, 4)),
  yshare = c(rep(0L, 4), rep(0L, 4), rep(0L, 4)),
  ID = 1:3,
  cohort = c(1, 1, 2),
  x = c(5, 5, 10),
  y = c(0, 0, 0),
  doseGrid = c(5, 10, 20, 40, 80)
)
d %>% tidy()
