
# Example of usage for `NextBestTD` NextBest class.
my_data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  ID = 1:8,
  cohort = c(1L, 2L, 2L, 3L, 4L, 5L, 6L, 7L),
  doseGrid = seq(from = 25, to = 300, by = 25)
)

my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# Target probabilities of the occurrence of a DLT during trial and
# at the end of the trial are defined as 0.35 and 0.3, respectively.
td_next_best <- NextBestTD(targetDuringTrial = 0.35, targetEndOfTrial = 0.3)

# doselimit is the maximum allowable dose level to be given to subjects.
dose_recommendation <- nextBest(
  nextBest = td_next_best,
  doselimit = max(my_data@doseGrid),
  model = my_model,
  data = my_data
)

dose_recommendation$next_dose_drt
dose_recommendation$plot
