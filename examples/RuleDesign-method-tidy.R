RuleDesign(
  nextBest = NextBestThreePlusThree(),
  cohortSize = CohortSizeConst(size=3L),
  data = Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
  startingDose = 5
) %>% tidy()
