# Initialing a 3+3 design with constant cohort size of 3 and starting dose equal 5.
my_design <- RuleDesign(
  nextBest = NextBestThreePlusThree(),
  cohortSize = CohortSizeConst(size = 3L),
  data = Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
  startingDose = 5
)
