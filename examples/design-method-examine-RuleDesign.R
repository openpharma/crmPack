
# Define the dose-grid
emptydata <- Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80))

# inizialing a 3+3 design with constant cohort size of 3 and
# starting dose equal 5
myDesign <- RuleDesign(nextBest = NextBestThreePlusThree(),
                       cohort_size = CohortSizeConst(size = 3L),
                       data = emptydata,
                       startingDose = 5)

# Examine the design
set.seed(4235)
examine(myDesign)
