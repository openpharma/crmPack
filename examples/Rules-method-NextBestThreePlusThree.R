
# Create the data
data <- Data(x=c(5, 5, 5, 10, 10, 10),
             y=c(0, 0, 0, 0, 1, 0),
             cohort=c(0, 0, 0, 1, 1, 1),
             doseGrid=
               c(0.1, 0.5, 1.5, 3, 5,
                 seq(from=10, to=80, by=2)))


# The rule to select the next best dose will be based on the 3+3 method
myNextBest <- NextBestThreePlusThree()

# Calculate the next best dose
doseRecommendation <- nextBest(myNextBest,
                               data=data)

