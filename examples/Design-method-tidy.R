emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           refDose=56)

myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)

mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35), prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)
myStopping <- (myStopping1 & myStopping2) | myStopping3

myIncrements <- IncrementsRelative(intervals=c(0, 20), increments=c(1, 0.33))

# Initialize the design
Design(#
  model=model,
  nextBest=myNextBest,
  stopping=myStopping,
  increments=myIncrements,
  cohortSize=mySize,
  data=emptydata,
  startingDose=3
) %>% tidy()
