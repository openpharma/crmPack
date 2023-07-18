# nolint start

library(crmPack)
mySize <- CohortSizeConst(size = 3)
myIncrements1 <- IncrementsRelative(intervals = c(2), increments = c(2))

data2 <- DataDual(doseGrid = c(seq(2, 10, by = 1), 12, 15, 20, 24, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))

data4 <- DataDual(
  x = c(9, 9, 9, 9, 9),
  y = c(0, 0, 0, 0, 0),
  w = c(0.404, 1.232, 0.328, -1.040, -1.588),
  doseGrid = c(seq(2, 10, by = 1), 12, 15, 20, 24, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
)

# efficacy model

Effmodel <- Effloglog(Eff = c(0.05, 1.0), Effdose = c(2, 120), nu = c(a = 0.001, b = 0.001), data = data2)


newEffmodel <- update(object = Effmodel, data = data4)



# dlt model
DLTmodel <- LogisticIndepBeta(binDLE = c(0.0713, 0.9507), DLEweights = c(3, 3), DLEdose = c(2, 120), data = data2)


data3 <- Data(
  x = c(9, 9, 9, 9, 9),
  y = c(0, 0, 0, 0, 0),
  doseGrid = c(seq(2, 10, by = 1), 12, 15, 20, 24, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
)

# efficacy model
# Modal estimates
newDLTmodel <- update(object = DLTmodel, data = data3)


#*****
GainNextBest <- NextBestMaxGain(DLEDuringTrialtarget = 0.35, DLEEndOfTrialtarget = 0.3)
doseRecGain <- nextBest(GainNextBest,
  doselimit = max(data4@doseGrid),
  model = newDLTmodel,
  Effmodel = newEffmodel,
  data = data4
)
doseRecGain$plot
doseRecGain


# stopping
myStopping7 <- StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3)

myStopping8 <- myStopping7 | StoppingMinPatients(72)
stopTrial(
  stopping = myStopping7, dose = doseRecGain$nextdose, model = newDLTmodel,
  data = data4, Effmodel = newEffmodel
)


stopTrial(
  stopping = myStopping8, dose = doseRecGain$nextdose, model = newDLTmodel,
  data = data4, Effmodel = newEffmodel
)


## check how lapply works with missing arguments
myfun <- function(x, a, b) {
  if (missing(b)) {
    x + a
  } else {
    x + a + b
  }
}
test <- function(x, a, b) {
  lapply(
    x,
    myfun,
    a,
    b
  )
}
test(1:5, a = 3, b = 2)

# nolint end
