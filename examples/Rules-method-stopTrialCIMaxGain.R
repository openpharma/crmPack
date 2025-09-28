# nolint start

##define the stopping rules based on the 'StoppingMaxGainCIRatio' class
##Using both DLE and efficacy responses
## we need a data object with doses >= 1:
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)

##DLEmodel must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

##Effmodel must be  of 'ModelEff' class
##For example, the 'Effloglog' class model
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)


##define the 'StoppingMaxGainCIRatio' class
myStopping <- StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
mynextbest <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

RecommendDose <- nextBest(
  mynextbest,
  doselimit = max(data@doseGrid),
  model = DLEmodel,
  model_eff = Effmodel,
  data = data
)

##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose,
  model = DLEmodel,
  data = data,
  Effmodel = Effmodel
)

# nolint end
