# nolint start

##define the stopping rules based on the 'StoppingTDCIRatio' class
##Using only DLE responses with samples
## we need a data object with doses >= 1:
data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = seq(from = 25, to = 300, by = 25)
)

##model can be specified of 'Model' or 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)
##define MCMC options
## for illustration purposes, in reality larger number of burnin and samples shoud be used
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
##samples of 'Samples' class
samples <- mcmc(data, model, options)
##define the 'StoppingTDCIRatio' class
myStopping <- StoppingTDCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

RecommendDose <- nextBest(
  tdNextBest,
  doselimit = max(data@doseGrid),
  samples = samples,
  model = model,
  data = data
)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose_drt,
  samples = samples,
  model = model,
  data = data
)

# nolint end
