data <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()
samples <- mcmc(data, model, options)

myIncrements <- .DefaultIncrementsOrdinal()
nextMaxDose <- maxDose(myIncrements, data = data)

myNextBest <- .DefaultNextBestOrdinal()

doseRecommendation <- nextBest(
  myNextBest,
  doselimit=nextMaxDose,
  samples=samples,
  model=model,
  data=data
)

myStopping <- .DefaultStoppingOrdinal()

stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
