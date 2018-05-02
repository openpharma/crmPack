## ----echo=FALSE----------------------------------------------------------
render_sweave()
options(prompt = "R> ", continue = "+  ", width = 76, useFancyQuotes = FALSE)
opts_chunk$set(abbreviate=0, wrap=FALSE)
Sys.setenv(TEXINPUTS=getwd(),
           BIBINPUTS=getwd(),
           BSTINPUTS=getwd())
## opts_chunk$set(cache=TRUE)  # to prevent rerunning the code every time

oh <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
  ## do we need to abbreviate the output?
  if(options$abbreviate > 0)
  {
    ret <- strsplit(x, "\n")[[1]]
    ret <- if (length(ret) > options$abbreviate * 2)
      paste(paste(head(ret, options$abbreviate), 
                  collapse="\n"),
            "\n...\n", 
            paste(tail(ret, options$abbreviate),
                  collapse="\n"),
            "\n",
            sep="")
    else
      ret
    oh(ret, options)
  } else if(options$wrap) {
    ## do we need to wrap the output?
    oh(paste(paste(strwrap(strsplit(x, "\n")[[1]]), collapse="\n"),
             "\n"), 
       options)
  } else {
    ## original output hook
    oh(x, options)
  }
})

## ----start, warning=FALSE------------------------------------------------
library("crmPack")

## ----min-inf, warning=FALSE, message=FALSE, results="hide"---------------
coarseGrid <- c(25, 50, 100, 200, 300)
model <- MinimalInformative(dosegrid = coarseGrid, refDose = 100, 
                            logNormal = TRUE, threshmin = 0.1, 
                            threshmax = 0.2, seed = 432, 
                            control = list(max.time = 30))$model

## ----init-Data-----------------------------------------------------------
PL <- 0.001
data <- Data(x = c(PL, 25, 25, 25, PL, 50, 50, 50, PL, 100, 100, 100),
             y = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
             cohort = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
             doseGrid = c(PL, seq(25, 300, 25)),
             ID = 1:12,
             placebo = TRUE)

## ----plot-data,fig.show='hold',fig.width=5,fig.height=5,out.width='.35\\linewidth',fig.align='center', fig.env='figure', fig.cap='Open and blinded data plots'----
plot(data)
plot(data, blind = TRUE)

## ----mcmc-sampling-------------------------------------------------------
options <- McmcOptions(burnin = 1000, step = 2, samples = 10000)
set.seed(94)
samples <- mcmc(data, model, options)

## ----plot-model-fit,fig.show='hold',fig.width=5,fig.height=5,out.width='.35\\linewidth',fig.align='center', fig.env='figure', fig.cap='Posterior and prior regression model fits'----
plot(samples, model, data) + ggtitle("Posterior")

emptydata <- Data(doseGrid = data@doseGrid, placebo = TRUE)
priorsamples <- mcmc(emptydata, model, options)
plot(priorsamples, model, emptydata) + ggtitle("Prior")

## ----rel-incrs-----------------------------------------------------------
myIncrements <- IncrementsRelative(intervals = c(0, 100, 200), 
                                   increments = c(1, 0.5, 0.33))

## ----rel-incrs-eval------------------------------------------------------
(nextMaxDose <- maxDose(myIncrements, data))

## ----nextBest-ncrm-def---------------------------------------------------
myNextBest <- NextBestNCRM(target = c(0.2, 0.35), overdose = c(0.35, 1),
                           maxOverdoseProb = 0.25)

## ----nextBest-ncrm-eval--------------------------------------------------
nextDoseRes <- nextBest(myNextBest, nextMaxDose, samples, model, data)
(nextDoseVal <- nextDoseRes$value)

## ----nextBest-ncrm,echo=FALSE,fig.width=4,fig.height=4,out.width='.5\\linewidth',fig.align='center',fig.env='figure', fig.cap='Dose recommendation plot from NCRM design. Target probabilities as green bars, overdose probabilities as red bars, maximum next dose $t_{N+1}$ as vertical dashed black line, highest dose with probability of overdosing not exceeding 25\\% as vertical dashed red line, final dose recommendation with a red triangle.'----
nextDoseRes$plot

## ----stop-rules-def------------------------------------------------------
myStopping1 <- StoppingMinPatients(nPatients = 30)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping3 <- StoppingPatientsNearDose(nPatients = 9, percentage = 20)
myStopping <- myStopping1 | (myStopping2 & myStopping3)

## ----stop-rules-eval, wrap=TRUE------------------------------------------
stopTrial(myStopping, nextDoseVal, samples, model, data)

## ----size-rules----------------------------------------------------------
mySize <- CohortSizeConst(3)
mySizePL <- CohortSizeConst(1)

design <- Design(model = model, nextBest = myNextBest, 
                 stopping = myStopping, increments = myIncrements, 
                 cohortSize = mySize, PLcohortSize = mySizePL, 
                 data = emptydata, startingDose = 25)

## ----design-examine, abbreviate=5----------------------------------------
set.seed(23)
examine(design, options)

## ----true-def------------------------------------------------------------
myTruth <- function(dose){model@prob(dose, alpha0 = 4.5, alpha1 = 8)}

## ----true-def-alternative------------------------------------------------
doseProbMatrix <- cbind(c(1, 2, 3, 4, 5), c(0.01, 0.02, 0.04, 0.06, 0.09))
myTruthMatrix <- 
  function(dose){doseProbMatrix[match(dose, doseProbMatrix[, 1]), 2]}

## ----run-sims------------------------------------------------------------
mySimsTime <- 
  system.time(mySims <- simulate(design, truth = myTruth, nsim = 100, 
                                 seed = 819, mcmcOptions = options, 
                                 parallel = FALSE))[3]

## ----third-trial, fig.width=4,fig.height=4,out.width='.3\\linewidth',fig.align='center'----
mySims@data[[3]]@nObs
mySims@doses[3]      

## ----sim-plot, echo=FALSE, fig.width=4,fig.height=4,out.width='.5\\linewidth',fig.align='center',fig.env='figure', fig.cap='Simulation plot. On the top panel a summary of the trial trajectories, and on the bottom, the proportions of doses tried, averaged over the simulated trials, are shown.'----
plot(mySims)

## ----sim-summary, abbreviate=3-------------------------------------------
simSum <- summary(mySims, truth = myTruth)
simSum

## ----sim-summary-plot, echo=FALSE, fig.width=6,fig.height=6,out.width='.6\\linewidth',fig.align='center', warning=FALSE, fig.env='figure', fig.cap='Simulation summary plot'----
plot(simSum)

## ----data-dual, warning=FALSE--------------------------------------------
data2 <- DataDual(x = data@x, y = data@y, placebo = TRUE,
                  w = c(0.02, 0.42, 0.59, 0.45, 0.03, 0.7, 0.6, 0.52, 
                        0.01, 0.71, 0.54, 0.45), cohort = data@cohort, 
                  doseGrid = data@doseGrid, ID = data@ID)

## ----DLTmodel------------------------------------------------------------
DLTmodel <- LogisticIndepBeta(binDLE = c(1.05, 1.8), DLEweights = c(3, 3),
                              DLEdose = c(25, 300), data = emptydata)

## ----Effmodel------------------------------------------------------------
emptydata2 <- DataDual(doseGrid = emptydata@doseGrid, placebo = TRUE)
Effmodel <- Effloglog(Eff = c(1.223, 2.513), Effdose = c(25, 300), 
                      nu = c(a = 1, b = 0.025), data = emptydata2, c = 2)

## ----update--------------------------------------------------------------
newDLTmodel <- update(object = DLTmodel, data = data2)
newEffmodel <- update(object = Effmodel, data = data2)

## ----NextBestGain--------------------------------------------------------
GainNextBest <- NextBestMaxGain(DLEDuringTrialtarget = 0.35, 
                                DLEEndOfTrialtarget = 0.3)

## ----nextBest------------------------------------------------------------
(nextMaxDose <- maxDose(myIncrements, data2))
doseRecGain <- nextBest(GainNextBest, doselimit = nextMaxDose, 
                        model = newDLTmodel, Effmodel = newEffmodel, 
                        data = data2)
(nextDoseVal <- doseRecGain$nextdose)

## ----doseRecommendation, echo=FALSE, fig.width=7,fig.height=5,out.width='.6\\linewidth',fig.align='center'----
print(doseRecGain$plot)

## ----newdualstopping-rule------------------------------------------------
myStopping4 <- StoppingGstarCIRatio(targetRatio = 5,
                                    targetEndOfTrial = 
                                      GainNextBest@DLEEndOfTrialtarget)
myStoppingDual <- myStopping1 | myStopping4

## ----design--------------------------------------------------------------
design2 <- DualResponsesDesign(nextBest = GainNextBest, model = DLTmodel,
                               Effmodel = Effmodel, data = emptydata2,
                               stopping = myStoppingDual, 
                               increments = myIncrements, 
                               cohortSize = mySize, startingDose = 25)

## ----scenario------------------------------------------------------------
myTruthDLT<- function(dose){DLTmodel@prob(dose, phi1 = -53, phi2 = 10)}
myTruthEff<- function(dose){Effmodel@ExpEff(dose, theta1 = -4.8, 
                                            theta2 = 3.7)}
myTruthGain <- function(dose){myTruthEff(dose) * (1 - myTruthDLT(dose))}

## ----simulate------------------------------------------------------------
Sim1 <- simulate(object = design2, args = NULL, trueDLE = myTruthDLT,
                 trueEff = myTruthEff, trueNu = 1 / 0.025, nsim = 20, 
                 seed = 819, parallel = FALSE)

## ----classOnePara--------------------------------------------------------
.OneParExp <-
    setClass(Class = "OneParExp", contains = "Model",
             representation(skeletonFun = "function", 
                            skeletonProbs = "numeric", 
                            lambda = "numeric"))

## ----classOnePara-init---------------------------------------------------
OneParExp <- function(skeletonProbs, doseGrid, lambda)
{
  skeletonFun <- approxfun(x = doseGrid, y = skeletonProbs, rule = 2)
  invSkeletonFun <- approxfun(x = skeletonProbs, y = doseGrid, rule = 1)
  
  .OneParExp(
    skeletonFun = skeletonFun, skeletonProbs = skeletonProbs, 
    lambda = lambda,
    datamodel = function(){
      for (i in 1:nObs)
      {
        y[i] ~ dbern(p[i])
        p[i] <- skeletonProbs[xLevel[i]]^theta
      }},
    datanames = c("nObs", "y", "xLevel"),
    prob = function(dose, theta){ skeletonFun(dose)^theta },
    dose = function(prob, theta){ invSkeletonFun(prob^(1 / theta)) },
    priormodel = function(){ theta ~ dexp(lambda) },
    modelspecs = function(){ list(skeletonProbs = skeletonProbs,
                                  lambda = lambda) },
    init = function(){ list(theta = 1) }, sample = "theta")
}

## ----oneParaExp-model-example, fig.width=5,fig.height=4,out.width='.5\\linewidth',fig.align='center', fig.env='figure', fig.cap='Model fit of the one parameter power model'----
(skeletonProbs <- round(data@doseGrid / max(data@doseGrid) / 2, 2))
newModel <- OneParExp(skeletonProbs = skeletonProbs, 
                      doseGrid = data@doseGrid, lambda = 1)
newSamples <- mcmc(data, newModel, options)
plot(newSamples, newModel, data)

## ----newNextBest-def-----------------------------------------------------
.NextBestMinDist <- setClass(Class = "NextBestMinDist", 
                             contains = "NextBest",
                             representation(target = "numeric"))
NextBestMinDist <- function(target){ .NextBestMinDist(target = target) }

## ----newNextBest-method, results='hide'----------------------------------
setMethod("nextBest", 
          signature = signature(nextBest = "NextBestMinDist",
                                doselimit = "numeric", samples = "Samples",
                                model = "Model", data = "Data"),
          def = function(nextBest, doselimit, samples, model, data, ...){
            dosesOK <-
                if(length(doselimit))
                  which(data@doseGrid <= doselimit)
              else
                seq_along(data@doseGrid)
              modelfit <- fit(samples, model, data)
              probDLT <- modelfit$middle[dosesOK]
              doses <- modelfit$dose[dosesOK]
              bestIndex <- which.min(abs(probDLT - nextBest@target))
              bestDose <- doses[bestIndex]
              return(list(value = bestDose))
            })

## ----newNextBest-try-----------------------------------------------------
newMyNextBest <- NextBestMinDist(target = 0.3)
(newNextDoseVal <- nextBest(newMyNextBest, nextMaxDose, 
                            newSamples, newModel, data)$value)

