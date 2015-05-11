### R code from vignette source 'c:/Users/sabanesd/R/forge/crmPack/vignettes/example.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: load
###################################################
## load package already here to be able to reference the package version!!
library(crmPack)
crmPackVersion <- as.character(packageVersion("crmPack"))


###################################################
### code chunk number 2: setup
###################################################
options(continue="  ")                  # use two blanks instead of "+" for
                                        # continued lines (for easy copying)


###################################################
### code chunk number 3: load
###################################################
library(crmPack)


###################################################
### code chunk number 4: webinterface (eval = FALSE)
###################################################
## crmPackHelp()


###################################################
### code chunk number 5: model-setup
###################################################
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                               matrix(c(1, -0.5, -0.5, 1),
                                      nrow=2),
                           refDose=56)


###################################################
### code chunk number 6: class
###################################################
class(model)


###################################################
### code chunk number 7: str
###################################################
str(model)


###################################################
### code chunk number 8: dose
###################################################
model@dose


###################################################
### code chunk number 9: min-inf
###################################################
set.seed(432)
coarseGrid <- c(0.1, 10, 30, 60, 100)
minInfModel <- MinimalInformative(dosegrid = coarseGrid,
                                  refDose=50,
                                  threshmin=0.2,
                                  threshmax=0.3,
                                  control=
                                  list(threshold.stop=0.03,
                                       maxit=200))


###################################################
### code chunk number 10: min-inf-res
###################################################
matplot(x=coarseGrid,
        y=minInfModel$required,
        type="b", pch=19, col="blue", lty=1,
        xlab="dose",
        ylab="prior probability of DLT")
matlines(x=coarseGrid,
         y=minInfModel$quantiles,
         type="b", pch=19, col="red", lty=1)
legend("right",
       legend=c("quantiles", "approximation"),
       col=c("blue", "red"),
       lty=1,
       bty="n")


###################################################
### code chunk number 11: min-inf-dist
###################################################
minInfModel$distance


###################################################
### code chunk number 12: min-inf-model
###################################################
str(minInfModel$model)


###################################################
### code chunk number 13: min-inf-model-extract
###################################################
myModel <- minInfModel$model


###################################################
### code chunk number 14: data
###################################################
data <- Data(x=c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
             y=c(0, 0, 0, 0, 0, 0, 1, 0),
             cohort=c(0, 1, 2, 3, 4, 5, 5, 5),
             doseGrid=
                 c(0.1, 0.5, 1.5, 3, 6,
                   seq(from=10, to=80, by=2)))


###################################################
### code chunk number 15: ids
###################################################
data@ID


###################################################
### code chunk number 16: plotdata
###################################################
print(plot(data))


###################################################
### code chunk number 17: mcmc-opts
###################################################
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=2000)


###################################################
### code chunk number 18: mcmc-sampling
###################################################
set.seed(94)
samples <- mcmc(data, model, options)


###################################################
### code chunk number 19: mcmc-extract
###################################################
## look at the structure of the samples object:
str(samples)
## now extract the alpha0 samples (intercept of the regression model)
alpha0samples <- get(samples, "alpha0")


###################################################
### code chunk number 20: ggmcmc
###################################################
library(ggmcmc)
print(ggs_traceplot(alpha0samples))


###################################################
### code chunk number 21: ggmcmc2
###################################################
print(ggs_autocorrelation(alpha0samples))


###################################################
### code chunk number 22: ggmcmc-help (eval = FALSE)
###################################################
## help(package="ggmcmc", help_type="html")


###################################################
### code chunk number 23: plot-model-fit
###################################################
print(plot(samples, model, data))


###################################################
### code chunk number 24: empty-data
###################################################
## provide only the dose grid:
emptydata <- Data(doseGrid=data@doseGrid)
## obtain prior samples with this Data object
priorsamples <- mcmc(emptydata, model, options)
## then produce the plot
print(plot(priorsamples, model, emptydata))


###################################################
### code chunk number 25: rel-incs
###################################################
myIncrements <- IncrementsRelative(intervals=c(0, 20),
                                   increments=c(1, 0.33))


###################################################
### code chunk number 26: max-dose
###################################################
nextMaxDose <- maxDose(myIncrements,
                       data=data)
nextMaxDose


###################################################
### code chunk number 27: ncrm-spec
###################################################
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)


###################################################
### code chunk number 28: mtd-spec
###################################################
mtdNextBest <- NextBestMTD(target=0.33,
                           derive=
                               function(mtdSamples){
                                   quantile(mtdSamples, probs=0.25)
                               })


###################################################
### code chunk number 29: next-best-run
###################################################
doseRecommendation <- nextBest(myNextBest,
                               doselimit=nextMaxDose,
                               samples=samples, model=model, data=data)


###################################################
### code chunk number 30: next-best-results
###################################################
doseRecommendation$value
print(doseRecommendation$plot)


###################################################
### code chunk number 31: size-range
###################################################
mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))


###################################################
### code chunk number 32: size-dlt
###################################################
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))


###################################################
### code chunk number 33: size-combined
###################################################
mySize <- maxSize(mySize1, mySize2)


###################################################
### code chunk number 34: size-eval
###################################################
size(mySize,
     dose=doseRecommendation$value,
     data=data)


###################################################
### code chunk number 35: size-const
###################################################
mySize <- CohortSizeConst(size=3)


###################################################
### code chunk number 36: rules-bits
###################################################
myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)


###################################################
### code chunk number 37: rules-compose
###################################################
myStopping <- (myStopping1 & myStopping2) | myStopping3


###################################################
### code chunk number 38: rules-try
###################################################
stopTrial(stopping=myStopping, dose=doseRecommendation$value,
          samples=samples, model=model, data=data)


###################################################
### code chunk number 39: design-setup
###################################################
design <- Design(model=model,
                 nextBest=myNextBest,
                 stopping=myStopping,
                 increments=myIncrements,
                 cohortSize=mySize,
                 data=emptydata,
                 startingDose=3)


###################################################
### code chunk number 40: true-def
###################################################
## define the true function
myTruth <- function(dose)
{
    model@prob(dose, alpha0=7, alpha1=8)
}

## plot it in the range of the dose grid
curve(myTruth(x), from=0, to=80, ylim=c(0, 1))


###################################################
### code chunk number 41: run-sims
###################################################
time <- system.time(mySims <- simulate(design,
                                       args=NULL,
                                       truth=myTruth,
                                       nsim=100,
                                       seed=819,
                                       mcmcOptions=options,
                                       parallel=FALSE))[3]
time


###################################################
### code chunk number 42: sim-class
###################################################
class(mySims)


###################################################
### code chunk number 43: sim-help
###################################################
help("Simulations-class", help="html")


###################################################
### code chunk number 44: third-trial
###################################################
print(plot(mySims@data[[3]]))


###################################################
### code chunk number 45: third-dose
###################################################
mySims@doses[3]


###################################################
### code chunk number 46: third-stop
###################################################
mySims@stopReasons[[3]]


###################################################
### code chunk number 47: sim-plot
###################################################
print(plot(mySims))


###################################################
### code chunk number 48: sim-summary
###################################################
summary(mySims,
        truth=myTruth)


###################################################
### code chunk number 49: sim-sum-plot
###################################################
simSum <- summary(mySims,
                  truth=myTruth)
print(plot(simSum))


###################################################
### code chunk number 50: sim-sum-plot2
###################################################
dosePlot <- plot(simSum, type="doseSelected") +
      scale_x_continuous(breaks=10:30, limits=c(10, 30))
print(dosePlot)


###################################################
### code chunk number 51: explain-fut
###################################################
model@prob


###################################################
### code chunk number 52: fut-samples
###################################################
postSamples <- as.data.frame(samples@data)[(1:20)*50, ]
postSamples


###################################################
### code chunk number 53: design-future
###################################################
nowDesign <- Design(model=model,
                    nextBest=myNextBest,
                    stopping=myStopping,
                    increments=myIncrements,
                    cohortSize=mySize,
                    ## use the current data:
                    data=data,
                    ## and the recommended dose as the starting dose:
                    startingDose=doseRecommendation$value)


###################################################
### code chunk number 54: sim-future
###################################################
time <- system.time(futureSims <- simulate(
    ## supply the new design here
    nowDesign,
    ## the truth is the assumed prob function
    truth=model@prob,
    ## further arguments are the
    ## posterior samples
    args=postSamples,
    ## do exactly so many simulations as
    ## we have samples
    nsim=nrow(postSamples),
    seed=918,
    ## this remains the same:
    mcmcOptions=options,
    parallel=FALSE))[3]
time


###################################################
### code chunk number 55: sim-future-plot
###################################################
print(plot(futureSims))


###################################################
### code chunk number 56: sim-future-summary
###################################################
summary(futureSims,
        truth=myTruth)


###################################################
### code chunk number 57: three-plus-three-setup
###################################################
threeDesign <- ThreePlusThreeDesign(doseGrid=c(5, 10, 15, 25, 35, 50, 80))
class(threeDesign)


###################################################
### code chunk number 58: three-sims
###################################################
threeSims <- simulate(threeDesign,
                      nsim=1000,
                      seed=35,
                      truth=myTruth,
                      parallel=FALSE)


###################################################
### code chunk number 59: three-sims-summary
###################################################
threeSimsSum <- summary(threeSims,
                        truth=myTruth)
threeSimsSum


###################################################
### code chunk number 60: three-sims-plot
###################################################
print(plot(threeSimsSum))


###################################################
### code chunk number 61: dual-data-struct
###################################################
data <- DataDual(
    x=
        c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10,
          20, 20, 20, 40, 40, 40, 50, 50, 50),
    y=
        c(0, 0, 0, 0, 0, 0, 1, 0,
          0, 1, 1, 0, 0, 1, 0, 1, 1),
    w=
        c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.55, 0.6,
          0.52, 0.54, 0.56, 0.43, 0.41, 0.39, 0.34, 0.38, 0.21),
    doseGrid=
        c(0.1, 0.5, 1.5, 3, 6,
          seq(from=10, to=80, by=2)))


###################################################
### code chunk number 62: dual-data-plot
###################################################
print(plot(data))


###################################################
### code chunk number 63: dual-rw1-model
###################################################
model <- DualEndpointRW(mu=c(0, 1),
                        Sigma=matrix(c(1, 0, 0, 1), nrow=2),
                        sigma2betaW=
                        0.01,
                        sigma2W=
                        c(a=0.1, b=0.1),
                        rho=
                        c(a=1, b=1),
                        smooth="RW1")


###################################################
### code chunk number 64: dual-options
###################################################
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=500)


###################################################
### code chunk number 65: dual-mcmc
###################################################
samples <- mcmc(data, model, options)


###################################################
### code chunk number 66: dual-conv
###################################################
data@nGrid
betaWpicks <- get(samples, "betaW", c(1, 5, 10, 25))
ggs_traceplot(betaWpicks)


###################################################
### code chunk number 67: dual-modelfit
###################################################
print(plot(samples, model, data, extrapolate=FALSE))


###################################################
### code chunk number 68: dual-variance
###################################################
ggs_histogram(get(samples, "precW"))


###################################################
### code chunk number 69: dual-nextbest
###################################################
myNextBest <- NextBestDualEndpoint(target=c(0.9, 1),
                                   overdose=c(0.35, 1),
                                   maxOverdoseProb=0.25)


###################################################
### code chunk number 70: dual-nextdose-eval
###################################################
nextDose <- nextBest(myNextBest,
                     doselimit=50,
                     samples=samples,
                     model=model,
                     data=data)
nextDose$value


###################################################
### code chunk number 71: dual-nextdose-plot
###################################################
print(nextDose$plot)


###################################################
### code chunk number 72: dual-stop
###################################################
myStopping4 <- StoppingTargetBiomarker(target=c(0.9, 1),
                                       prob=0.5)


###################################################
### code chunk number 73: dual-stop-try
###################################################
stopTrial(myStopping4, dose=nextDose$value,
          samples, model, data)


###################################################
### code chunk number 74: dual-stop-whole
###################################################
myStopping <- myStopping4 | StoppingMinPatients(40)


###################################################
### code chunk number 75: dual-design
###################################################
emptydata <- DataDual(doseGrid=data@doseGrid)
design <- DualDesign(model=model,
                     data=emptydata,
                     nextBest=myNextBest,
                     stopping=myStopping,
                     increments=myIncrements,
                     cohortSize=CohortSizeConst(3),
                     startingDose=6)


###################################################
### code chunk number 76: dual-scenario
###################################################
betaMod <- function (dose, e0, eMax, delta1, delta2, scal)
{
    maxDens <- (delta1^delta1) * (delta2^delta2)/((delta1 + delta2)^(delta1 + delta2))
    dose <- dose/scal
    e0 + eMax/maxDens * (dose^delta1) * (1 - dose)^delta2
}
trueBiomarker <- function(dose)
{
    betaMod(dose, e0=0.2, eMax=0.6, delta1=5, delta2=5 * 0.5 / 0.5, scal=100)
}
trueTox <- function(dose)
{
    pnorm((dose-60)/10)
}


###################################################
### code chunk number 77: dual-sc-plot
###################################################
par(mfrow=c(1, 2))
curve(trueTox(x), from=0, to=80)
curve(trueBiomarker(x), from=0, to=80)


###################################################
### code chunk number 78: dual-sims
###################################################
mySims <- simulate(design,
                   trueTox=trueTox,
                   trueBiomarker=trueBiomarker,
                   sigma2W=0.01,
                   rho=0,
                   nsim=10,
                   parallel=FALSE,
                   seed=3,
                   startingDose=6,
                   mcmcOptions =
                       McmcOptions(burnin=1000,
                                   step=1,
                                   samples=3000))


###################################################
### code chunk number 79: dual-sims-plot
###################################################
print(plot(mySims))


###################################################
### code chunk number 80: dual-sims-sum
###################################################
sumOut <- summary(mySims,
                  trueTox=trueTox,
                  trueBiomarker=trueBiomarker)
sumOut


###################################################
### code chunk number 81: dual-sim-sum-plot
###################################################
print(plot(sumOut))


