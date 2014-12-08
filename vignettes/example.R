### R code from vignette source 'c:/Users/sabanesd/R/forge/crmPack/vignettes/example.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: setup
###################################################
options(continue="  ")                  # use two blanks instead of "+" for
                                        # continued lines (for easy copying)


###################################################
### code chunk number 2: install-others (eval = FALSE)
###################################################
## install.packages(c("Rcpp", "RcppArmadillo", "rjags",
## 	           "ggplot2", "gridExtra", "GenSA", "BayesLogit", "mvtnorm"),
##                  dependencies=TRUE)


###################################################
### code chunk number 3: install-crmpack (eval = FALSE)
###################################################
## install.packages("path/to/the/directory/crmpack-master",
##                  repos=NULL,
##                  type="source")


###################################################
### code chunk number 4: load
###################################################
library(crmPack)


###################################################
### code chunk number 5: webinterface (eval = FALSE)
###################################################
## help.start()


###################################################
### code chunk number 6: model-setup
###################################################
model <- new("LogisticLogNormal",
             mean=c(-0.85, 1),
             cov=
             matrix(c(1, -0.5, -0.5, 1),
                    nrow=2L),
             refDose=56)


###################################################
### code chunk number 7: class
###################################################
class(model)


###################################################
### code chunk number 8: str
###################################################
str(model)


###################################################
### code chunk number 9: dose
###################################################
model@dose


###################################################
### code chunk number 10: min-inf
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
### code chunk number 11: min-inf-res
###################################################
matplot(x=coarseGrid,
        y=minInfModel$required,
        type="b", pch=19, col="blue", lty=1,
        xlab="dose",
        ylab="prior probability of DLT")
matlines(x=coarseGrid,
         y=minInfModel$quantiles,
         type="b", pch=19, col="red", lty=1)


###################################################
### code chunk number 12: min-inf-dist
###################################################
minInfModel$distance


###################################################
### code chunk number 13: min-inf-model
###################################################
str(minInfModel$model)


###################################################
### code chunk number 14: data
###################################################
data <- new("Data",
            x=
            c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
            y=
            as.integer(c(0, 0, 0, 0, 0, 0, 1, 0)),
            cohort=
            as.integer(c(0, 1, 2, 3, 4, 5, 5, 5)),
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
options <- new("McmcOptions",
               burnin=100,
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
alpha0samples <- extract(samples, "alpha0")


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
## help(package="ggmcmc")


###################################################
### code chunk number 23: plot-model-fit
###################################################
print(plot(samples, model, data))


###################################################
### code chunk number 24: empty-data
###################################################
## provide empty dose and DLT vectors, and use same dose grid
## as before:
emptydata <- new("Data",
                 x=numeric(),
                 y=integer(),
                 doseGrid=data@doseGrid)
## obtain prior samples with this Data object
priorsamples <- mcmc(emptydata, model, options)
## then produce the plot
print(plot(priorsamples, model, emptydata))


###################################################
### code chunk number 25: rel-incs
###################################################
myIncrements <- new("IncrementsRelative",
                    intervals=c(0, 20, Inf),
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
myNextBest <- new("NextBestNCRM",
                  target=c(0.2, 0.35),
                  overdose=c(0.35, 1),
                  maxOverdoseProb=0.25)


###################################################
### code chunk number 28: mtd-spec
###################################################
mtdNextBest <- new("NextBestMTD",
                   target=0.33,
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
mySize1 <- new("CohortSizeRange",
               intervals=c(0, 30, Inf),
               cohortSize=as.integer(c(1, 3)))


###################################################
### code chunk number 32: size-dlt
###################################################
mySize2 <- new("CohortSizeDLT",
               DLTintervals=c(0, 1, Inf),
               cohortSize=as.integer(c(1, 3)))


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
mySize <- new("CohortSizeConst",
              size=3L)


###################################################
### code chunk number 36: rules-bits
###################################################
myStopping1 <- new("StoppingMinCohorts",
                   nCohorts=3L)
myStopping2 <- new("StoppingTargetProb",
                   target=c(0.2, 0.35),
                   prob=0.5)
myStopping3 <- new("StoppingMaxPatients",
                   nPatients=20L)


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
design <- new("Design",
              model=model,
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
    model@prob(dose, alpha0=-1, alpha1=4)
}

## plot it in the range of the dose grid
curve(myTruth(x), from=0, to=80, ylim=c(0, 1))


###################################################
### code chunk number 41: run-sims
###################################################
set.seed(819)
time <- system.time(mySims <- simulate(design,
                                       truth=myTruth,
                                       args=NULL,
                                       firstSeparate=FALSE,
                                       nsim=10L,
                                       mcmcOptions=options,
                                       parallel=TRUE))[3]
time


###################################################
### code chunk number 42: sim-class
###################################################
class(mySims)


###################################################
### code chunk number 43: sim-help
###################################################
help("Simulations-class")


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
library(grid)
grid.draw(plot(mySims))


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
grid.draw(plot(simSum))


###################################################
### code chunk number 50: sim-sum-plot2
###################################################
print(plot(simSum, type="doseSelected") +
      scale_x_continuous(breaks=40:60, limits=c(40, 60)))


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
nowDesign <- new("Design",
                 model=model,
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
set.seed(901)
time <- system.time(futureSims <- simulate(## supply the new design here
                                           nowDesign,
                                           ## the truth is the assumed prob function
                                           truth=model@prob,
                                           ## further arguments are the
                                           ## posterior samples
                                           args=postSamples,
                                           ## no separate first patient
                                           firstSeparate=FALSE,
                                           ## do exactly so many simulations as
                                           ## we have samples
                                           nsim=nrow(postSamples),
                                           ## this remains the same:
                                           mcmcOptions=options,
                                           parallel=TRUE))[3]
time


###################################################
### code chunk number 55: sim-future-plot
###################################################
a <- plot(futureSims)
grid.draw(a)


###################################################
### code chunk number 56: sim-future-summary
###################################################
summary(futureSims,
        truth=myTruth)


