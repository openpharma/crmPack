#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[combo.R] by DSB Sam 07/03/2015 21:58>
##
## Description:
## Test the combo stuff. For development only!!
##
## History:
## 25/01/2015   file creation
###################################################################################

source("../R/helpers.R")
source("../R/Data-class.R")


## create some test data
data <- DataCombo(x=
                      cbind(a=c(0, 0, 0, 0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 30, 30, 30),
                            b=c(10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50, 50, 50, 60, 60, 60)),
                  y=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1),
                  doseGrid=
                      list(a=
                               c(0, 0.1, 0.5, 1.5, 3, 6,
                                 seq(from=10, to=80, by=2)),
                           b=seq(from=0, to=80, by=10)))

data
data@nGrid
data@nObs

## now test updating and plotting the data
source("../R/Data-methods.R")

## updating:
data2 <- update(data,
                x=c(a=0.5, b=30),
                y=c(0, 0, 0, 0))

## plotting:
library(ggplot2)
x11()
grid::grid.draw(plot(data2))

## load model code
source("../R/Model-class.R")

## define the model
model <- ComboLogistic(singlePriors=
                           list(a=
                                    LogisticLogNormal(mean=c(0, 1),
                                                      cov=diag(2),
                                                      refDose=10),
                                b=
                                    LogisticLogNormal(mean=c(1, 2),
                                                      cov=diag(2),
                                                      refDose=20)),
                       gamma=2,
                       tau=1)

## what is the distribution on the interaction parameter eta?
curve(dnorm(x, 2, 1), from=-3, to=3,
      xlab="eta", ylab="prior density")
abline(v=0, col="gray")


## try sampling from the model:

source("../R/McmcOptions-class.R")
source("../R/McmcOptions-methods.R")
## and some MCMC options
options <- McmcOptions(burnin=10000,
                       step=2,
                       samples=50000)


source("../R/mcmc.R")
source("../R/writeModel.R")
source("../R/Samples-class.R")

## obtain the samples
library(rjags)

## if sampling from prior:
data <- DataCombo(doseGrid=
                    list(a=
                           c(0.1, 0.5, 1.5, 3, 6,
                             seq(from=10, to=80, by=2)),
                         b=seq(from=10, to=80, by=10)))

plot(data)

samples <- mcmc(data, model, options, verbose=TRUE)

str(samples)

source("../R/Samples-methods.R")

## use the ggmcmc package for convergence checks. we provide the extract function
## for this purpose:
library(ggmcmc)

alpha0samples <- get(samples, "alpha0")
ggs_traceplot(alpha0samples)

alpha1samples <- get(samples, "alpha1")
ggs_traceplot(alpha1samples)

etasamples <- get(samples, "eta")
ggs_traceplot(etasamples)


## ok now we want to plot the fit:
source("../R/Model-methods.R")

## test C++ inline. requires MinGW installation and path settings,
## see http://stackoverflow.com/questions/23458841/how-to-get-rcpp-to-work
library(Rcpp)

cppFunction('
int fibonacci(const int x) {
if (x < 2) {
return x; } else {
return (fibonacci(x - 1)) + fibonacci(x - 2); }
}
')
fibonacci(5)



source("../R/crmPack-package.R")
.onLoad("", "")
## options(crmPackUsesCpp=FALSE)
system.time(print(plot(samples, model, data, focus=c("a", "b"))))

## old:
##   user  system elapsed
##  73.40    0.25   73.74

## new:

## user  system elapsed
## 5.27    0.20    5.51

## after first run even slightly faster:
   ## user  system elapsed
## 4.87    0.15    5.02

## ==> ~15 times faster with C++!
## nice!
## todo: later when pushing to GRAN/CRAN include C code properly in src,
## instead of loading via cppFunction.

## focus on single agent:
system.time(print(plot(samples, model, data, focus=c("a"))))
system.time(print(plot(samples, model, data, focus=c("b"))))

## compare single agent model results with marginal from combo model:

## only drug b here:
data <- DataCombo(x=
                    cbind(a=rep(0, 10),
                          b=c(10, 10, 10, 20, 20, 20, 40, 40, 40, 50)),
                  y=c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
                  doseGrid=
                    list(a=
                           c(0, 0.1, 0.5, 1.5, 3, 6,
                             seq(from=10, to=80, by=2)),
                         b=seq(from=0, to=80, by=10)))
samples <- mcmc(data, model, options, verbose=TRUE)
plot(samples, model, data, focus=c("b"))

## now with the single agent data and model:
datab <- Data(x=data@x[, "b"],
              y=data@y,
              doseGrid=data@doseGrid$b)
modelb <- model@singlePriors$b
samplesb <- mcmc(datab, modelb, options)
plot(samplesb, modelb, datab)
## perfect!

## todo: cont here
## Next step: 
## Maximum increments
## nextBest functionality, including plots



