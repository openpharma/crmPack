## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# nolint start

## ----simulation setting-------------------------------------------------------
id <- 1
onset <- 3
a0 <- 2
a1 <- 3
refDose <- 56

# True dose-DLT relationship
myTruth <- function(dose){
  StandLogDose <- log(dose / refDose)
  plogis(a0 + a1 * StandLogDose)
}

# The conditional CDF of the PEM
if(onset==30) {
  onset=15
  exp_cond.cdf<-function(x){
    (pexp(42-x,1/onset,lower.tail=FALSE)-pexp(Tmax_,1/onset,lower.tail=FALSE))/pexp(Tmax_,1/onset)
  }
} else{ exp_cond.cdf<-function(x){
  1-(pexp(x,1/onset,lower.tail=FALSE)-pexp(Tmax_,1/onset,lower.tail=FALSE))/pexp(Tmax_,1/onset)
}}

## ----dose_definition----------------------------------------------------------
library(crmPack)
Tmax_ <- 42

model <- TITELogisticLogNormal(
  mean=c(1.33,1.49),
  cov=matrix(c(1.826,0.0209,0.0209,0.0245),nrow=2),
  ref_dose=refDose
)

myIncrements <- IncrementsRelative(intervals=c(0,20),
                                   increments=c(10,3))

myNextBest <- NextBestMTD(target=0.3,
                          derive=
                            function(mtd_samples){
                              mean(mtd_samples)
                            })

myStopping <- StoppingMinPatients(nPatients=48)

mySize <- CohortSizeConst(size=3)

emptydata <- DataDA(doseGrid=seq(from=2, to=50, by=2),Tmax=Tmax_)

mysafetywindow <- SafetyWindowConst(c(7,7),7,7)

design <- DADesign(model=model,
                   increments=myIncrements,
                   nextBest=myNextBest,
                   stopping=myStopping,
                   cohortSize=mySize,
                   data=emptydata,
                   safetyWindow=mysafetywindow,
                   startingDose=8)
# nolint end
