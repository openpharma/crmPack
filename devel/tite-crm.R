###two examples of using crmPack to run a TITE-CRM design with overdose control

library("crmPack")
source("R/SafetyWindow.r")
source("R/TITE-CRM-class.r")
source("R/TITE-CRM-simulation.r")


##Example 1: recommend a dose for the next cohort;

#1) Data

data <- DataTITE(x=c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
                 y=c(0, 0, 1, 1, 0, 0, 1, 0),
                 doseGrid=
                   c(0.1, 0.5, 1.5, 3, 6,
                     seq(from=10, to=80, by=2)),
                 u=c(42,30,15,5,20,25,30,60),
                 t0=c(0,-15,-30,-40,-55,-70,-75,-85),
                 Tmax=60,
                 weightMethod="adaptive")

emptydata <- DataTITE(doseGrid=c(0.1, 0.5,1, 1.5, 3, 6,
                                 seq(from=2, to=50, by=2)),Tmax=42,weightMethod = "adaptive")


#2) Structure of the model class

##need to fill in
Tmax_=42

model<-TITELogisticLogNormal(mean=c(-0.85,1),
                             cov=matrix(c(1,-0.5,-0.5,1),nrow=2),
                             ref_dose=56)

#3) Obtain the posterior

options <- McmcOptions(burnin=10,
                       step=2,
                       samples=1e2)

set.seed(94)
samples <- mcmc (data,model,options)

#4) use ggmcmc to diagnose

library(ggmcmc)
alpha0samples=get(samples,"alpha0")

print(ggs_traceplot(alpha0samples))

print(ggs_autocorrelation(alpha0samples))


#5) plot the model fit

plot(samples, model,data,hazard=TRUE)

plot(samples, model,data,hazard=FALSE)

#prior mean curve
emptydata <- DataTITE(doseGrid=c(0.1, 0.5, 1.5, 3, 6,
                                 seq(from=10, to=80, by=2)),Tmax=60,weightMethod = "adaptive")

set.seed(94)
Priorsamples <- mcmc (emptydata,model,options)

plot(Priorsamples, model,emptydata,hazard=FALSE)


#6) Escalation rules

##need to fill in (use the same rule in the section 8 of "using the package crmPack: introductory examples")
myIncrements <- IncrementsRelative(intervals=c(0,20),
                                   increments=c(1,0.33))

nextMaxDose <- maxDose(myIncrements,data=data)

myNextBest <- NextBestNCRM(target=c(0.2,0.35),
                           overdose=c(0.35,1),
                           maxOverdoseProb=0.25)

mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

myStopping1 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping2 <- StoppingMinPatients(nPatients=50)

myStopping <- (myStopping1 | myStopping2)


#7) recommended dose for the next cohort

doseRecommendation <- nextBest(myNextBest,
                               doselimit=nextMaxDose,
                               samples=samples,
                               model=model,
                               data=data)

doseRecommendation$value

doseRecommendation$plot

##Example 2: run a simulation to evaluate design operating characters;

#1) set up safety window and DADesign
##to be completed
mysafetywindow=SafetyWindowConst(c(6,2),10,20)

design <- TITEDesign(model=model,
                     increments=myIncrements,
                     nextBest=myNextBest,
                     stopping=myStopping,
                     cohortSize=mySize,
                     data=emptydata,
                     safetyWindow=mysafetywindow,
                     startingDose=3)

#2)set up truth curves

myTruth<-function(dose){

  model@prob(dose,alpha0=2,alpha1=3)

}

curve(myTruth(x), from=0, to=100, ylim=c(0, 1))



onset=15

exp_cond.cdf<-function(x){

  1-(pexp(x,1/onset,lower.tail=FALSE)-pexp(28,1/onset,lower.tail=FALSE))/pexp(28,1/onset)

}


#3) set up simulation settings

mySims <- simulate(design,
                   args=NULL,
                   truthTox=myTruth,
                   truthSurv=exp_cond.cdf,#piece_exp_cond.cdf,
                   trueTmax=80,
                   nsim=5,
                   seed=819,
                   mcmcOptions=options,
                   firstSeparate=TRUE,
                   deescalate=FALSE,
                   parallel=FALSE)

# system.time(simulate(design,
#                      args=NULL,
#                      truthTox=myTruth,
#                      truthSurv=exp_cond.cdf,#piece_exp_cond.cdf,
#                      trueTmax=80,
#                      nsim=500,
#                      seed=819,
#                      mcmcOptions=options,
#                      firstSeparate=TRUE,
#                      deescalate=FALSE,
#                      parallel=FALSE))


#4) interprate simulation result
#use a similar way as section 9.2 in the "using the package crmPack: introductory examples" document
a=summary(mySims,truth=myTruth)

plot(mySims)

mySims@stopReasons[[3]]

savePlot <- function(myPlot,name) {
  png(filename = paste(Sys.Date(),"C:/Users/liaoz4/Documents/R/simulation_result/",name,".png",sep=""), width = 480, height = 480)
  print(myPlot)
  dev.off()}
