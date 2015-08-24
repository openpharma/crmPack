##Simulate dose-escalation procedure based on DLE and efficacy responses where no DLE 
## and efficacy samples are used
##Specified the design(for details please refer to the 'DualResponsesDesign' example)
design <- DualResponsesDesign(nextBest=mynextbest,
                              model=DLEmodel,
                              Effmodel=Effmodel,
                              stopping=myStopping,
                              increments=myIncrements,
                              cohortSize=mySize,
                              data=emptydata,startingDose=25)
##Specify the true DLE and efficacy curves
myTruthDLE<- function(dose)
{ DLEmodel@prob(dose, phi1=-53.66584, phi2=10.50499)
}

myTruthEff<- function(dose)
{Effmodel@ExpEff(dose,theta1=-4.818429,theta2=3.653058)
}

## Then specified the simulations and generate the trial for 10 times

options<-McmcOptions(burnin=10000,step=20,samples=2000)
mySim <-simulate(object=design,
                 args=NULL,
                 trueDLE=myTruthDLE,
                 trueEff=myTruthEff,
                 trueNu=1/0.025,
                 nsim=10,
                 seed=819,
                 parallel=FALSE)
