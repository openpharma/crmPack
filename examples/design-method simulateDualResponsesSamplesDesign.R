##Simulate dose-escalation procedure based on DLE and efficacy responses where DLE 
## and efficacy samples are used
##Specified the design(for details please refer to the 'DualResponsesSamplesDesign' example)
design <- DualResponsesSamplesDesign(nextBest=mynextbest,
                                     cohortSize=mySize,
                                     startingDose=25,
                                     model=DLEmodel,
                                     Effmodel=Effmodel,
                                     data=emptydata,
                                     stopping=myStopping,
                                     increments=myIncrements)
##specified the true DLE and efficacy curve
myTruthDLE<- function(dose)
{ DLEmodel@prob(dose, phi1=-53.66584, phi2=10.50499)
}



myTruthEff<- function(dose)
{Effmodel@ExpEff(dose,theta1=-4.818429,theta2=3.653058)
}

##simulate the trial for 10 times involving samples
options<-McmcOptions(burnin=100,step=2,samples=2000)
mySim<-simulate(design,
                 args=NULL,
                 trueDLE=myTruthDLE,
                 trueEff=myTruthEff,
                 trueNu=1/0.025,
                 nsim=10,
                 mcmcOptions=options,
                 seed=819,
                 parallel=FALSE)


