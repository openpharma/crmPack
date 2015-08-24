##Simulate dose-escalation procedure based on DLE and efficacy responses where DLE 
## and efficacy samples are used
## The efficacy model must be of 'EffFlexi' class
Effmodel<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                    sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),smooth="RW2",data=data)

##Specified the design(for details please refer to the 'DualResponsesSamplesDesign' example)
design <- DualResponsesSamplesDesign(nextBest=mynextbest,
                                     cohortSize=mySize,
                                     startingDose=25,
                                     model=DLEmodel,
                                     Effmodel=Effmodel,
                                     data=emptydata,
                                     stopping=myStopping,
                                     increments=myIncrements)
##specified the true DLE curve and the true expected efficacy values at all dose levels
myTruthDLE<- function(dose)
{ DLEmodel@prob(dose, phi1=-53.66584, phi2=10.50499)
}

myTruthEff<- c(-0.5478867, 0.1645417,  0.5248031,  0.7604467,  
               0.9333009  ,1.0687031,  1.1793942 , 1.2726408 , 
               1.3529598 , 1.4233411 , 1.4858613 , 1.5420182)
##The true gain curve can also be seen
myTruthGain <- function(dose)
{return((myTruthEff(dose))/(1+(myTruthDLE(dose)/(1-myTruthDLE(dose)))))}


##simulate the trial for 10 times involving samples
options<-McmcOptions(burnin=100,step=2,samples=2000)
mySim<-simulate(object=design,
                args=NULL,
                trueDLE=myTruthDLE,
                trueEff=myTruthEff,
                trueSigma2=0.025,
                trueSigma2betaW=1,
                nsim=1,
                seed=819,
                parallel=FALSE)
