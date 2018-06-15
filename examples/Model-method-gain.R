##Obtain the gain value for a given dose, a pseudo DLE model, a DLE sample, 
## a pseudo efficacy model and an efficacy sample
##The DLE model must be from 'ModelTox' class (DLEmodel slot)
emptydata<- DataDual(doseGrid=seq(25,300,25),placebo=FALSE)
data<-emptydata
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
DLEsamples <- mcmc(data, DLEmodel, McmcOptions(burnin=100,step=2,samples=200))

##The efficacy model must be from 'ModelEff' class (Effmodel slot)
## The DLE and efficayc samples must be from 'Samples' class (DLEsamples and Effsamples slot)
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=data,c=0)
Effsamples <- mcmc(data, Effmodel, McmcOptions(burnin=100,step=2,samples=200))

## Given a dose level 75,
gain(dose=75,DLEmodel=DLEmodel,DLEsamples=DLEsamples,Effmodel=Effmodel,Effsamples=Effsamples)