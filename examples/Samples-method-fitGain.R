##Obtain the 'fitGain' the middle, uppper and lower quantiles for the samples of gain values
## at all dose levels using a pseudo DLE model, a DLE sample, a pseudo Efficacy model and
## a efficacy sample
## data must be from 'DataDual' class
data<-DataDual(x=c(25,50,25,50,75,300,250,150),
               y=c(0,0,0,0,0,1,1,0),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))
## DLE model must be from 'ModelTox' class e.g using 'LogisticIndepBeta' model
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

## Efficacy model must be from 'ModelEff' class e.g using 'Effloglog' model
Effmodel<-Effloglog(c(1.223,2.513),c(25,300),nu=c(a=0.025,b=1),data=data)
## samples must be from 'Samples' class (object slot in fit)
options<-McmcOptions(burnin=100,step=2,samples=200)
##set up the same data set in class 'Data' for MCMC sampling for DLE
data1 <- Data(x=data@x,y=data@y,doseGrid=data@doseGrid)

DLEsamples <- mcmc(data=data1,model=DLEmodel,options=options)
Effsamples <- mcmc(data=data,model=Effmodel,options=options)

fitGain(DLEmodel=DLEmodel,DLEsamples=DLEsamples,
        Effmodel=Effmodel, Effsamples=Effsamples,data=data)