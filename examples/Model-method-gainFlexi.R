# nolint start

##Obtain the gain value for a given dose, a pseudo DLE model, a DLE sample,
## the 'EffFlexi' efficacy model and an efficacy sample
##The DLE model must be from 'ModelTox' class (DLEmodel slot)
emptydata<- DataDual(doseGrid=seq(25,300,25))
data<-emptydata
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
DLEsamples <- mcmc(data, DLEmodel, McmcOptions(burnin=100,step=2,samples=200))

##The efficacy model must be from 'EffFlexi' class (Effmodel slot)
## The DLE and efficacy samples must be from 'Samples' class (DLEsamples and Effsamples slot)
EffFleximodel <- EffFlexi(eff=c(1.223, 2.513),eff_dose=c(25,300),
                          sigma2W=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),rw1 = FALSE,data=data)
Effsamples <- mcmc(data, EffFleximodel, McmcOptions(burnin=100,step=2,samples=200))

## Given a dose level 75,
gain(dose=75,DLEmodel=DLEmodel,DLEsamples=DLEsamples,Effmodel=EffFleximodel,Effsamples=Effsamples)

# nolint end
