# nolint start

data <-DataDual(x=c(25,50,25,50,75,300,250,150),
                y=c(0,0,0,0,0,1,1,0),
                w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
                doseGrid=seq(25,300,25),placebo=FALSE)
##The 'nextBest' method using NextBestMaxGainSamples' rules class object
## using the 'ModelTox' class DLE model
## DLEmodel e.g 'LogisticIndepBeta' class
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

## using the 'ModelEff' class efficacy model
## Effmodel e.g 'Effloglog' class
Effmodel<-Effloglog(eff = c(1.223,2.513), eff_dose = c(25,300),nu=c(a=1,b=0.025),data=data)
##DLE and efficacy samples must be of 'Samples' Class
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
DLEsamples<-mcmc(data,DLEmodel,my_options)
Effsamples<-mcmc(data,Effmodel,my_options)

##target probabilities of the occurrence of a DLE during trial and at the end of trial
## are defined as 0.35 and 0.3, respectively
## Using 30% posterior quantile of the TD35 and TD30 samples as estimates of TD35 and TD30,
## function specified in TDderive slot
## Using the 50% posterior quantile of the Gstar (the dose which gives the maxim gain value)
## samples as Gstar estimate,function specified in Gstarderive slot
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})

RecommendDose<-nextBest(mynextbest,doselimit=max(data@doseGrid),samples=DLEsamples,model=DLEmodel,
                        data=data,Effmodel=Effmodel,Effsamples=Effsamples)

## now using the 'EffFlexi' class efficacy model:

##The 'nextBest' method using NextBestMaxGainSamples' rules class object for 'EffFlexi' model class
## using the 'ModelTox' class DLE model
## DLEmodel e.g 'LogisticIndepBeta' class
Effmodel<- EffFlexi(eff=c(1.223, 2.513), eff_dose=c(25,300),
                    sigma2W=c(a=0.1,b=0.1),
                    sigma2betaW=c(a=20,b=50), rw1 = FALSE, data=data)

##DLE and efficacy samples must be of 'Samples' Class
DLEsamples<-mcmc(data,DLEmodel,my_options)
Effsamples<-mcmc(data,Effmodel,my_options)

##target probabilities of the occurrence of a DLE during trial and at the
## end of trial are defined as 0.35 and 0.3, respectively
## Using 30% posterior quantile of the TD35 and TD30 samples as estimates of
## TD35 and TD30, function specified in TDderive slot
## Using the 50% posterio quantile of the Gstar (the dose which gives the maximum
## gain value) samples as Gstar estimate,function specified in Gstarderive slot
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})

RecommendDose<-nextBest(mynextbest,doselimit=max(data@doseGrid),samples=DLEsamples,
                        model=DLEmodel,
                        data=data,Effmodel=Effmodel,Effsamples=Effsamples)

# nolint end
