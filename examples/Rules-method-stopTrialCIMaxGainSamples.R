##define the stopping rules based on the 'StoppingCIRatio' class
##Using both DLE and efficacy responses
## we need a data object with doses >= 1:
data <-DataDual(x=c(25,50,25,50,75,300,250,150),
                y=c(0,0,0,0,0,1,1,0),
                w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
                doseGrid=seq(25,300,25))

##DLEmodel must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

##Effmodel must be  of 'ModelEff' class
##For example, the 'Effloglog' class model
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)

##DLE and efficacy samples must be of 'Samples' class
DLEsamples<-mcmc(data,DLEmodel,options)
Effsamples<-mcmc(data,Effmodel,options)

##define the 'StoppingGstarsamplesCIRatio' class
myStopping <- StoppingGstarsamplesCIRatio(targetRatio=5)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})

RecommendDose<-nextBest(mynextbest,doselimit=max(data@doseGrid),samples=DLEsamples,model=DLEmodel,
                        data=data,Effmodel=Effmodel,Effsamples=Effsamples)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial 

stopTrial(stopping=myStopping,
          dose=RecommendDose$nextdose,
          samples=DLEsamples,
          model=DLEmodel,
          data=data,
          targetEndOfTrial=0.3,
          TDderive=function(TDsamples){
            quantile(TDsamples,prob=0.3)},
          Effmodel=Effmodel,
          Effsamples=Effsamples,
          Gstarderive=function(Gstarsamples){
            quantile(Gstarsamples,prob=0.5)})

## RecommendDose$nextdose refers to the next dose obtained in RecommendDose