##define the stopping rules based on the 'StoppingTDsamplesCIRatio' class
##Using only DLE responses with samples
## we need a data object with doses >= 1:
data<-Data(x=c(25,50,50,75,150,200,225,300),
           y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(from=25,to=300,by=25))

##model can be specified of 'Model' or 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##samples of 'Samples' class
samples<-mcmc(data,model,options)
##define the 'StoppingTDsamplesCIRatio' class
myStopping <- StoppingTDsamplesCIRatio(targetRatio=5,
                                       targetEndOfTrial=0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest<-NextBestTDsamples(targetDuringTrial=0.35,targetEndOfTrial=0.3,
                              derive=function(TDsamples){quantile(TDsamples,probs=0.3)})


RecommendDose<-nextBest(tdNextBest,doselimit=max(data@doseGrid),samples=samples,
                        model=model,data=data)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(stopping=myStopping,dose=RecommendDose$nextdose,
          samples=samples,model=model,data=data)
## RecommendDose$nextdose refers to the next dose obtained in RecommendDose
