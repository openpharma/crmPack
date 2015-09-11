##define the stopping rules based on the 'StoppingCIRatio' class
##Using only DLE responses with samples
##model can be specified of 'Model' or 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##samples of 'Samples' class
samples<-mcmc(data,model,options)
##define the 'StoppingCIRatio' class
myStopping <- StoppingCIRatio(targetRatio=5)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest<-NextBestTDsamples(targetDuringTrial=0.35,targetEndOfTrial=0.3,
                              derive=function(TDsamples){quantile(TDsamples,probs=0.3)})


RecommendDose<-nextBest(tdNextBest,doselimit=max(data@doseGrid),samples=samples,model=model,data=data)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(stopping=myStopping,dose=RecommendDose$nextdose,
          samples=samples,model=model,data=data,targetEndOfTrial=0.3)
## RecommendDose$nextdose refers to the next dose obtained in RecommendDose
