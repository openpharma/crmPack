##define the stopping rules based on the 'StoppingCIRatio' class
##Using only DLE responses 
##model must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##define the 'StoppingCIRatio' class
myStopping <- StoppingCIRatio(targetRatio=5)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest<-NextBestTD(targetDuringTrial=0.35,targetEndOfTrial=0.3)


RecommendDose<-nextBest(tdNextBest,doselimit=max(data@doseGrid),model=model,data=data)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(stopping=myStopping,dose=RecommendDose$nextdose,
          model=model,data=data,targetEndOfTrial=0.3)
## RecommendDose$nextdose refers to the next dose obtained in RecommendDose