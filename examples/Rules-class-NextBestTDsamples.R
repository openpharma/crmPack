##define the NextBestTDsamples class
##specified the target probability of the occurrence of a DLE during the trial be 0.35
##specified the target probability of the occurrence of a DLE at the end of trial be 0.3
## we want the use the 30% posterior quantile (the 30th percentaile) of the TD35 (the dose level with 
##probability of DLE equals 0.35) and TD30 samples. A function is then defined in the 'derive' slot
myNextBest <-NextBestTDsamples(targetDuringTrial=0.35,
                               targetEndOfTrial=0.3,
                               derive=function(TDsamples){quantile(TDsamples,probs=0.3)})
