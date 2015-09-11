##Define the NextBestMaxGainSamples class
## target probabilities of the occurrence of a DLE during trial and at the end of trial are defined as
## 0.35 and 0.3, respectively
## Using 30% posterior quantile of the TD35 and TD30 samples as estimates of TD35 and TD30, function specified
## in TDderive slot
## Using the 50% posterio quantile of the Gstar (the dose which gives the maxim gain value) samples as Gstar 
##estimate,function specified in Gstarderive slot 
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})