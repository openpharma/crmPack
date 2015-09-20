##define the NextBestMaxGainsamples class
##specified the target probability of the occurrence of a DLE during the trial be 0.35
##specified the target probability of the occurrence of a DLE at the end of trial be 0.3
## we want the use the 30% posterior quantile (the 30th percentaile) of the TD35 
## (the dose level with probability of DLE equals 0.35) and TD30 samples
## For Gstar (the dose which gives tha maximum
##gain) samples, we will use the 50% posterio quantile (the median or th 50th percentile) 
## of the Gstar sample
##A function is then defined in the 'TDderive' slot for the TD30 and TD35 samples
## and another function is defined in the 'Gstarderive' slot for Gstar samples
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})
