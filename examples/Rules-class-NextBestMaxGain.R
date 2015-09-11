##define the NextBestMaxGain class (no samples and based a pseudo DLE model and a efficacy model)
##specified the target probability of the occurrence of a DLE during the trial be 0.35
##specified the target probability of the occurrence of a DLE at the end of trial be 0.3

myNextBest <-NextBestMaxGain(DLEDuringTrialtarget=0.35,
                             DLEEndOfTrialtarget=0.3)