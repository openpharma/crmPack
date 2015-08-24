##define the NextBestTD class (no samples and based a pseudo DLE model)
##specified the target probability of the occurrence of a DLE during the trial be 0.35
##specified the target probability of the occurrence of a DLE at the end of trial be 0.3

myNextBest <-NextBestTD(targetDuringTrial=0.35,
                        targetEndOfTrial=0.3)
