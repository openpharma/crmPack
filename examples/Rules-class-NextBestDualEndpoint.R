
# Target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
# and with a probability below 0.25 that prob(DLT)>0.35 (safety)

myNextBest <- NextBestDualEndpoint(target=c(0.9, 1),
                                   overdose=c(0.35, 1),
                                   maxOverdoseProb=0.25)

## now do it with an absolute target on the natural biomarker scale:

myNextBest <- NextBestDualEndpoint(target=c(200, 300),
                                   scale="absolute",
                                   overdose=c(0.35, 1),
                                   maxOverdoseProb=0.25)