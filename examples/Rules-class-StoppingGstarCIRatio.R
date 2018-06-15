##Define the target stopping ratio of 5 and 
##the target probability of DLE to be used at the end of a trial
##This is a ratio of the upper to the lower 95% credibility interval of the estimates
myStopping <- StoppingGstarCIRatio(targetRatio=5,
                                   targetEndOfTrial=0.3)