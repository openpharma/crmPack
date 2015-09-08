
# Define some stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)

# Create a list of stopping rules (of class 'StoppingAny') which will then be
# summarized with the 'any' function, meaning that the study would be stopped if
# 'any' of the single stopping rules is TRUE.
mystopping <- StoppingAny(stopList=c(myStopping1,myStopping2,myStopping3))


