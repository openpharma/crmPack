
# Define some stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)

# Create a list of stopping rules (of class 'StoppingAll') which would then be
# summarized by the 'all' function, meaning that the study would be stopped only if
# 'all' the single stopping rules are TRUE
mystopping <- StoppingAll(stopList=c(myStopping1,myStopping2,myStopping3))


