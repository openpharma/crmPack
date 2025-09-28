## Example of combining an atomic stopping rule with a list of stopping rules
## with an AND ('&') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

myStopping3 <- StoppingMinPatients(nPatients = 20)

myStopping <- myStopping3 & (myStopping1 | myStopping2)
