
## Example of combining a list of stopping rules with an atomic stopping rule
## with an OR ('|') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3L)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)

myStopping3 <- StoppingMinPatients(nPatients = 20L)

myStopping <- (myStopping1 & myStopping2 ) | myStopping3




