
## Example of combining two atomic stopping rules with an OR ('|') operator

myStopping1 <- StoppingMinCohorts(nCohorts = 3L)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)

myStopping <- myStopping1 | myStopping2




