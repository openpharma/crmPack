# nolint start

##obtain the plot for the simulation results
##If only DLE responses are considered in the simulations
##Specified your simulations when no DLE samples are used
data <- Data(doseGrid=seq(25,300,25))

##The design only incorporate DLE responses and DLE samples are involved
##Specified the model of 'ModelTox' class eg 'LogisticIndepBeta' class model
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##Then the escalation rule
tdNextBest <- NextBestTD(targetDuringTrial=0.35,
                         targetEndOfTrial=0.3)

##Then the starting data, an empty data set
emptydata<-Data(doseGrid=seq(25,300,25))
## The cohort size, size of 3 subjects
mySize <-CohortSizeConst(size=3)
##Deifne the increments for the dose-escalation process
##The maximum increase of 200% for doses up to the maximum of the dose specified in the doseGrid
##The maximum increase of 200% for dose above the maximum of the dose specified in the doseGrid
##This is to specified a maximum of 3-fold restriction in dose-esclation
myIncrements<-IncrementsRelative(intervals=c(min(data@doseGrid),max(data@doseGrid)),
                                 increments=c(2,2))
##Specified the stopping rule e.g stop when the maximum sample size of 36 patients has been reached
myStopping <- StoppingMinPatients(nPatients=36)


##Specified the design(for details please refer to the 'TDDesign' example)
design <- TDDesign(model=model,
                   nextBest=tdNextBest,
                   stopping=myStopping,
                   increments=myIncrements,
                   cohortSize=mySize,
                   data=data,startingDose=25)

# Specify the truth of the DLE responses.
myTruth <- probFunction(model, phi1 = -53.66584, phi2 = 10.50499)

##The simulations
##For illustration purpose only 1 simulation is produced (nsim=1).
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=1,
                  seed=819,
                  parallel=FALSE)
##Then produce a summary of your simulations
MYSUM <- summary(mySim,
                 truth=myTruth)
##show the summary of the simulated results in a data frame
show(MYSUM)



##If DLE samples are involved
##The escalation rule
tdNextBest<-NextBestTDsamples(targetDuringTrial=0.35,
                              targetEndOfTrial=0.3,
                              derive=function(TDsamples){quantile(TDsamples,probs=0.3)})
##The design
design <- TDsamplesDesign(model=model,
                          nextBest=tdNextBest,
                          stopping=myStopping,
                          increments=myIncrements,
                          cohortSize=mySize,
                          data=data,startingDose=25)
##Options for MCMC
##For illustration purpose, we will use 50 burn-ins to generate 200 samples and
##only simulate for 2 trials (nsim=2)
options<-McmcOptions(burnin=50,step=2,samples=200)
##The simualtions
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=2,
                  seed=819,
                  mcmcOptions=options,
                  parallel=FALSE)
##Then produce a summary of your simulations
MYSUM <- summary(mySim,
         truth=myTruth)
##show the summary of the simulated results in a data frame
show(MYSUM)

# nolint end
