
# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model 
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           refDose=56)

# Choose the rule for selecting the next dose 
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)

# Choose the rule for the cohort-size 
mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
myStopping1 <- StoppingMinCohorts(nCohorts=3)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)
myStopping <- (myStopping1 & myStopping2) | myStopping3

# Choose the rule for dose increments
myIncrements <- IncrementsRelative(intervals=c(0, 20),
                                   increments=c(1, 0.33))

# Initialize the design
design <- Design(model=model,
                 nextBest=myNextBest,
                 stopping=myStopping,
                 increments=myIncrements,
                 cohortSize=mySize,
                 data=emptydata,
                 startingDose=3)

## define the true function
myTruth <- function(dose)
{
  model@prob(dose, alpha0=7, alpha1=8)
}

# Run the simulation on the desired design
# We only generate 1 trial outcome here for illustration, for the actual study 
# this should be increased of course
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=1000)
time <- system.time(mySims <- simulate(design,
                                       args=NULL,
                                       truth=myTruth,
                                       nsim=1,
                                       seed=819,
                                       mcmcOptions=options,
                                       parallel=FALSE))[3]

# Plot the results of the simulation
print(plot(mySims))
print(plot(mySims@data[[1]]))



##obtain the plot for the simulation results
##If only DLE responses are considered in the simulations
##Specified your simulations when no DLE samples are used
##Define your data set first using an empty data set 
## with dose levels from 25 to 300 with increments 25
data <- Data(doseGrid=seq(25,300,25))

##Specified the model of 'ModelTox' class eg 'LogisticIndepBeta' class model
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##Then the escalation rule
tdNextBest <- NextBestTD(targetDuringTrial=0.35,
                         targetEndOfTrial=0.3)

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
##Now specified the design with all the above information and starting with a dose of 25
design <- TDDesign(model=model,
                   nextBest=tdNextBest,
                   stopping=myStopping,
                   increments=myIncrements,
                   cohortSize=mySize,
                   data=data,startingDose=25)

##Specify the truth of the DLE responses
myTruth <- function(dose)
{ model@prob(dose, phi1=-53.66584, phi2=10.50499)
}

## Then specified the simulations and generate the trial 
##For illustration purpose only 1 simulation is produced (nsim=1). 
##The simulations
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=1,
                  seed=819,
                  parallel=FALSE)


##plot the simulations
print(plot(mySim))



##If DLE samples are involved
##The escalation rule
tdNextBest<-NextBestTDsamples(targetDuringTrial=0.35,
                              targetEndOfTrial=0.3,
                              derive=function(TDsamples){quantile(TDsamples,probs=0.3)})
##specify the design
design <- TDsamplesDesign(model=model,
                          nextBest=tdNextBest,
                          stopping=myStopping,
                          increments=myIncrements,
                          cohortSize=mySize,
                          data=data,startingDose=25)
##options for MCMC
options<-McmcOptions(burnin=100,step=2,samples=200)
##The simulations
##For illustration purpose only 1 simulation is produced (nsim=1). 
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=1,
                  seed=819,
                  mcmcOptions=options,
                  parallel=FALSE)

##plot the simulations
print(plot(mySim))

