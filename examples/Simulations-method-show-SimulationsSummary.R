# nolint start

# Define the dose-grid
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           ref_dose=56)

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
myStopping1 <- StoppingMinCohorts(nCohorts = 4)
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
                                  prob=0.5)
myStopping3 <- StoppingMinPatients(nPatients=20)



as.character(StoppingMinCohorts(nCohorts = 4))
as.character(StoppingCohortsNearDose(n=5,percentage = 76))
as.character(StoppingPatientsNearDose(nPatients=5,percentage = 75))
as.character(StoppingMinPatients(nPatients = 5))
as.character(StoppingTargetProb(target=c(0.2,0.35),prob=0.5))
as.character(StoppingMTDdistribution(target=0.33,thresh=0.5,prob=0.9))
as.character(StoppingMTDCV(target=0.3,thresh_cv=40))
as.character(StoppingTargetBiomarker(target = c(0.9, 1),scale="relative",prob = 0.5))
as.character(StoppingHighestDose())
as.character(StoppingTDCIRatio(targetRatio = 5,targetEndOfTrial = 0.3))
as.character(StoppingGstarCIRatio(targetRatio = 5,targetEndOfTrial = 0.3))
as.character(StoppingGstarCIRatio(targetRatio = 5,targetEndOfTrial = 0.3))




myStopping <- (myStopping1 | myStopping2) & myStopping3
#myStoppingAll <- myStopping1 & myStopping2


#mystoppinglist <- StoppingList(stopList=c(myStopping1,myStopping2,myStopping3),
#                           summary=any)

as.character(mystoppinglist)

as.character(myStopping)

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
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

# Run the simulation on the desired design
# We only generate 1 trial outcome here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=1000)
time <- system.time(mySims <- simulate(design,
                                       args=NULL,
                                       truth=myTruth,
                                       nsim=2,
                                       seed=819,
                                       mcmcOptions=options,
                                       parallel=FALSE))[3]

# Show the Summary of the Simulations
#show(summary(mySims,truth=myTruth))

#summary(mySims,truth=myTruth)
# nolint end
