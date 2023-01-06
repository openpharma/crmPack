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
                           max_overdose_prob=0.25)

# Choose the rule for the cohort-size
mySize1 <- CohortSizeRange(intervals=c(0, 30),
                           cohortSize=c(1, 3))
mySize2 <- CohortSizeDLT(DLTintervals=c(0, 1),
                         cohortSize=c(1, 3))
mySize <- maxSize(mySize1, mySize2)

# Choose the rule for stopping
#myStopping1 <- StoppingMinCohorts(nCohorts=3, reportLabel="default")

#myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
#                                  prob=0.5)
#myStopping3 <- StoppingMinPatients(nPatients=20)
#myStopping <- (myStopping1 | myStopping2) & myStopping3

#"c(myStopping1 | myStopping2) & myStopping3"
# (myStopping1 & myStopping3) | (myStopping2 & myStopping3)
#"c(myStopping1 | myStopping2) | myStopping3"

# myStopping <- StoppingAny(list(myStopping1, myStopping2), reportLabel = "bla") | myStopping3
# stop1or2 <- StoppingAny(list(myStopping1, myStopping2), reportLabel = "bla")
# myStopping <- stop1or2 | myStopping3

#scenarios to be covered:
#StoppingMinCohorts(nCohorts=3, reportLabel = NULL) # should be the default since "no reporting" is the default state
#StoppingMinCohorts(nCohorts=3, reportLabel = "My specific Label Term") #user specified report label
#
#StoppingMinCohorts(nCohorts=3, reportLabel = "default")

# Define the stopping rules
#myStopping1 <- StoppingMinCohorts(nCohorts=3, reportLabel="default")
#myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35),
#                                 prob=0.5)
#myStopping3 <- StoppingMinPatients(nPatients=20, reportLabel="default")

# Create a list of stopping rules (of class 'StoppingList') which will then be
# summarized (in this specific example) with the 'any' function, meaning that the study
# would be stopped if 'any' of the single stopping rules is TRUE.
#mystopping <- StoppingList(stopList=c(myStopping1,myStopping3),
#                          summary=any)


# Define some stopping rules



# Create a list of stopping rules (of class 'StoppingAll') which would then be
# summarized by the 'all' function, meaning that the study would be stopped only if
# 'all' the single stopping rules are TRUE
#mystopping <- StoppingAll(stopList=c(myStopping1, myStopping2, myStopping3))


myStopping1 <- StoppingMinCohorts(nCohorts=3, report_label = "default")
myStopping2 <- StoppingTargetProb(target=c(0.2, 0.35), prob=0.5, report_label = "default")
myStopping3 <- StoppingMinPatients(nPatients=20, report_label = "default")
#StoppingAll with reporting (needs set report Labels in atomic rules - otherwise an error message will be displayed)
myStopping <- StoppingAll(stopList=c(myStopping1, myStopping2), report=T)
#StoppingAny with reporting (needs set report Labels in atomic rules - otherwise an error message will be displayed)
myStopping <- StoppingAny(stopList=c(myStopping1, myStopping3), report=T)
#nestedStoppingAny Object with reporting (needs set report Labels in atomic rules - otherwise an error message will be displayed)
myStopping <- StoppingAny(stopList = list(StoppingAll(stopList=list(myStopping1, myStopping3),report_label = "InnerStoppingAllLabel"),myStopping2),report_label = "OuterStoppingAnyLabel")
#nestedStoppingAll Object with reporting (needs set report Labels in atomic rules - otherwise an error message will be displayed)
myStopping <- StoppingAll(stopList = list(StoppingAny(stopList=list(myStopping1, myStopping3),report_label = "InnerStoppingAnyLabel"),myStopping2),report_label = "OuterStoppingAllLabel")



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
# We only generate 1 trial outcomes here for illustration, for the actual study
# this should be increased of course
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=1000)
time <- system.time(mySims <- simulate(design,
                                       args=NULL,
                                       truth=myTruth,
                                       nsim=3,
                                       seed=819,
                                       mcmcOptions=options,
                                       parallel=FALSE))[3]

# Summarize the Results of the Simulations
summary(mySims,truth=myTruth)

# nolint end
