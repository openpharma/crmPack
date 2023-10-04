# nolint start

# Create some data
data <- Data(x=c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
             y=c(0, 0, 0, 0, 0, 0, 1, 0),
             cohort=c(0, 1, 2, 3, 4, 5, 5, 5),
             doseGrid=
               c(0.1, 0.5, 1.5, 3, 6,
                 seq(from=10, to=80, by=2)))

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           ref_dose=56)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=2000)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(intervals=c(0, 20),
                                   increments=c(1, 0.33))
nextMaxDose <- maxDose(myIncrements,
                       data=data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           max_overdose_prob=0.25)

# Calculate the next best dose
doseRecommendation <- nextBest(myNextBest,
                               doselimit=nextMaxDose,
                               samples=samples, model=model, data=data)

# Define the stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts = 3L)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35),
                                  prob = 0.5)
myStopping3 <- StoppingMinPatients(nPatients = 20L)

# Create a list of stopping rules (of class 'StoppingList') which will then be
# summarized (in this specific example) with the 'any' function, meaning that the study
# would be stopped if 'any' of the single stopping rules is TRUE.
mystopping <- StoppingList(stop_list=c(myStopping1,myStopping2,myStopping3),
                           summary=any)

# Evaluate if to stop the Trial
stopTrial(stopping=myStopping, dose=doseRecommendation$value,
          samples=samples, model=model, data=data)

# nolint end
