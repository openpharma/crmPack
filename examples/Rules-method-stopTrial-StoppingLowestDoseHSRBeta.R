
# Create the data
data <- Data(x=c(0.1, 0.1, 0.1),
             y=c(0, 0, 1),
             cohort=c(1, 1, 1),
             doseGrid=
               c(0.1, 0.5, 1.5, 3, 6,
                 seq(from=10, to=80, by=2)))

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           refDose=56)

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
                           maxOverdoseProb=0.25)

# Calculate the next best dose
doseRecommendation <- nextBest(myNextBest,
                               doselimit=nextMaxDose,
                               samples=samples, model=model, data=data)

# Define the stopping rule such that the study would be stopped if first dose
# is toxic based on a Beta posterior distribution with Beta(1,1) prior
myStopping <- StoppingLowestDoseHSRBeta(target = 0.3,
                                        prob = 0.9)

# Evaluate if to stop the trial
stopTrial(stopping=myStopping, 
          dose=doseRecommendation$value,
          samples=samples,
          model=model,
          data=data)
