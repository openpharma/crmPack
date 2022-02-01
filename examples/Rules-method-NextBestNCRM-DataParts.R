# nolint start

# create an object of class 'DataParts'
data <- DataParts(x=c(0.1,0.5,1.5),
                    y=c(0,0,0),
                    doseGrid=c(0.1,0.5,1.5,3,6,
                               seq(from=10,to=80,by=2)),
                    part=c(1L,1L,1L),
                    nextPart=1L,
                    part1Ladder=c(0.1,0.5,1.5,3,6,10))

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

myIncrements <- IncrementsRelativeParts(dltStart=0,
                                        cleanStart=1)
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
                               samples=samples, 
                               model=model, 
                               data=data)

# nolint end
