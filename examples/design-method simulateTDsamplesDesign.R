##Simulate dose-escalation procedure based only on DLE responses with DLE samples involved
##Specified the design(for details please refer to the 'TDsamplesDesign' example)
design <- TDsamplesDesign(model=model,
                          nextBest=tdNextBest,
                          stopping=myStopping,
                          increments=myIncrements,
                          cohortSize=mySize,
                          data=emptydata,startingDose=25)

##Specify the truth of the DLE responses
myTruth <- function(dose)
{ model@prob(dose, phi1=-53.66584, phi2=10.50499)
}
##then plot the truth to see how the truth dose-DLE curve look like
curve(myTruth(x), from=0, to=300,ylim=c(0,1))

## Then specified the simulations and generate the trial for 10 times

options<-McmcOptions(burnin=10000,step=20,samples=2000)

mySim <-  simulate(object=design,
                   args=NULL,
                   truth=myTruth,
                   nsim=10,
                   seed=819,
                   mcmcOptions=options,
                   parallel=FALSE)
