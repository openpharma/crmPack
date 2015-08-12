##obtain the plot for the simulation results
##If only DLE responses are considered in the simulations
##Specified your simulations when no DLE samples are used
##(Please refer to desgin-method 'simulate TDDesign' examples for details)
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=10,
                  seed=819,
                  parallel=FALSE)
##If DLE samples are involved
##Please refer to design-method 'simulate TDsamplesDesign' examples for details
mySim <- simulate(design,
                  args=NULL,
                  truth=myTruth,
                  nsim=10,
                  seed=819,
                  mcmcOptions=options,
                  parallel=FALSE)
##Then produce a summary of your simulations
MYSUM <- summary(mySim,
                 truth=myTruth)
##plot the summary of the simulations
print(plot(MYSUM))