##obtain the plot for the simulation results
##If DLE and efficacy responses are considered in the simulations
##Specified your simulations when no samples are used
##(Please refer to desgin-method 'simulate DualResponsesDesign' examples for details)
mySim <-simulate(object=design,
                 args=NULL,
                 trueDLE=myTruthDLE,
                 trueEff=myTruthEff,
                 trueNu=1/0.025,
                 nsim=10,
                 seed=819,
                 parallel=FALSE)
##If DLE and efficacy samples are involved
##Please refer to design-method 'simulate DualResponsesSamplesDesign' examples for details
mySim<-simulate(design,
                args=NULL,
                trueDLE=myTruthDLE,
                trueEff=myTruthEff,
                trueNu=1/0.025,
                nsim=10,
                mcmcOptions=options,
                seed=819,
                parallel=FALSE)
##plot the simulation results
print(plot(mySim))