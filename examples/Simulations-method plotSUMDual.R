##obtain the plot of the summary for the simulation results
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
##OR if the 'EffFlexi' class is used 
## for the efficacy model
Effmodel<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                    sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),smooth="RW2",data=data)
##Specify the simulations
##Please refer to design-method 'simulate DualResponsesSamplesFlexiDesign' examples for details
mySim<-simulate(object=design,
                args=NULL,
                trueDLE=myTruthDLE,
                trueEff=myTruthEff,
                trueSigma2=0.025,
                trueSigma2betaW=1,
                nsim=1,
                seed=819,
                parallel=FALSE)
##Then produce a summary of your simulations
MYSUM <- summary(mySim,
                 trueDLE=myTruthDLE,
                  trueEff=myTruthEff)

##Then plot the summary of the simulations
print(plot(MYSUM))