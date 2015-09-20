##obtain the plot for the simulation results
##If DLE and efficacy responses are considered in the simulations
##The efficacy model must be of 'EffFlexi' class
Effmodel<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                    sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),smooth="RW2",data=data)
## todo: how does the Effmodel come into the below code?
##Specify your simulations. Please refers to the example in design-method 'simulate' 
## DualResponsesSamplesFlexiDesign for details
mySim<-simulate(object=design,
                args=NULL,
                trueDLE=myTruthDLE,
                trueEff=myTruthEff,
                trueSigma2=0.025,
                trueSigma2betaW=1,
                nsim=1,
                seed=819,
                parallel=FALSE)
##plot this simulated results
print(plot(mySim))
