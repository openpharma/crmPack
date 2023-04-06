# nolint start

##obtain the plot for the simulation results
##If DLE and efficacy responses are considered in the simulations
##Specified your simulations when no samples are used
data <- DataDual(doseGrid=seq(25,300,25))
##First for the DLE model
##The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class
DLEmodel <- LogisticIndepBeta(binDLE=c(1.05,1.8),
                              DLEweights=c(3,3),
                              DLEdose=c(25,300),
                              data=data)

##The efficacy model of 'ModelEff' (e.g 'Effloglog') class
Effmodel<-Effloglog(eff=c(1.223,2.513),eff_dose=c(25,300),
                    nu=c(a=1,b=0.025),data=data)

##The escalation rule using the 'NextBestMaxGain' class
mynextbest<-NextBestMaxGain(prob_target_drt=0.35,
                            prob_target_eot=0.3)


##The increments (see Increments class examples)
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements<-IncrementsRelative(intervals=c(25,300),
                                 increments=c(2,2))
##cohort size of 3
mySize<-CohortSizeConst(size=3)
##Stop only when 36 subjects are treated
myStopping <- StoppingMinPatients(nPatients=36)
##Now specified the design with all the above information and starting with a dose of 25

##Specified the design(for details please refer to the 'DualResponsesDesign' example)
design <- DualResponsesDesign(nextBest=mynextbest,
                              model=DLEmodel,
                              Effmodel=Effmodel,
                              stopping=myStopping,
                              increments=myIncrements,
                              cohortSize=mySize,
                              data=data,startingDose=25)
##Specify the true DLE and efficacy curves
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)
myTruthEff <- efficacyFunction(Effmodel, theta1 = -4.818429, theta2 = 3.653058)

## Then specified the simulations and generate the trial for 2 times
mySim <-simulate(object=design,
                 args=NULL,
                 trueDLE=myTruthDLE,
                 trueEff=myTruthEff,
                 trueNu=1/0.025,
                 nsim=2,
                 seed=819,
                 parallel=FALSE)

##Then produce a summary of your simulations
summary(mySim,
        trueDLE=myTruthDLE,
        trueEff=myTruthEff)
##If DLE and efficacy samples are involved
##Please refer to design-method 'simulate DualResponsesSamplesDesign' examples for details
##specified the next best
mynextbest <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)
##specified the design
design <- DualResponsesSamplesDesign(nextBest=mynextbest,
                                     cohortSize=mySize,
                                     startingDose=25,
                                     model=DLEmodel,
                                     Effmodel=Effmodel,
                                     data=data,
                                     stopping=myStopping,
                                     increments=myIncrements)
##options for MCMC
##For illustration purpose, we will use 50 burn-ins to generate 200 samples
options<-McmcOptions(burnin=50,step=2,samples=200)
##The simulations
##For illustration purpose only 2 simulation is produced (nsim=2).
mySim<-simulate(design,
                args=NULL,
                trueDLE=myTruthDLE,
                trueEff=myTruthEff,
                trueNu=1/0.025,
                nsim=2,
                mcmcOptions=options,
                seed=819,
                parallel=FALSE)

##Then produce a summary of your simulations
summary(mySim,
        trueDLE=myTruthDLE,
        trueEff=myTruthEff)

# nolint end