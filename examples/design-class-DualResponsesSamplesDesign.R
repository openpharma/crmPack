##Construct the DualResponsesSamplesDesign for simulations
##The design comprises the DLE and efficacy models, the escalation rule, starting data, 
##a cohort size and a starting dose
##Define your data set first using an empty data set 
## with dose levels from 25 to 300 with increments 25
data <- DataDual(doseGrid=seq(25,300,25))

## First for the DLE model and DLE samples
## The DLE model must be of 'ModelTox' 
## (e.g 'LogisticIndepBeta') class and 
## DLEsamples of 'Samples' class
options<-McmcOptions(burnin=100,step=2,samples=200)
DLEmodel <- LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),
                              DLEdose=c(25,300),data=data)
DLEsamples<-mcmc(data,DLEmodel,options)
##The efficacy model of 'ModelEff' (e.g 'Effloglog') class and the efficacy samples
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)
Effsamples<-mcmc(data,Effmodel,options)
##The escalation rule using the 'NextBestMaxGainSamples' class
mynextbest<-NextBestMaxGainSamples(DLEDuringTrialtarget=0.35,
                                   DLEEndOfTrialtarget=0.3,
                                   TDderive=function(TDsamples){
                                     quantile(TDsamples,prob=0.3)},
                                   Gstarderive=function(Gstarsamples){
                                     quantile(Gstarsamples,prob=0.5)})

RecommendedDose<-nextBest(mynextbest,doselimit=max(data@doseGrid),samples=DLEsamples,
                          model=DLEmodel, data=data,Effmodel=Effmodel,Effsamples=Effsamples)
##The increments (see Increments class examples) 
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements<-IncrementsRelative(intervals=c(25,300),
                                 increments=c(2,2))
##cohort size of 3
mySize<-CohortSizeConst(size=3)
##Stop only when 36 subjects are treated
myStopping <- StoppingMinPatients(nPatients=36)
##Now specified the design with all the above information and starting with a dose of 25

design <- DualResponsesSamplesDesign(nextBest=mynextbest,
                                     cohortSize=mySize,
                                     startingDose=25,
                                     model=DLEmodel,
                                     Effmodel=Effmodel,
                                     data=data,
                                     stopping=myStopping,
                                     increments=myIncrements)

