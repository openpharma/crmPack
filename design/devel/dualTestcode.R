library(crmPack)
PL <- 0.001
data <- Data(x=c(PL, 25, 25, 25, PL, 50, 50, 50, PL, 100, 100, 100),
              y=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
              cohort=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
              doseGrid=c(PL, seq(25, 300, 25)),
              ID=1:12,
              placebo=TRUE)
emptydata <- Data(doseGrid=data@doseGrid, placebo=TRUE)
DLTmodel <- LogisticIndepBeta(binDLE=c(1.05, 1.8),
                               DLEweights=c(3, 3),
                               DLEdose=c(25, 300),
                               data=emptydata)
data2 <- DataDual(doseGrid=data@doseGrid,
                  placebo=TRUE)

## this gives an error message:
Effmodel <- Effloglog(Eff=c(1.223, 2.513),
                       Effdose=c(25, 300),
                       nu=c(a=1, b=0.025),
                       data=data2)

## but we can correct with setting c>=1:
Effmodel <- Effloglog(Eff=c(1.223, 2.513),
                      Effdose=c(25, 300),
                      nu=c(a=1, b=0.025),
                      data=data2,
                      c=2)


data3 <- DataDual(x=c(PL, 25, 25, 25, PL, 100, 100, 100, PL, 300, 300, 300),
                   y=c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
                   w=c(0, 0.2, 0.5, 1, 0, 1.9, 2.2, 2, 
                       0.03, 2.7, 2.6, 3),
                   cohort=c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                   doseGrid=data@doseGrid,
                   ID=1:12,
                   placebo=TRUE)
newEffmodel <- update(object=Effmodel, data=data3)
newDLTmodel <- update(object=DLTmodel, data=data3)

GainNextBest <- NextBestMaxGain(DLEDuringTrialtarget=0.35,
                                DLEEndOfTrialtarget=0.3)
#plot of the efficacy curve
plot(data3, newEffmodel)

#probabilities of DLE at all dose
prob(data3@doseGrid,newDLTmodel)
DLE <- prob(data3@doseGrid,newDLTmodel)
ExpEff(data3@doseGrid,newEffmodel)

Eff <- ExpEff(data3@doseGrid,newEffmodel)

##calculate gain values at each dose levels including the placebo
gain(data3@doseGrid,DLEmodel=newDLTmodel,Effmodel=newEffmodel)

## plot continuously:
plotgrid <- seq(from=0.001, to=300, length=300)
gainvals <- gain(plotgrid, 
                 DLEmodel=newDLTmodel, 
                 Effmodel=newEffmodel)
effvals <- ExpEff(plotgrid, newEffmodel)
dlevals <- prob(plotgrid,newDLTmodel)

plot(plotgrid, effvals, type="l", col="blue")
lines(plotgrid, gainvals, col="black")
lines(plotgrid, dlevals, col="red")

##The follwoing is the same
Eff*(1-DLE)


##Using the optim function to find the max gain value and Gstar
##Define inverse gain function
              Gainfun<-function(DOSE){
                -gain(DOSE,DLEmodel=newDLTmodel,Effmodel=newEffmodel)
              }
              
              
LowestDose <- min(data3@doseGrid)

              ##Find the dose which gives the maximum gain
              Gstar<-(optim(LowestDose,Gainfun, method = "L-BFGS-B", lower=LowestDose,upper=max(data@doseGrid))$par)
              ##Find the maximum gain value
              
              MaxGain<--(optim(LowestDose,Gainfun,method = "L-BFGS-B", lower=LowestDose,upper=max(data@doseGrid))$value)
              ## be sure which doses are ok with respect to maximum

## Value of Gstar, the dose with maximum gain value
Gstar
## The maximum gain value
MaxGain

