## we need a data object with doses >= 1:
data <-DataDual(x=c(25,50,25,50,75,300,250,150),
               y=c(0,0,0,0,0,1,1,0),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))

##The 'nextBest' method using NextBestMaxGain' rules class object
## using the 'ModelTox' class DLE model 
## DLEmodel e.g 'LogisticIndepBeta' class
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

## using the 'ModelEff' class efficacy model 
## Effmodel e.g 'Effloglog' class
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=data)

##target probabilities of the occurrence of a DLE during trial and at the
## end of trial are defined as
## 0.35 and 0.3, respectively
mynextbest<-NextBestMaxGain(DLEDuringTrialtarget=0.35,DLEEndOfTrialtarget=0.3)

##doselimit is the maximum allowable dose level to be given to subjects
RecommendDose<-nextBest(mynextbest,doselimit=300,model=DLEmodel,Effmodel=Effmodel,data=data)
