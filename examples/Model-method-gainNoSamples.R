##Obtain the gain value for a given dose, a pseudo DLE model and  a pseudo efficacy model
## without samples
##The DLE model must be from 'ModelTox' class (DLEmodel slot)
emptydata<- DataDual(doseGrid=seq(25,300,25))
data<-Data(doseGrid=seq(25,300,25))

DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##The efficacy model must be from 'Effloglog' class  (Effmodel slot)
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=emptydata)
## Given a dose level 75,
gain(dose=75,DLEmodel=DLEmodel,Effmodel=Effmodel)