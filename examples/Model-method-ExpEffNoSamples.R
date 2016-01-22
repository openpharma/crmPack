##Obtain the expected efficacy value for a given dose and a given pseudo efficacy model 

##The efficacy model must be from 'ModelEff' class (model slot)
emptydata<-DataDual(doseGrid=seq(25,300,25))
data<-emptydata

model<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=data)


## Given the dose 75 (dose slot)
ExpEff(dose=75,model=model)