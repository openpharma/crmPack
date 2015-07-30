##Obtain prior modal estimates for the Effloglog model (efficacy model) given the pseudo data
##First define an empty data set
emptydata<-DataDual(doseGrid=seq(25,300,25))
data<-emptydata

Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)

##Obtain posterior modal estimates given the pseudo data
data<-DataDual(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)

