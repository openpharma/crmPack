##Obtain prior modal estimates for the Effloglog model (efficacy model) given the pseudo data.
##First define an empty data set by only define the dose levels used in the study,
## 12 dose levels are usesd from 25 to 300 mg with increments of 25.
emptydata<-DataDual(doseGrid=seq(25,300,25))
data<-emptydata
## define the pseudo data as first fixed 2 dose levels 25 and 300 mg and specified in 
## (Effdose slot).
## Then the efficacy responses observed at these two dose levels are 1.223 and 2.513 and 
## specified in (Eff slot).
## The prior precision of the pseudo efficay responses. This can be either a fixed value of 
## specifying the shape (a) and the rate (b) parameters for the gamma distribution in (nu slot).
## Then specify all data currentl available in (data slot).

Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=data)

##Obtain posterior modal estimates and other estimates from the model given some observed responses
## If there is some observations available
## first specified the data
data<-DataDual(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))

Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=1,b=0.025),data=data)


