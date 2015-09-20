##Obtain prior estimates for the EffFlexi (efficacy model) given the pseudo data.
##First define an empty data set by only define the dose levels used in the study
## 12 dose levels are usesd from 25 to 300 mg with increments of 25.
emptydata<-DataDual(doseGrid=seq(25,300,25))
data<-emptydata
## define the pseudo data as first fixed 2 dose levels 25 and 300 mg and 
## specified in (Effdose slot).
## Then the efficacy responses observed at these two dose levels are 1.223 and 2.513 and 
## specified in (Eff slot).
## The prior variance of the pseudo efficay responses. This can be either a fixed value of 
## specifying the shape (a) and the rate (b) parameters for the inverse gamma distribution 
## in (sigma2 slot). The prior variance of the random walk model which can be a fixed value or 
## two value for the shape (a) and rate (b) parameter of the inverse gamma distribution in 
## (sigma2betaW slot). The data are specified in (data slot)


Effmodel<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                    sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),
                    smooth="RW2",data=data)

##Obtain estimates from the model given some observed responses
## first specified the data
data<-DataDual(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))

Effmodel<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                    sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),
                    smooth="RW2",data=data)

