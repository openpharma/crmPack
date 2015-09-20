##Obtain the expected efficacy value for a given dose, a given pseudo 
## efficacy model and a given efficacy sample
##The efficacy model must be from 'ModelEff' class (model slot)
##The efficacy sample must be from 'Samples' class (sample slot)
emptydata<-DataDual(doseGrid=seq(25,300,25))
data<-emptydata
model<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                 sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),smooth="RW2",data=data)
options<-McmcOptions(burnin=100,step=2,samples=200)
set.seed(94)
samples<-mcmc(data=data,model=model,options=options)
## Given the dose 75 (dose slot)
ExpEff(dose=75,model=model,samples=samples)