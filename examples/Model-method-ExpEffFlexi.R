##Obtain the expected efficacy value for a given dose, the 'EffFlexi' efficacy model and 
##samples generated from this efficacy model
##The efficacy model must be from 'EffFlexi' class (model slot)
##The efficacy samples must be from 'Samples' class (samples slot)
model<- EffFlexi(Eff=c(1.223, 2.513),Effdose=c(25,300),
                 sigma2=c(a=0.1,b=0.1),sigma2betaW=c(a=20,b=50),smooth="RW2",data=data)
set.seed(94)
samples<-mcmc(data=data,model=model,options=options)
## Given the dose 75 (dose slot)
ExpEff(dose=75,model=model,samples=samples)