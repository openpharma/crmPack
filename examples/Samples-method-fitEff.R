##Obtain the 'fit' the middle, uppper and lower quantiles for the dose-efficacy curve
## at all dose levels using an efficacy sample, a pseudo efficacy model and the data
## data must be from 'DataDual' class
data<-DataDual(x=c(25,50,25,50,75,300,250,150),
               y=c(0,0,0,0,0,1,1,0),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25),
               placebo=FALSE)
## model must be from 'ModelEff' e.g using 'Effloglog' class
Effmodel<-Effloglog(c(1.223,2.513),c(25,300),nu=c(a=1,b=0.025),data=data,c=0)
## samples must be from 'Samples' class (object slot in fit)
options<-McmcOptions(burnin=100,step=2,samples=200)
Effsamples <- mcmc(data=data,model=Effmodel,options=options)
fit(object=Effsamples, model=Effmodel,data=data)