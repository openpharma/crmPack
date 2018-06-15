##Obtain the 'fit' the middle, uppper and lower quantiles for the dose-DLE curve
## at all dose levels using a DLE sample, a DLE model and the data
## samples must be from 'Samples' class (object slot)
## we need a data object with doses >= 1:
data<-Data(x=c(25,50,50,75,150,200,225,300),
           y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(from=25,to=300,by=25))
## model must be from 'Model' or 'ModelTox' class e.g using 'LogisticIbdepBeta' model class
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##options for MCMC
options<-McmcOptions(burnin=100,step=2,samples=200)
## samples must be from 'Samples' class (object slot in fit)
samples<-mcmc(data,model,options)

fit(object=samples, model=model,data=data)