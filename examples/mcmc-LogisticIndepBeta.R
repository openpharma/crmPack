##obtain mcmc DLE samples given the data, LogisticIndepBeta (DLE model) and mcmc simulations options
## data must be of 'Data' class
data<-Data(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(25,300,25))
## model must be of 'LogisticIndepBeta' class
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
## options must be ''McmcOptions' class
options<-McmcOptions(burnin=10000,step=20,samples=2000)
set.seed(94)
samples<-mcmc(data=data,model=model,options=options)