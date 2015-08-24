##plot the dose-efficacy curve with samples using the model from 'ModelEff' 
##class e.g. 'Effloglog' class model
##define the model (see Effloglog example)
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)
## define the samples obtained using the 'Effloglog' model (see details in 'Samples' example)
## samples must be of 'Samples' class
samples <- mcmc(data=data,model=Effmodel,options=options)
## plot the fitted dose-efficacy curve including the 95% credibility interval of the samples
## 'x' should be of 'Samples' class and 'y' of 'ModelEff' class
plot(x=samples,y=model)