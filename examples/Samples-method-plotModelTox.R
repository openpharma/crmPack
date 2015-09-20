##plot the dose-DLE curve with samples using the model from 'ModelTox' 
##class e.g. 'LogisticIndepBeta' class model
##define the model (see LogisticIndepBeta example)
model <-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
## define the samples obtained using the 'LogisticIndepGBeta' model 
## (see details in 'Samples' example) samples must be of 'Samples' class
samples <- mcmc(data=data,model=model,options=options)
## plot the fitted dose-DLE curve including the 95% credibility interval of the samples
## 'x' should be of 'Samples' class and 'y' of 'ModelTox' class
plot(x=samples,y=model)