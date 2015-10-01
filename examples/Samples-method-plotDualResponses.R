## we need a data object with doses >= 1:
data <-DataDual(x=c(25,50,25,50,75,300,250,150),
                y=c(0,0,0,0,0,1,1,0),
                w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
                doseGrid=seq(25,300,25))
##plot the dose-DLE and dose-efficacy curves in two plots with DLE and efficacy samples
##define the DLE model which must be of 'ModelTox' class 
##(e.g 'LogisticIndepBeta' class model)
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
## define the efficacy model which must be of 'ModelEff' class
## (e.g 'Effloglog' class)
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)
##define the DLE sample of 'Samples' class
##set up the same data set in class 'Data' for MCMC sampling for DLE
data1 <- Data(x=data@x,y=data@y,doseGrid=data@doseGrid)
##Specify the options for MCMC
options <- McmcOptions(burnin=100,step=2,samples=1000)


DLEsamples <- mcmc(data=data1,model=DLEmodel,options=options)
##define the efficacy sample of 'Samples' class
Effsamples <- mcmc(data=data,model=Effmodel,options=options)
##plot the dose-DLE and dose-efficacy curves with two plot side by side.
##For each curve the 95% credibility interval of the two samples are alos given
plotDualResponses(DLEmodel=DLEmodel,DLEsamples=DLEsamples,
         Effmodel=Effmodel,Effsamples=Effsamples,
         data=data)