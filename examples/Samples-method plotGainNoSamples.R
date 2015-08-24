##plot the dose-DLE , dose-efficacy and gain curve in the same plot with DLE and efficacy samples
##define the DLE model which must be of 'ModelTox' class 
##(e.g 'LogisticIndepBeta' class model)
DLEmodel<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
## define the efficacy model which must be of 'ModelEff' class
## (e.g 'Effloglog' class)
Effmodel<-Effloglog(Eff=c(1.223,2.513),Effdose=c(25,300),nu=c(a=0.025,b=1),data=data)
##plot the three curves of using modal estimates of model parameters at all dose levels
plotGain(DLEmodel=DLEmodel,
         Effmodel=Effmodel,
         data=data)