##Update the 'LogisticIndepBeta' model with new data
## first define the data and the model
emptydata<-Data(doseGrid=seq(25,300,25))
data<-emptydata

model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##Then we have some new observations data
data<-Data(x=c(25,50,50,75,100,100,225,300),
           y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(from=25,to=300,by=25))
##update the model to get new estimates
newModel <- update(object=model,data=data)