#using empty data set to get prior modal estimates for the model
emptydata<- Data(doseGrid=seq(25,300,25))
data<-emptydata

model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)
##using a data set to obtain posterio modal estimates for the model
data<-Data(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(25,300,25))

model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

