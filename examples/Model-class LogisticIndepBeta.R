##Obtain prior modal estimates given the pseudo data.
##First we used an empty data set such that only the dose levels under investigations are given.
##In total, 12 dose levels are under investigation ranging from 25 to 300 mg with increments of 25
##(i.e 25, 50, 75, ..., 300).
emptydata<- Data(doseGrid=seq(25,300,25))

##specified our data set is the empty data
data<-emptydata
## Given the pseudo data such that
## Fix two dose level 25 and 300 mg and specified in (DLEdose slot).
## Total number of subjects treated in each of these levels is 3, specified in (DLEweights slot).
## The number of subjects observed with a DLE is 1.05 at dose 25 mg and 1.8 at dose 300 mg, 
## and specified in (binDLE slot).
## the data set we used in the emptydata set, and specified in (data slot).
## Then to modal estimates of the model parameters.
model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

##using a data set (see data -class example specification) with observed DLE responses 
##to obtain posterior modal estimates. 
##for the model given the pseudo data

data<-Data(x=c(25,50,50,75,100,100,225,300),y=c(0,0,0,0,1,1,1,1),
           doseGrid=seq(25,300,25))

model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

