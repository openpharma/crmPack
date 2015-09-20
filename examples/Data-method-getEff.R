## Separate the efficacy responses of subjects with or without DLE
## data must be specified for in 'DataDual' class
data<-DataDual(x=c(25,50,25,50,75,300,250,150),
               y=c(0,0,0,0,0,1,1,0),
               w=c(0.31,0.42,0.59,0.45,0.6,0.7,0.6,0.52),
               doseGrid=seq(25,300,25))
##Display the efficacy response and their corresponding dose levels 
## treated at when no or a DLE is observed
getEff(data)
