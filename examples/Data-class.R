## specified a data set captured in the 'Data'  class object
## slot x are the correspondind dose levels that are treated
## slot y are the DLE (dose -limiting events) responses observed at corresponding dose levels (x) for
## 0 for no DLE and 1 for DLE)
## slot doseGrid is all the dose levels considered for the study
myData <- Data(x=c(0.1,0.5,1.5,3,6,10,10,10),
               y=c(0,0,0,0,0,0,1,0),
               doseGrid=c(0.1,0.5,1.5,3,6,
                          seq(from=10,to=80,by=2))
               