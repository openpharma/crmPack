
# create data from the 'Data" (or DataDual') class
data <- DataDual(x = c(25,50,25,50,75,300,250,150),
                 y = c(0,0,0,0,0,1,1,0),
                 doseGrid = seq(25,300,25))

## Initialize a model from 'ModelTox' class e.g using 'LogisticIndepBeta' model
DLEmodel <- LogisticIndepBeta(binDLE=c(1.05,1.8),
                              DLEweights=c(3,3),
                              DLEdose=c(25,300),
                              data=data)

options <- McmcOptions(burnin=100, step=2, samples=200)
DLEsamples <- mcmc(data=data,model=DLEmodel,options=options)

TD45 <- dose(prob=0.45, model = DLEmodel,samples = DLEsamples)

