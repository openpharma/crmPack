
# Create some data from the class 'DataDual'
myData <- DataDual(x=c(0.1,0.5,1.5,3,6,10,10,10),
                   y=c(0,0,0,0,0,0,1,0),
                   w=rnorm(8),
                   doseGrid=c(0.1,0.5,1.5,3,6,
                              seq(from=10,to=80,by=2)))