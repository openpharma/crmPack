
# create some data from the class 'Data'
myData <- Data(x=c(0.1,0.5,1.5,3,6,10,10,10),
               y=c(0,0,0,0,0,0,1,0),
               doseGrid=c(0.1,0.5,1.5,3,6,
                          seq(from=10,to=80,by=2)))

# Initialize the CRM model 
model <- LogisticLogNormal(mean=c(-0.85, 1),
                           cov=
                             matrix(c(1, -0.5, -0.5, 1),
                                    nrow=2),
                           refDose=56)


# Sample from the posterior distribution
options <- McmcOptions(burnin=100,
                       step=2,
                       samples=1000)

samples <- mcmc(data = myData, model = model, options=options)


