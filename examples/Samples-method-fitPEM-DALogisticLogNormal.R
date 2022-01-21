# Create the data
data <- DataDA(x=c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
               y=c(0, 0, 1, 1, 0, 0, 1, 0),
               doseGrid=
                 c(0.1, 0.5, 1.5, 3, 6,
                   seq(from=10, to=80, by=2)),
               u=c(42,30,15,5,20,25,30,60),
               t0=c(0,15,30,40,55,70,75,85),
               Tmax=60)

# Initialize the CRM model used to model the data
npiece_ <- 10
lambda_prior<-function(k){
  npiece_/(data@Tmax*(npiece_-k+0.5))
}

model<-DALogisticLogNormal(mean=c(-0.85,1),
                           cov=matrix(c(1,-0.5,-0.5,1),nrow=2),
                           refDose=56,
                           npiece=npiece_,
                           l=as.numeric(t(apply(as.matrix(c(1:npiece_),1,npiece_),2,lambda_prior))),
                           C_par=2)

#Obtain the posterior

options <- McmcOptions(burnin=10,
                       step=2,
                       samples=1e2)

set.seed(94)
samples <- mcmc (data,model,options)


# Extract the posterior mean hazard (and empirical 2.5 and 97.5 percentile) 
# for the piecewise exponential model
# If hazard=FALSE, the posterior PEM will be plot 
fitted  <- fitPEM(object = samples,
                  model = model,
                  data=data,
                  quantiles=c(0.025,0.975),
                  middle=mean,
                  hazard=TRUE)