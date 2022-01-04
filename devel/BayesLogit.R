# nolint start
## Title: Try to reproduce outcome from BayesLogit  with MCMCpack functions
## Purpose: Replace BayesLogit
## Author: Daniel Sabanes Bove (sabanesd@roche.com)
## Date: Mon Feb 05 16:55:22 2018

library(BayesLogit)
library(MCMCpack)
library(rjags)

## new helper function
myBayesLogit <- function(y, ## 0/1 vector of responses
                         X, ## design matrix
                         m0, ## prior mean vector
                         P0, ## precision matrix
                         options) ## McmcOptions object
{
  ## assertions
  p <- length(m0)
  nObs <- length(y)
  stopifnot(is.vector(y),
            all(y %in% c(0, 1)),
            is.matrix(P0),
            identical(dim(P0), c(p, p)),
            is.matrix(X),
            identical(dim(X), c(nObs, p)),
            is(options, "McmcOptions"))
  
  ## get or set the seed
  rSeed <- try(get(".Random.seed", envir = .GlobalEnv),
               silent=TRUE)
  if(is(rSeed, "try-error"))
  {
    set.seed(floor(runif(n=1, min=0, max=1e4)))
    rSeed <- get(".Random.seed", envir = .GlobalEnv)
  }
  ## .Random.seed contains two leading integers where the second
  ## gives the position in the following 624 long vector (see
  ## ?set.seed). Take the current position and ensure positivity
  rSeed <- abs(rSeed[-c(1:2)][rSeed[2]])
  
  ## get a temp directory
  bugsTempDir <- file.path(tempdir(), "bugs")
  ## don't warn, because the temp dir often exists (which is OK)
  dir.create(bugsTempDir, showWarnings=FALSE)
  
  ## build the model according to whether we sample from prior 
  ## or not:
  bugsModel <- function()
  {
    for (i in 1:nObs)
    {
      y[i] ~ dbern(p[i])
      logit(p[i]) <- mu[i]
    }
    
    mu <- X[,] %*% beta
    
    ## the multivariate normal prior on the coefficients
    beta ~ dmnorm(priorMean[], priorPrec[,])
  }
  
  ## write the model file into it
  modelFileName <- file.path(bugsTempDir, "bugsModel.txt")
  h_write_model(bugsModel, modelFileName)
  
  jagsModel <- rjags::jags.model(modelFileName,
                                 data = list('X' = X,
                                             'y' = y,
                                             'nObs' = nObs,
                                             priorMean = m0,
                                             priorPrec = P0),
                                 inits=
                                   ## add the RNG seed to the inits list:
                                   ## (use Mersenne Twister as per R
                                   ## default)
                                   list(.RNG.name="base::Mersenne-Twister",
                                          .RNG.seed=rSeed),
                                 n.chains = 1,
                                 n.adapt = 0)
  ## burn in
  update(jagsModel,
         n.iter=options@burnin,
         progress.bar="none")
  
  ## samples
  samples <-
    rjags::jags.samples(model=jagsModel,
                        variable.names="beta",
                        n.iter=
                          (options@iterations - options@burnin),
                        thin=options@step,
                        progress.bar="none")
  
  return(t(samples$beta[, , 1L]))
}


## From UCI Machine Learning Repository.
data(spambase);

## A subset of the data.
sbase = spambase[seq(1,nrow(spambase),10),];

X = model.matrix(is.spam ~ word.freq.free + word.freq.1999, data=sbase);
y = sbase$is.spam;

## Run logistic regression.
output = logit(y, X, samp=10000, burn=100);
str(output)

## coefficient samples are in here:
output$beta

## now try the same with MCMCpack:
initres <- MCMClogit(is.spam ~ word.freq.free + word.freq.1999, 
                     data=sbase,
                     burnin=100,
                     mcmc=10000)
str(initres)

## coefficient samples are in here:
initres

## and now with our own JAGS based mcmc:
res3 <- myBayesLogit(y=y,
                     X=X,
                     m0=rep(0, 3),
                     P0=diag(3),
                     options=McmcOptions(burnin=100, samples = 10000))
str(res3)

for(i in 1:3)
{
  par(mfrow=c(3, 1))
  hist(output$beta[, i], 
       main=paste("BayesLogit", i))
  hist(initres[, i],
       main=paste("MCMCpack", i))
  hist(initres[, i],
       main=paste("myBayesLogit (JAGS)", i))
}

## so this looks comparable - ok

## Winnie's example:
priorphi1 <- -1.946152
priorphi2 <- 0.4122909
precision <- matrix(c(1.402500, 6.303606,
                      6.303606, 30.495350),
                    nrow=2,
                    byrow=TRUE)

data<-Data(x=c(25,50,50,75,100,100,225,300),y=c(1,0,0,0,1,1,1,0),  doseGrid=seq(25,300,25))

model<-LogisticIndepBeta(binDLE=c(1.05,1.8),DLEweights=c(3,3),DLEdose=c(25,300),data=data)

options<-McmcOptions(burnin=100,step=2,samples=10000)

## The old code for package BayesLogit  for InitRes was

X <- cbind(1, log(data@x))
initRes <- BayesLogit::logit(y=data@y,
                             X=X,
                             m0=c(priorphi1,priorphi2),
                             P0=precision,
                             samp=sampleSize(options),
                             burn=options@burnin)
samples <- initRes$beta
head(samples)
plot(samples[, 1])

## with our version
res3 <- myBayesLogit(y=data@y,
                     X=X,
                     m0=c(priorphi1,priorphi2),
                     P0=precision,
                     options=options)
head(res3)
plot(res3[, 1])
## so JAGS mixing is not as good as BayesLogit


for(i in 1:2)
{
  par(mfrow=c(2, 1))
  hist(samples[, i], 
       main=paste("BayesLogit", i))
  hist(res3[, i],
       main=paste("myBayesLogit (JAGS)", i))
}

tempData <- data.frame(y=data@y,
                       logx=log(data@x))
initRes2 <- MCMCpack::MCMClogit(formula = y ~ logx,
                               data=tempData,
                               b0=c(priorphi1,priorphi2), BO=precision, 
                               mcmc=options@iterations - options@burnin,
                               burnin=options@burnin,
                               thin=options@step,
                               seed=19,
                               beta.start=c(0, 0))
head(initRes2)
hist(initRes2[, 1])
# nolint end
