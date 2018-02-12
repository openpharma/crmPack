## Title: Try to reproduce outcome from BayesLogit  with MCMCpack functions
## Purpose: Replace BayesLogit
## Author: Daniel Sabanes Bove (sabanesd@roche.com)
## Date: Mon Feb 05 16:55:22 2018

library(BayesLogit)
library(MCMCpack)

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

for(i in 1:3)
{
  par(mfrow=c(2, 1))
  hist(output$beta[, i], 
       main=paste("BayesLogit", i))
  hist(initres[, i],
       main=paste("MCMCpack", i))
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

options<-McmcOptions(burnin=100,step=2,samples=200)

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
