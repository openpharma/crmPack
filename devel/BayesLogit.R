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