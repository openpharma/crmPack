# MCMC Sampling for Bayesian Logistic Regression Model

**\[stable\]**

Replacement for `BayesLogit::logit`. Performs MCMC sampling for Bayesian
logistic regression using JAGS.

## Usage

``` r
myBayesLogit(y, X, m0, P0, options)
```

## Arguments

- y:

  (`integer`)  
  0/1 vector of responses.

- X:

  (`matrix`)  
  design matrix.

- m0:

  (`numeric`)  
  prior mean vector.

- P0:

  (`matrix`)  
  precision matrix.

- options:

  (`McmcOptions`)  
  MCMC options.

## Value

The matrix of samples (samples x parameters).
