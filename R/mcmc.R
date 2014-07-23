#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[mcmc.R] by DSB Mit 23/07/2014 03:04>
##
## Description:
## Methods for producing the MCMC samples from Data and Model input.
##
## History:
## 31/01/2014   file creation
###################################################################################

##' @include helpers.R
##' @include Samples-class.R
{}


## --------------------------------------------------
## The generic function
## --------------------------------------------------


##' Obtain posterior samples for all model parameters
##'
##' Obtain posterior samples for all model parameters
##'
##' This is the function to actually run the MCMC machinery to produce posterior
##' samples from all model parameters and required derived values. It is a
##' generic function, so that customized versions may be conveniently defined
##' for specific subclasses of Data, Model, and McmcOptions input.
##'
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param model The model input, an object of class \code{\linkS4class{Model}}
##' @param options MCMC options, an object of class
##' \code{\linkS4class{McmcOptions}}
##' @param \dots unused
##'
##' @return The posterior samples, an object of class
##' \code{\linkS4class{Samples}}.
##'
##' @genericMethods
##' @export
##' @keywords methods regression
setGeneric("mcmc",
           def=
           function(data, model, options, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("mcmc")
           },
           valueClass="Samples")


## --------------------------------------------------
## The standard method
## --------------------------------------------------

##' Standard method which uses BUGS
##'
##' @param program the program which shall be used (see
##' \code{\link[R2WinBUGS]{bugs}} for details)
##' @param verbose shall messages be printed? (not default)
##'
##' @importFrom R2WinBUGS write.model bugs
setMethod("mcmc",
          signature=
          signature(data="Data",
                    model="Model",
                    options="McmcOptions"),
          def=
          function(data, model, options,
                   program="OpenBUGS",
                   verbose=FALSE,
                   ...){

              ## get a temp directory
              bugsTempDir <- file.path(tempdir(), "bugs")
              dir.create(bugsTempDir)

              options(BRugsVerbose=verbose)
              if(verbose)
              {
                  cat("Using BUGS temporary directory", bugsTempDir, "\n")
              }

              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L

              ## and accordingly build the model
              bugsModel <-
                  if(fromPrior)
                  {
                      ## here only the prior
                      model@priormodel
                  } else {
                      ## here the data model + the prior
                      joinModels(model@datamodel,
                                 model@priormodel)
                  }

              ## write the model file into it
              modelFileName <- file.path(bugsTempDir, "bugsModel.txt")
              R2WinBUGS::write.model(bugsModel, modelFileName)

              ## get the initial values for the parameters,
              ## by evaluating the init function from the model object.
              ## This gives us a list
              inits <-
                  do.call(model@init,
                          as.list(data)[names(formals(model@init))])
              stopifnot(is.list(inits))

              ## get the model specs
              ## by evaluating the modelspecs function from the model object.
              ## This gives us a list
              modelspecs <-
                  do.call(model@modelspecs,
                          as.list(data)[names(formals(model@modelspecs))])
              stopifnot(is.list(modelspecs))

              ## prepare the required data.
              ## This is necessary, because if there is only one observation,
              ## the data is not passed correctly to openBUGS: Then x and y
              ## are treated like scalars in the data file. Therefore we add
              ## dummy values to the vectors in this case.
              requiredData <-
                  if(fromPrior)
                  {
                      ## in this case requiredData will not be used
                      NULL
                  } else if(data@nObs == 1L) {
                      ## here we need to modify!!
                      tmp <- as.list(data)[model@datanames]

                      ## get the names where to add dummy entries:
                      addWhere <- which(! (names(tmp) %in% c("nObs", "nGrid")))
                      ## all names that are not referring to the scalars
                      ## nObs and nGrid

                      for(i in addWhere)
                      {
                          tmp[[i]] <- as(c(tmp[[i]],
                                           0), ## additional zero here!
                                         class(tmp[[i]])) ## preserve class
                      }
                      ## because we don't change the number of observations
                      ## (nObs), this addition of zeros doesn't affect the
                      ## results.

                      ## return:
                      tmp

                  } else {
                      ## we can take the usual one
                      as.list(data)[model@datanames]
                  }

              ## Obtain MCMC samples:
              bugsResult <-
                  try(R2WinBUGS::bugs(data=
                                      ## combine the *required* data (as list)
                                      ## with the model specs
                                      if(fromPrior) modelspecs else
                                      c(requiredData,
                                        modelspecs),
                                      inits=
                                      ## currently only have one chain
                                      ## in this inits list
                                      list(inits),
                                      parameters.to.save=
                                      ## which parameters should be saved?
                                      model@sample,
                                      ## don't save DIC because we also want
                                      ## to sample from prior without any
                                      ## data
                                      DIC=FALSE,
                                      model=
                                      ## supply the model file
                                      "bugsModel.txt",
                                      working.directory=
                                      ## and the working directory
                                      bugsTempDir,
                                      ## supply MCMC options
                                      program=program,
                                      n.burnin=options@burnin,
                                      n.iter=options@iterations,
                                      n.thin=options@step,
                                      n.chains=1,
                                      bugs.seed=2L),
                      silent=TRUE)

              ## catch possible error here
              if(is(bugsResult, "try-error"))
              {
                  ## give the log file output
                  stop(paste(readLines(file.path(bugsTempDir, "log.txt")),
                             collapse="\n"))
              }

              ## extract the samples (really only save the parameters we wanted
              ## to sample)
              ret <- bugsResult$sims.list[model@sample]

              ## and form a Samples object from it for return
              ret <- new("Samples",
                         data=ret,
                         options=options)
              return(ret)
          })



## --------------------------------------------------
## The fast method for the LogisticNormal class
## --------------------------------------------------

##' The fast method for the LogisticNormal class
##'
##' @param verbose shall messages be printed? (not default)
##' @importFrom BayesLogit logit
setMethod("mcmc",
          signature=
          signature(data="Data",
                    model="LogisticNormal",
                    options="McmcOptions"),
          def=
          function(data, model, options,
                   verbose=FALSE,
                   ...){

              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L

              if(fromPrior)
              {
                  ## sample from the bivariate normal prior for theta
                  tmp <- mvtnorm::rmvnorm(n=sampleSize(options),
                                          mean=model@mean,
                                          sigma=model@cov)
                  samples <- list(alpha0=tmp[, 1],
                                  alpha1=tmp[, 2])
              } else {

                  ## set up design matrix
                  X <- cbind(1, log(data@x / model@refDose))

                  ## use fast special sampler here
                  initRes <- BayesLogit::logit(y=data@y,
                                               X=X,
                                               m0=model@mean,
                                               P0=model@prec,
                                               samp=sampleSize(options),
                                               burn=options@burnin)

                  ## then form the samples list
                  samples <- list(alpha0=initRes$beta[,1],
                                  alpha1=initRes$beta[,2])
              }

              ## form a Samples object for return:
              ret <- new("Samples",
                         data=samples,
                         options=options)

              return(ret)
          })


## --------------------------------------------------
## The fast method for the LogisticLogNormal class
## --------------------------------------------------

##' The fast method for the LogisticLogNormal class
##'
##' @param verbose shall messages be printed? (not default)
setMethod("mcmc",
          signature=
          signature(data="Data",
                    model="LogisticLogNormal",
                    options="McmcOptions"),
          def=
          function(data, model, options,
                   verbose=FALSE,
                   ...){

              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L

              if(fromPrior)
              {
                  ## sample from the bivariate normal prior for theta
                  tmp <- mvtnorm::rmvnorm(n=sampleSize(options),
                                          mean=model@mean,
                                          sigma=model@cov)
                  samples <- list(alpha0=tmp[, 1],
                                  alpha1=exp(tmp[, 2]))
              } else {

                  ## prior correlation
                  rho <- model@cov[1, 2] / sqrt(model@cov[1, 1] * model@cov[2, 2])

                  ## prior precisions
                  prec0 <- 1 / model@cov[1, 1]
                  prec1 <- 1 / model@cov[2, 2]

                  ## starting values
                  inits <- model@init()$theta

                  ## marginal unvariate normal prior for theta0
                  theta0 <- mcmc.normal(x=inits[1],
                                        mu=model@mean[1],
                                        tau=prec0)

                  ## moments for the conditional normal prior of theta1
                  condMu <- deterministic(function(theta0){
                      model@mean[2] + rho / sqrt(prec1) * (theta0 - model@mean[1])
                  }, theta0)

                  condTau <- prec1 / (1 - rho^2)

                  ## conditional normal prior for theta1
                  theta1 <- mcmc.normal(x=inits[2],
                                        mu=condMu,
                                        tau=condTau)

                  ## intercept and standardized log dose
                  X <- cbind(1, log(data@x / model@refDose))

                  ## the estimated probabilities
                  probs <-
                      deterministic(function(X, theta0, theta1){
                          plogis(X[,1] * theta0 + X[,2] * exp(theta1))
                      }, X, theta0, theta1)

                  ## the likelihood
                  lik <- mcmc.bernoulli(x=as.double(data@y), p=probs, observed=TRUE)

                  ## create the model
                  m <- create.model(theta0, theta1, probs, lik)

                  ## run the sampler
                  ans <- run.model(m,
                                   iterations=options@iterations,
                                   burn=options@burnin,
                                   adapt=0,
                                   thin=options@step)

                  ## select the right iterations first
                  select <- seq(from=length(ans$theta0) - sampleSize(options) + 1,
                                to=length(ans$theta0))

                  ## then form the samples list
                  samples <- list(alpha0=ans$theta0[select],
                                  alpha1=exp(ans$theta1[select]))
              }

              ## form a Samples object for return:
              ret <- new("Samples",
                         data=samples,
                         options=options)

              return(ret)
          })

## --------------------------------------------------
## JAGS for the LogisticNormalMixture method
## --------------------------------------------------

##' JAGS for the LogisticNormalMixture method
##'
##' @param verbose shall messages be printed? (not default)
##'
##' @importFrom R2WinBUGS write.model
##' @importFrom rjags jags.model jags.samples
setMethod("mcmc",
          signature=
          signature(data="Data",
                    model="LogisticNormalMixture",
                    options="McmcOptions"),
          def=
          function(data, model, options,
                   verbose=FALSE,
                   ...){

              ## get a temp directory
              bugsTempDir <- file.path(tempdir(), "bugs")
              dir.create(bugsTempDir)

              options(BRugsVerbose=verbose)
              if(verbose)
              {
                  cat("Using BUGS temporary directory", bugsTempDir, "\n")
              }

              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L

              ## and accordingly build the model
              bugsModel <-
                  if(fromPrior)
                  {
                      ## here only the prior
                      model@priormodel
                  } else {
                      ## here the data model + the prior
                      joinModels(model@datamodel,
                                 model@priormodel)
                  }

              ## write the model file into it
              modelFileName <- file.path(bugsTempDir, "bugsModel.txt")
              R2WinBUGS::write.model(bugsModel, modelFileName)

              ## get the initial values for the parameters,
              ## by evaluating the init function from the model object.
              ## This gives us a list
              inits <-
                  do.call(model@init,
                          as.list(data)[names(formals(model@init))])
              stopifnot(is.list(inits))

              ## get the model specs
              ## by evaluating the modelspecs function from the model object.
              ## This gives us a list
              modelspecs <-
                  do.call(model@modelspecs,
                          as.list(data)[names(formals(model@modelspecs))])
              stopifnot(is.list(modelspecs))

              ## prepare the required data.
              ## This is necessary, because if there is only one observation,
              ## the data is not passed correctly to openBUGS: Then x and y
              ## are treated like scalars in the data file. Therefore we add
              ## dummy values to the vectors in this case.
              requiredData <-
                  if(fromPrior)
                  {
                      ## in this case requiredData will not be used
                      NULL
                  } else if(data@nObs == 1L) {
                      ## here we need to modify!!
                      tmp <- as.list(data)[model@datanames]

                      ## get the names where to add dummy entries:
                      addWhere <- which(! (names(tmp) %in% c("nObs", "nGrid")))
                      ## all names that are not referring to the scalars
                      ## nObs and nGrid

                      for(i in addWhere)
                      {
                          tmp[[i]] <- as(c(tmp[[i]],
                                           0), ## additional zero here!
                                         class(tmp[[i]])) ## preserve class
                      }
                      ## because we don't change the number of observations
                      ## (nObs), this addition of zeros doesn't affect the
                      ## results.

                      ## return:
                      tmp

                  } else {
                      ## we can take the usual one
                      as.list(data)[model@datanames]
                  }

              ## specify the JAGS model
              jagsModel <- jags.model(file=modelFileName,
                                      data=
                                      if(fromPrior) modelspecs else
                                      c(requiredData,
                                        modelspecs),
                                      inits=
                                      list(inits),
                                      quiet=!verbose)

              ## generate samples
              samples <- jags.samples(model=jagsModel,
                                      variable.names=model@sample,
                                      n.iter=options@iterations,
                                      thin=options@step)

              ## discard burnin and reformat slightly for Samples object
              ret <- lapply(samples,
                            function(x) {
                                ## take the first chain (because we use only
                                ## one anyway), discard the burnin
                                x <- x[, - seq_len(options@burnin /
                                                   options@step), 1]
                                ## transpose if it is a matrix
                                ## (in case that there are multiple parameters
                                ## in a node)
                                if(is.matrix(x))
                                {
                                    x <- t(x)
                                }
                                x
                            })

              ## and form a Samples object from it for return
              ret <- new("Samples",
                         data=ret,
                         options=options)
              return(ret)
          })


