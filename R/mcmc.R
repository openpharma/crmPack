#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com ],
##         Wai Yin Yeung [,w *.* yeung *a*t* lancaster *.* ac *.* uk]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[mcmc.R] by DSB Mit 13/05/2015 23:10>
##
## Description:
## Methods for producing the MCMC samples from Data and Model input.
##
## History:
## 31/01/2014   file creation
## 08/12/2014   no longer rely on R2WinBUGS for write.model but get it internal
## 10/07/2015   added mcmc methods for pseudo model class
###################################################################################

##' @include helpers.R
##' @include Samples-class.R
##' @include writeModel.R
{}


## --------------------------------------------------
## The generic function
## --------------------------------------------------


##' Obtain posterior samples for all model parameters
##'
##' This is the function to actually run the MCMC machinery to produce posterior
##' samples from all model parameters and required derived values. It is a
##' generic function, so that customized versions may be conveniently defined
##' for specific subclasses of GeneralData, GeneralModel, and McmcOptions input.
##'
##' Reproducible samples can be obtained by setting the seed via
##' \code{\link{set.seed}} before in the user code as usual. However, note that
##' because the RNG sampler used is external to R, running this MCMC function
##' will not change the seed position -- that is, the repeated call to this
##' function will then result in exactly the same output.
##'
##' @param data The data input, an object of class \code{\linkS4class{GeneralData}}
##' @param model The model input, an object of class \code{\linkS4class{GeneralModel}}
##' @param options MCMC options, an object of class
##' \code{\linkS4class{McmcOptions}}
##' @param \dots unused
##'
##' @return The posterior samples, an object of class
##' \code{\linkS4class{Samples}}.
##'
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

##' @describeIn mcmc Standard method which uses JAGS/BUGS
##'
##' @param program the program which shall be used: either \dQuote{JAGS} (default),
##' \dQuote{OpenBUGS} or \dQuote{WinBUGS}
##' @param verbose shall progress bar and messages be printed? (not default)
##'
##' @importFrom rjags jags.model jags.samples
##' @importFrom utils capture.output
setMethod("mcmc",
          signature=
          signature(data="GeneralData",
                    model="GeneralModel",
                    options="McmcOptions"),
          def=
          function(data, model, options,
                   program=c("JAGS", "OpenBUGS", "WinBUGS"),
                   verbose=FALSE,
                   ...){

              ## select program
              program <- match.arg(program)

              ## get a temp directory
              bugsTempDir <- file.path(tempdir(), "bugs")
              ## don't warn, because the temp dir often exists (which is OK)
              dir.create(bugsTempDir, showWarnings=FALSE)

              options(BRugsVerbose=verbose)
              if(verbose)
              {
                  cat("Using temporary directory", bugsTempDir, "\n")
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
              writeModel(bugsModel, modelFileName)

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

              ## now decide whether JAGS or something else is used
              if(program=="JAGS")
              {
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

                  ## specify the JAGS model
                  jagsModel <-
                      rjags::jags.model(file=modelFileName,
                                        data=
                                        if(fromPrior) modelspecs else
                                        c(requiredData,
                                          modelspecs),
                                        inits=
                                        ## add the RNG seed to the inits list:
                                        ## (use Mersenne Twister as per R
                                        ## default)
                                        c(inits,
                                          list(.RNG.name="base::Mersenne-Twister",
                                               .RNG.seed=rSeed)),
                                        quiet=!verbose,
                                        ## important for
                                        ## reproducibility:
                                        n.adapt=0)

                  ## run for the burnin time -> but don't show progress bar for
                  ## this one in order not to confuse the user
                  update(jagsModel,
                         n.iter=options@burnin,
                         progress.bar="none")

                  ## afterwards generate more samples and save every step one
                  ## code is:
                  samplesCode <- "samples <-
                      rjags::jags.samples(model=jagsModel,
                                          variable.names=model@sample,
                                          n.iter=
                                              (options@iterations - options@burnin),
                                          thin=options@step,
                                          progress.bar=
                                              ifelse(verbose,
                                                     'text',
                                                     'none'))"

                  ## evaluate with or without outstream capturing
                  if(verbose)
                  {
                      eval(parse(text=samplesCode))
                  } else {

                      ## this is necessary because some outputs
                      ## are written directly from the JAGS compiled
                      ## code to the outstream
                      capture.output(eval(parse(text=samplesCode)))
                  }

                  ## reformat slightly for Samples object
                  ret <- lapply(samples,
                                function(x) {
                                    ## take the first chain (because we use only
                                    ## one anyway), and take all samples (burnin
                                    ## was already not saved!)
                                    x <- x[, , 1L]
                                    ## transpose if it is a matrix
                                    ## (in case that there are multiple parameters
                                    ## in a node)
                                    if(is.matrix(x))
                                    {
                                        x <- t(x)
                                    }
                                    x
                                })

              } else {
                  ## here we use OpenBUGS or WinBUGS.
                  require("R2WinBUGS")

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

              }

              ## form a Samples object for return
              ret <- Samples(data=ret,
                             options=options)
              return(ret)
          })



## --------------------------------------------------
## The fast method for the LogisticNormal class
## --------------------------------------------------

##' @describeIn mcmc The fast method for the LogisticNormal class
##'
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
              ret <- Samples(data=samples,
                             options=options)

              return(ret)
          })

## ----------------------------------------------------------------------------------
## Obtain postrior samples for the two-parameter logistic pseudo DLE model
## -------------------------------------------------------------------------------


##' Obtain postrior samples for the two-parameter logistic pseudo DLE model
##' 
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param model The model input, an object of class \code{\linkS4class{LogisticIndepBeta}}
##' @param options MCMC options, an object of class
##' \code{\linkS4class{McmcOptions}}
##' @param \dots unused
##'
##' @return The posterior samples, an object of class
##' \code{\linkS4class{Samples}}.
##'
##' @export
##' @keywords methods
setMethod("mcmc",
          signature=
            signature(data="Data",
                      model="LogisticIndepBeta",
                      options="McmcOptions"),
          def=
            function(data, model, options,
                     verbose=FALSE,
                     ...){
              
              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L
              
              
              ##probabilities of risk of DLE at all dose levels
              pi<-(model@binDLE)/(model@DLEweights)
              ##scalar term for the covariance matrix
              scalarI<-model@DLEweights*pi*(1-pi)
              ##
              cov<-matrix(rep(0,4),nrow=2,ncol=2)
              
              for (i in (1:(length(model@binDLE)))){
                
                covmat<-scalarI[i]*matrix(c(1,log(model@DLEdose[i]),log(model@DLEdose[i]),(log(model@DLEdose[i]))^2),2,2)
                cov<-cov+covmat
              }
              
              if(fromPrior){
                ## sample from the (asymptotic) bivariate normal prior for theta
                
                tmp <- mvtnorm::rmvnorm(n=sampleSize(options),
                                        mean=c(slot(model,"phi1"),slot(model,"phi2")),
                                        sigma=solve(cov)) 
                
                
                samples <- list(phi1=tmp[, 1],
                                phi2=tmp[, 2])
              } else {
                
                ## set up design matrix
                X <- cbind(1, log(data@x))
                
                weights<-rep(1,length(data@y))
                ##probabilities of risk of DLE at all dose levels
                pi<-(data@y)/weights
                ##scalar term for the covariance matrix
                scalarI<-weights*pi*(1-pi)
                ##
                
                priordle<-model@binDLE
                priorw1<-model@DLEweights
                
                priordose<-model@DLEdose
                FitDLE<-glm(priordle/priorw1~log(priordose),family=binomial(link="logit"),weights=priorw1)
                SFitDLE<-summary(FitDLE)
                ##Obtain parameter estimates for dose-DLE curve
                priorphi1<-coef(SFitDLE)[1,1]
                priorphi2<-coef(SFitDLE)[2,1]
                cov1<-matrix(nrow=2,ncol=2)
                
                
                cov1<-matrix(rep(0,4),nrow=2,ncol=2)
                
                for (i in (1:(length(data@y)))){
                  
                  covmat<-scalarI[i]*matrix(c(1,log(data@x[i]),log(data@x[i]),(log(data@x[i]))^2),2,2)
                  cov1<-cov1+covmat
                }
                
                
                ## use fast special sampler here
                initRes <- BayesLogit::logit(y=data@y,
                                             X=X,
                                             m0=c(priorphi1,priorphi2),
                                             P0=cov,
                                             samp=sampleSize(options),
                                             burn=options@burnin)
                
                ## then form the samples list
                samples <- list(phi1=initRes$beta[,1],
                                phi2=initRes$beta[,2])
              }
              
              ## form a Samples object for return:
              ret <- Samples(data=samples,
                             options=options)
              
              return(ret)
            })

## ================================================================================

## -----------------------------------------------------------------------------------
## obtain the posterior samples for the Pseudo Efficacy log log model
## ----------------------------------------------------------------------------
##
##' Obtain the posterior samples for the efficacy linear log log model using pseudo 
##' data as prior
##' 
##' @export
##' @keywords methods

setMethod("mcmc",
          signature=
            signature(data="DataDual",
                      model="Effloglog",
                      options="McmcOptions"),
          def=
            function(data, model, options,
                     verbose=FALSE,
                     ...){
              
              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L
              
              thismodel <- update(object=model,data=data)
              
              
              
              
              if (length(thismodel@nu)==2) {
                nusamples <- rgamma(sampleSize(options),shape=thismodel@nu[1],rate=thismodel@nu[2])
                priornu <- mean(nusamples)} else {
                  priornu <- thismodel@nu
                  nusamples <- rep(nu,sampleSize(options))}
              
              
              
              ## sample from the (asymptotic) bivariate normal prior for theta1 and theta2
              
              tmp <- mvtnorm::rmvnorm(n=sampleSize(options),
                                      mean=c(thismodel@theta1,thismodel@theta2),
                                      sigma=solve(priornu*(thismodel@matQ))) 
              
              
              samples <- list(theta1=tmp[, 1],
                              theta2=tmp[, 2],
                              nu=nusamples)
              
              ## form a Samples object for return:
              ret <- Samples(data=samples,
                             options=options)
              
              return(ret)
            })
## ======================================================================================

