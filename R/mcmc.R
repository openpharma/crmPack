#' @include helpers.R Samples-class.R
NULL

# mcmc ----

#' Obtaining Posterior Samples for all Model Parameters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is the function that actually runs the MCMC machinery to produce
#' posterior samples from all model parameters and required derived values.
#' It is a generic function, so that customized versions may be conveniently
#' defined for specific subclasses of [`GeneralData`], [`GeneralModel`], and
#' [`McmcOptions`] input.
#'
#' @note Reproducible samples can be obtained by setting the seed using
#'   [set.seed()] in the user code as usual. However, note that because the RNG
#'   sampler used is external to R, running this MCMC function will not change
#'   the seed position, that is, the repeated call to this function will then
#'   result in exactly the same output.
#'
#' @param data (`GeneralData`)\cr an input data.
#' @param model (`GeneralModel`)\cr an input model.
#' @param options (`McmcOptions`)\cr MCMC options.
#' @param ... not used.
#'
#' @return The posterior samples, an object of class [`Samples`].
#' @export
#'
setGeneric(
  name = "mcmc",
  def = function(data, model, options, ...) {
    # There should be no default, just dispatch it to the class-specific method!
    standardGeneric("mcmc")
  },
  valueClass = "Samples"
)

## --------------------------------------------------
## The standard method
## --------------------------------------------------

##' @describeIn mcmc Standard method which uses JAGS
##'
##' @param verbose shall progress bar and messages be printed? (not default)
##' @param from_prior sample from the prior only? Defaults to checking if nObs is
##' 0. For some models it might be necessary to specify it manually here though.
##'
##' @importFrom rjags jags.model jags.samples
##' @importFrom utils capture.output
##'
##' @example examples/mcmc.R
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "GeneralModel",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 verbose = FALSE,
                 from_prior = data@nObs == 0L,
                 ...) {
    assert_flag(verbose)
    assert_flag(from_prior)

    if (verbose) {
      futile.logger::flog.threshold(futile.logger::INFO, name = "mcmc")
    } else {
      futile.logger::flog.threshold(futile.logger::FATAL, name = "mcmc")
    }

    jags_model_fun <- if (from_prior) {
      model@priormodel
    } else {
      h_join_models(model@datamodel, model@priormodel)
    }

    # Write the model into the file.
    jags_dir <- file.path(tempdir(), "R_crmPack")
    jags_file <- tempfile("jags_model_fun", jags_dir, ".txt")
    # Don't warn, as the temp dir often exists (which is OK).
    dir.create(jags_dir, showWarnings = FALSE)
    futile.logger::flog.info("Temporary directory: %s", jags_dir, name = "mcmc")
    h_write_model(jags_model_fun, jags_file)

    # Get the initial values for the parameters.
    inits <- do.call(model@init, h_slots(data, formalArgs(model@init)))
    assert_list(inits)
    inits <- inits[sapply(inits, length) > 0L]

    # Get the model specs.
    modelspecs <- do.call(
      model@modelspecs, h_slots(data, formalArgs(model@modelspecs))
    )
    assert_list(modelspecs)

    if (from_prior) {
      # Remove elements named "zeros" to avoid JAGS error of unused variables.
      modelspecs <- modelspecs[setdiff(names(modelspecs), "zeros")]
      data_model <- NULL
    } else {
      # Add dummy to ensure that e.g. `x` and `y` in `data` won't be treated as
      # scalars by `openBUGS` if `data@nObs == 0`, which leads to failures.
      add_where <- setdiff(
        model@datanames,
        c("nObs", "nGrid", "nObsshare", "yshare", "xshare", "Tmax")
      )
      data_model <- h_slots(add_dummy(data, where = add_where), model@datanames)
    }

    # Specify the JAGS model and generate samples.
    # The `inits` in `c()` below, must be a `list`!
    jags_model <- rjags::jags.model(
      file = jags_file,
      data = c(data_model, modelspecs),
      inits = c(
        inits,
        .RNG.name = h_null_if_na(options@rng_kind),
        .RNG.seed = h_null_if_na(options@rng_seed)
      ),
      quiet = !verbose,
      n.adapt = 0 # Important for reproducibility.
    )
    update(jags_model, n.iter = options@burnin, progress.bar = "none")

    jags_samples <- invisible(
      rjags::jags.samples(
        model = jags_model,
        variable.names = model@sample,
        n.iter = (options@iterations - options@burnin),
        thin = options@step,
        progress.bar = ifelse(verbose, "text", "none")
      )
    )
    futile.logger::flog.info(
      "rjags samples: ", jags_samples, name = "mcmc", capture = TRUE
    )
    samples <- lapply(jags_samples, h_extract_jags_samples)

    futile.logger::flog.remove("mcmc")
    Samples(data = samples, options = options)
  }
)

# nolint start

## --------------------------------------------------
## The method for DataMixture usage
## --------------------------------------------------

##' @describeIn mcmc Method for DataMixture with different fromPrior default
setMethod("mcmc",
          signature=
            signature(data="DataMixture",
                      model="GeneralModel",
                      options="McmcOptions"),
          def=
            function(data, model, options,
                     fromPrior=data@nObs == 0L & data@nObsshare == 0L,
                     ...){
              callNextMethod(data, model, options, fromPrior=fromPrior, ...)
            })


## --------------------------------------------------
## Replacement for BayesLogit::logit
## --------------------------------------------------

#' Do MCMC sampling for Bayesian logistic regression model
#'
#' @param y 0/1 vector of responses
#' @param X design matrix
#' @param m0 prior mean vector
#' @param P0 precision matrix
#' @param options McmcOptions object
#'
#' @importFrom rjags jags.model jags.samples
#' @return the matrix of samples (samples x parameters)
#' @keywords internal
myBayesLogit <- function(y,
                         X,
                         m0,
                         P0,
                         options)
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
                                 quiet=TRUE,
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
  samplesCode <- "samples <-
    rjags::jags.samples(model=jagsModel,
                        variable.names='beta',
                        n.iter=
                          (options@iterations - options@burnin),
                        thin=options@step,
                        progress.bar='none')"

  ## this is necessary because some outputs
  ## are written directly from the JAGS compiled
  ## code to the outstream
  capture.output(eval(parse(text=samplesCode)))

  return(t(samples$beta[, , 1L]))
}


## ----------------------------------------------------------------------------------
## Obtain posterior samples for the two-parameter logistic pseudo DLE model
## -------------------------------------------------------------------------------


##' @describeIn mcmc Obtain posterior samples for the model parameters based on the pseudo 'LogisticsIndepBeta'
##' DLE model. The joint prior and posterior probability density function of
##' the intercept \eqn{\phi_1} (phi1) and the slope \eqn{\phi_2} (phi2) are given in Whitehead and
##' Williamson (1998) and TsuTakawa (1975). However, since asymptotically, the joint posterior probability density
##' will be bivariate normal and we will use the bivariate normal distribution to
##' generate posterior samples of the intercept and the slope parameters. For the prior samples of
##' of the intercept and the slope a bivariate normal distribution with mean and the covariance matrix given in Whitehead and
##' Williamson (1998) is used.
##'
##' @importFrom mvtnorm rmvnorm
##' @example examples/mcmc-LogisticIndepBeta.R
setMethod("mcmc",
          signature=
            signature(data="Data",
                      model="LogisticIndepBeta",
                      options="McmcOptions"),
          def=
            function(data, model, options,
                     ...){

              ##update the DLE model first
              thismodel <- update(object=model,data=data)

              ## decide whether we sample from the prior or not
              fromPrior <- data@nObs == 0L


              ##probabilities of risk of DLE at all dose levels
              pi<-(thismodel@binDLE)/(thismodel@DLEweights)
              ##scalar term for the covariance matrix
              scalarI<-thismodel@DLEweights*pi*(1-pi)
              ##
              precision<-matrix(rep(0,4),nrow=2,ncol=2)

              for (i in (1:(length(thismodel@binDLE)))){

                precisionmat<-scalarI[i]*matrix(c(1,log(thismodel@DLEdose[i]),log(thismodel@DLEdose[i]),(log(thismodel@DLEdose[i]))^2),2,2)
                precision<-precision+precisionmat
              }

              if(fromPrior){
                ## sample from the (asymptotic) bivariate normal prior for theta

                tmp <- mvtnorm::rmvnorm(n=sampleSize(options),
                                        mean=c(slot(thismodel,"phi1"),slot(thismodel,"phi2")),
                                        sigma=solve(precision))


                samples <- list(phi1=tmp[, 1],
                                phi2=tmp[, 2])
              } else {


                weights<-rep(1,length(data@y))
                ##probabilities of risk of DLE at all dose levels
                pi<-(data@y)/weights
                ##scalar term for the covariance matrix
                scalarI<-weights*pi*(1-pi)
                ##

                priordle<-thismodel@binDLE
                priorw1<-thismodel@DLEweights

                priordose<-thismodel@DLEdose
                FitDLE<-suppressWarnings(glm(priordle/priorw1~log(priordose),family=binomial(link="logit"),weights=priorw1))
                SFitDLE<-summary(FitDLE)
                ##Obtain parameter estimates for dose-DLE curve
                priorphi1<-coef(SFitDLE)[1,1]
                priorphi2<-coef(SFitDLE)[2,1]

                ## use fast special sampler here
                ## set up design matrix
                X <- cbind(1, log(data@x))
                initRes <- myBayesLogit(y=data@y,
                                        X=X,
                                        m0=c(priorphi1,priorphi2),
                                        P0=precision,
                                        options=options)

                ## then form the samples list
                samples <- list(phi1=initRes[,1],
                                phi2=initRes[,2])
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
##' @describeIn mcmc Obtain the posterior samples for the model parameters in the
##' Efficacy log log model. Given the value of \eqn{\nu}, the precision of the efficacy responses,
##' the joint prior or the posterior probability of the intercept \eqn{\theta_1} (theta1) and
##' the slope \eqn{\theta_2} (theta2) is a bivariate normal distribution. The  \eqn{\nu} (nu),
##' the precision of the efficacy responses is either a fixed value or has a gamma distribution.
##' If a gamma distribution is used, the samples of nu will be first generated.
##' Then the mean of the of the nu samples
##' will be used the generate samples of the intercept and slope parameters of the model
##' @example examples/mcmc-Effloglog.R
##' @importFrom mvtnorm rmvnorm
setMethod("mcmc",
          signature=
            signature(data="DataDual",
                      model="Effloglog",
                      options="McmcOptions"),
          def=
            function(data, model, options,
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
## -----------------------------------------------------------------------------------
## obtain the posterior samples for the Pseudo Efficacy Flexible form
## ----------------------------------------------------------------------------
##
##' @describeIn mcmc Obtain the posterior samples for the estimates in the Efficacy Flexible form.
##' This is the mcmc procedure based on what is described in Lang and Brezger (2004) such that
##' samples of the mean efficacy responses at all dose levels, samples of sigma2 \eqn{sigma^2},
##' the variance of the efficacy response and samples of sigma2betaW \eqn{sigma^2_{beta_W}}, the variance of
##' the random walk model will
##' be generated. Please refer to Lang and Brezger (2004) for the procedures and the form of
##' the joint prior and posterior probability density for the mean efficacy responses. In addition,
##' both sigma2 and sigma2betaW can be fixed or having an inverse-gamma prior and posterior distribution.
##' Therefore, if the inverse gamma distribution(s) are used, the parameters in the distribution will be
##' first updated and then samples of sigma2 and sigma2betaW will be generated using the updated parameters.
##' @example examples/mcmc-EffFlexi.R
setMethod("mcmc",
          signature=
            signature(data="DataDual",
                      model="EffFlexi",
                      options="McmcOptions"),
          def=
            function(data,model,options,
                     ...){
             ##update the model
              thismodel <- update(object=model,data=data)

              nSamples <- sampleSize(options)

              ##Prepare samples container
              ###List parameter samples to save
              samples<- list(ExpEff=
                               matrix(ncol=data@nGrid, nrow=nSamples),
                             sigma2betaW=matrix(nrow=nSamples),
                             sigma2=matrix(nrow=nSamples))
              ##Prepare starting values
              ##Index of the next sample to be saved:

              iterSave <- 1L
              ##Monitoring the Metropolis-Hastings update for sigma2

              acceptHistory <- list(sigma2=logical(options@iterations))

              ## Current parameter values and also the starting values for the MCMC are set
              ## EstEff: constant, the average of the observed efficacy values

              if (length(data@w)==0){
                w1<-thismodel@Eff
                x1<-thismodel@Effdose} else {
                  ## Combine pseudo data with observed efficacy responses and no DLT observed
                  w1<-c(thismodel@Eff, getEff(data)$w_no_dlt)
                  x1<-c(thismodel@Effdose, getEff(data)$x_no_dlt)
                }
              x1Level <- matchTolerance(x1,data@doseGrid)
              ##betaW is constant, the average of the efficacy values
              betaW <- rep(mean(w1), data@nGrid)
              ##sigma2betaW use fixed value or prior mean
              sigma2betaW <-
                if (thismodel@useFixed[["sigma2betaW"]])
                {thismodel@sigma2betaW
                } else {thismodel@sigma2betaW["b"]/(thismodel@sigma2betaW["a"]-1)}
              ##sigma2: fixed value or just the empirical variance
              sigma2 <- if (thismodel@useFixed[["sigma2"]])
              {
                thismodel@sigma2
              } else {
                var(w1)
              }
              ##Set up diagonal matrix with the number of patients in the corresponding dose levels on the diagonal
              designWcrossprod <- crossprod(thismodel@designW)

              ###The MCMC cycle

              for (iterMcmc in seq_len(options@iterations))
              {## 1) Generate coefficients for the Flexible Efficacy model
                ## the variance
                adjustedVar <- sigma2
                ## New precision matrix
                thisPrecW <- designWcrossprod/adjustedVar + thismodel@RWmat/sigma2betaW
                ##draw random normal vector
                normVec <- rnorm(data@nGrid)
                ##and its Cholesky factor
                thisPrecWchol <- chol(thisPrecW)
                ## solve betaW for L^T * betaW = normVec
                betaW <- backsolve(r=thisPrecWchol,
                                   x=normVec)
                ##the residual
                adjustedW <- w1-thismodel@designW%*%betaW

                ##forward substitution
                ## solve L^T * tmp =designW ^T * adjustedW/ adjustedVar

                tmp <- forwardsolve(l=thisPrecWchol,
                                    x=crossprod(thismodel@designW,adjustedW)/adjustedVar,
                                    upper.tri=TRUE,
                                    transpose=TRUE)
                ##Backward substitution solve R*tepNew =tmp
                tmp <- backsolve(r=thisPrecWchol,
                                 x=tmp)

                ## tmp is the mean vector of the distribution
                ## add tmp to betaW to obtain final sample

                betaW <- betaW + tmp

                ## 2) Generate prior variance factor for the random walk
                ## if fixed, do nothing
                ## Otherwise sample from full condition

                if (!thismodel@useFixed$sigma2betaW)
                {
                  sigma2betaW <- rinvGamma (n=1L,
                                            a=thismodel@sigma2betaW["a"]+thismodel@RWmatRank/2,
                                            b=thismodel@sigma2betaW["b"]+crossprod(betaW,thismodel@RWmat%*%betaW)/2)
                }
                ##3) Generate variance for the flexible efficacy model
                ##if fixed variance is used
                if (thismodel@useFixed$sigma2)
                {##do nothing
                  acceptHistory$sigma2[iterMcmc] <- TRUE
                } else {
                  ##Metropolis-Hastings update step here, using
                  ##an inverse gamma distribution
                  aStar <- thismodel@sigma2["a"] + length(x1)/2
                  ##Second paramter bStar depends on the value for sigma2
                  bStar <- function(x)
                  {adjW <-w1
                  ret <- sum((adjW - betaW[x1Level])^2)/2 + thismodel@sigma2["b"]
                  return(ret)
                  }
                  ###Draw proposal:
                  bStarProposal <- bStar(sigma2)
                  sigma2<- rinvGamma(n=1L,a=aStar,b=bStarProposal)


                }


                ##4)Save Samples

                if (saveSample(options, iterMcmc)){
                  samples$ExpEff[iterSave,]<-betaW
                  samples$sigma2[iterSave,1]<-sigma2
                  samples$sigma2betaW[iterSave,1] <-sigma2betaW
                  iterSave <- iterSave+1L
                }
              }


              ret <- Samples(data=samples,
                             options=options)
              return(ret)
            })
# nolint end
