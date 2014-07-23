#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Model-class.R] by DSB Sam 19/07/2014 03:59>
##
## Description:
## Encapsulate the model input in a formal class.
##
## History:
## 31/01/2014   file creation
###################################################################################

##' @include helpers.R
{}

## ============================================================

##' Class for the model input
##'
##' This is the general model class, from which all other specific models
##' inherit.
##'
##' The \code{datamodel} must obey the convention that the data input is
##' called exactly as in the \code{\linkS4class{Data}} class.
##' All prior distributions for parameters should be contained in the
##' model function \code{priormodel}. The background is that this can
##' be used to simulate from the prior distribution, before obtaining any
##' data.
##'
##' The \code{dose} function has as first argument \code{prob}, a scalar
##' toxicity probability which is targeted. Additional arguments are model
##' parameters. Then it computes, using model parameter(s) (samples), the
##' resulting dose. Note that the model parameters are called exactly as in the
##' \code{model} and must be included in the \code{sample} vector. The vectors
##' of all samples for these parameters will then be supplied to the function.
##' So your function must be able to process vectors of the model parameters,
##' i.e. it must vectorize over them.
##'
##' The \code{prob} function has as first argument \code{dose}, which is a
##' scalar dose. Additional arguments are model parameters. Then it computes,
##' using model parameter(s) (samples), the resulting probability of toxicity at
##' that dose. Again here, the function must vectorize over the model
##' parameters.
##'
##' If you work with multivariate parameters, then please assume that your
##' the two functions receive either one parameter value as a row vector,
##' or a samples matrix where the rows correspond to the sampling index, i.e.
##' the layout is then nSamples x dimParameter.
##'
##' Note that \code{dose} and \code{prob} are the inverse functions of each
##' other.
##'
##' @slot datamodel a function representing the BUGS data model specification
##' (see the details above)
##' @slot priormodel a function representing the BUGS prior specification
##' (see the details above)
##' @slot datanames The names of all \code{\linkS4class{Data}} slots that are
##' used in the \code{datamodel} and/or \code{priormodel} definition. Note that
##' you cannot specify more variables than those that are really used in the
##' model!
##' @slot modelspecs a function computing the list of the data model and prior
##' model specifications that are required for fully specifying them (e.g. prior
##' parameters, reference dose, etc.), based on the \code{\linkS4class{Data}}
##' slots that are then required as arguments of this function. This will then
##' be passed to BUGS for the computations.
##' @slot dose a function computing the dose reaching a specific target
##' probability, based on the model parameters and additional prior settings
##' (see the details above)
##' @slot prob a function computing the probability of toxicity for a specific
##' dose, based on the model parameters and additional prior settings (see the
##' details above)
##' @slot init a function computing the list of starting values for parameters
##' required to be initialized in the MCMC sampler, based on the
##' \code{\linkS4class{Data}} slots that are then required as arguments of this
##' function
##' @slot sample names of all parameters from which you would like to save the
##' MCMC samples. These must include the ones required by the \code{dose} and
##' \code{prob} functions.
##'
##' @seealso \code{\linkS4class{LogisticNormal}},
##' \code{\linkS4class{LogisticLogNormal}},
##' \code{\linkS4class{LogisticKadane}},
##' \code{\linkS4class{DualEndpoint}}
##'
##' @export
##' @keywords classes
setClass(Class="Model",
         representation=
         representation(datamodel="function",
                        priormodel="function",
                        datanames="character",
                        modelspecs="function",
                        dose="function",
                        prob="function",
                        init="function",
                        sample="character"),
         validity=
         function(object){
             ## convenience function
             noOverlap <- function(a, b)
             {
                 identical(intersect(a, b),
                           character(0))
             }

             ## names of the Data class slots
             allDatanames <- c("x", "y", "w",
                               "doseGrid", "nObs", "nGrid", "xLevel")

             stopifnot(## check that arguments of the init function
                       ## are only data names
                       all(names(formals(object@init)) %in%
                           allDatanames),
                       ## check that only possible slots are in datanames
                       all(object@datanames %in% allDatanames),
                       ## check that arguments of the dose and prob
                       ## functions are correct
                       all(names(formals(object@dose)) %in%
                           c("prob", object@sample)),
                       all(names(formals(object@prob)) %in%
                           c("dose", object@sample)))
         })


## ============================================================


##' Standard logistic model with bivariate (log) normal prior
##'
##' This is the usual logistic regression model with a bivariate normal prior on
##' the intercept and log slope.
##'
##' The covariate is the natural logarithm of the dose \eqn{x} divided by
##' the reference dose \eqn{x^{*}}:
##'
##' \deqn{logit[p(x)] = \alpha + \beta \cdot \log(x/x^{*})}
##' where \eqn{p(x)} is the probability of observing a DLT for a given dose
##' \eqn{x}.
##'
##' The prior is
##' \deqn{(\alpha, \log(\beta)) \sim Normal(\mu, \Sigma)}
##'
##' The slots of this class contain the mean vector and the covariance matrix of
##' the bivariate normal distribution, as well as the reference dose.
##'
##' @slot mean the prior mean vector \eqn{\mu}
##' @slot cov the prior covariance matrix \eqn{\Sigma}
##' @slot refDose the reference dose \eqn{x^{*}}
##'
##' @export
##' @keywords classes
setClass(Class="LogisticLogNormal",
         contains="Model",
         representation=
         representation(mean="numeric",
                        cov="matrix",
                        refDose="numeric"),
         validity=
         function(object){
             stopifnot(length(object@mean) == 2,
                       identical(dim(object@cov), c(2L, 2L)),
                       ! is.null(chol(object@cov)),
                       is.scalar(object@refDose))
         })


##' Initialization method for the "LogisticLogNormal" class
##'
##' @param .Object the \code{\linkS4class{LogisticLogNormal}} we want to
##' initialize
##' @param mean the prior mean vector
##' @param cov the prior covariance matrix
##' @param refDose the reference dose
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "LogisticLogNormal"),
          function (.Object,
                    mean,
                    cov,
                    refDose,
                    ...){
              ## go to the general initialize method now
              callNextMethod(.Object,
                             mean=mean,
                             cov=cov,
                             refDose=refDose,
                             datamodel=
                             function(){
                                 ## the logistic likelihood
                                 for (i in 1:nObs)
                                 {
                                     y[i] ~ dbern(p[i])
                                     logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
                                     StandLogDose[i] <- log(x[i] / refDose)
                                 }
                             },
                             priormodel=
                             function(){
                                 ## the multivariate normal prior on the (transformed)
                                 ## coefficients
                                 priorPrec[1:2,1:2] <- inverse(priorCov[,])
                                 theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2,1:2])
                                 ## extract actual coefficients
                                 alpha0 <- theta[1]
                                 alpha1 <- exp(theta[2])

                                 ## dummy to use refDose here.
                                 ## It is contained in the modelspecs list below,
                                 ## so it must occur here
                                 bla <- refDose + 1
                             },
                             datanames=c("nObs", "y", "x"),
                             modelspecs=
                             function(){
                                 list(refDose=refDose,
                                      priorCov=cov,
                                      priorMean=mean)
                             },
                             dose=
                             function(prob, alpha0, alpha1){
                                 StandLogDose <- (logit(prob) - alpha0) / alpha1
                                 return(exp(StandLogDose) * refDose)
                             },
                             prob=
                             function(dose, alpha0, alpha1){
                                 StandLogDose <- log(dose / refDose)
                                 return(plogis(alpha0 + alpha1 * StandLogDose))
                             },
                             init=
                             ## todo: find better starting values
                             function(){
                                 list(theta=c(0, 1))
                             },
                             sample=
                             c("alpha0", "alpha1"),
                             ...)
          })

## ============================================================


##' Standard logistic model with bivariate normal prior
##'
##' This is the usual logistic regression model with a bivariate normal prior on
##' the intercept and slope.
##'
##' The covariate is the natural logarithm of the dose \eqn{x} divided by
##' the reference dose \eqn{x^{*}}:
##'
##' \deqn{logit[p(x)] = \alpha + \beta \cdot \log(x/x^{*})}
##' where \eqn{p(x)} is the probability of observing a DLT for a given dose
##' \eqn{x}.
##'
##' The prior is
##' \deqn{(\alpha, \beta) \sim Normal(\mu, \Sigma)}
##'
##' The slots of this class contain the mean vector, the covariance and
##' precision matrices of the bivariate normal distribution, as well as the
##' reference dose.
##'
##' @slot mean the prior mean vector \eqn{\mu}
##' @slot cov the prior covariance matrix \eqn{\Sigma}
##' @slot prec the prior precision matrix \eqn{\Sigma^{-1}}
##' @slot refDose the reference dose \eqn{x^{*}}
##'
##' @export
##' @keywords classes
setClass(Class="LogisticNormal",
         contains="Model",
         representation=
         representation(mean="numeric",
                        cov="matrix",
                        prec="matrix",
                        refDose="numeric"),
         validity=
         function(object){
             stopifnot(length(object@mean) == 2,
                       identical(dim(object@cov), c(2L, 2L)),
                       ! is.null(chol(object@cov)),
                       is.scalar(object@refDose))
         })


##' Initialization method for the "LogisticNormal" class
##'
##' @param .Object the \code{\linkS4class{LogisticNormal}} we want to
##' initialize
##' @param mean the prior mean vector
##' @param cov the prior covariance matrix
##' @param refDose the reference dose
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "LogisticNormal"),
          function (.Object,
                    mean,
                    cov,
                    refDose,
                    ...){
              ## go to the general initialize method now
              callNextMethod(.Object,
                             mean=mean,
                             cov=cov,
                             prec=solve(cov),
                             refDose=refDose,
                             datamodel=
                             function(){
                                 ## the logistic likelihood
                                 for (i in 1:nObs)
                                 {
                                     y[i] ~ dbern(p[i])
                                     logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
                                     StandLogDose[i] <- log(x[i] / refDose)
                                 }
                             },
                             priormodel=
                             function(){
                                 ## the multivariate normal prior on the coefficients
                                 theta[1:2] ~ dmnorm(priorMean[1:2], priorPrec[1:2,1:2])
                                 ## extract actual coefficients
                                 alpha0 <- theta[1]
                                 alpha1 <- theta[2]

                                 ## dummy to use refDose here.
                                 ## It is contained in the modelspecs list below,
                                 ## so it must occur here
                                 bla <- refDose + 1
                             },
                             datanames=c("nObs", "y", "x"),
                             modelspecs=
                             function(){
                                 list(refDose=refDose,
                                      priorPrec=prec,
                                      priorMean=mean)
                             },
                             dose=
                             function(prob, alpha0, alpha1){
                                 StandLogDose <- (logit(prob) - alpha0) / alpha1
                                 return(exp(StandLogDose) * refDose)
                             },
                             prob=
                             function(dose, alpha0, alpha1){
                                 StandLogDose <- log(dose / refDose)
                                 return(plogis(alpha0 + alpha1 * StandLogDose))
                             },
                             init=
                             ## todo: find better starting values
                             function(){
                                 list(theta=c(0, 1))
                             },
                             sample=
                             c("alpha0", "alpha1"),
                             ...)
          })


## ============================================================

##' Reparametrized logistic model
##'
##' This is the logistic model in the parametrization of Kadane et al. (1980).
##'
##' Let \eqn{\rho_{0} = p(x_{min})} be the probability of a DLT and the minimum
##' dose \eqn{x_{min}}, and let \eqn{\gamma} be the dose with target toxicity
##' probability \eqn{\theta}, i.e. \eqn{p(\gamma) = \theta}. Then it can easily
##' be shown that the logistic regression model has intercept
##' \deqn{\frac{\gamma logit(\rho_{0}) - x_{min} logit(\theta)}{\gamma -
##' x_{min}}}{(\gamma logit(\rho_{0}) - x_{min} logit(\theta)) / (\gamma -
##' x_{min})}
##' and slope
##' \deqn{\frac{logit(theta) - logit(\rho_{0})}{\gamma - x_{min}}}{(logit(theta)
##' - logit(\rho_{0})) / (\gamma - x_{min})}
##'
##' The prior is a uniform distribution for \eqn{\gamma} between \eqn{x_{min}}
##' and \eqn{x_{max}}, and for \eqn{\rho_{0}} as well a uniform distribution
##' between \eqn{0} and \eqn{\theta}.
##'
##' The slots of this class, required for creating the model, are the target
##' toxicity, as well as the minimum and maximum of the dose range. Note that
##' these can be different from the minimum and maximum of the dose grid in the
##' data later on.
##'
##' @slot theta the target toxicity probability \eqn{\theta}
##' @slot xmin the minimum of the dose range \eqn{x_{min}}
##' @slot xmax the maximum of the dose range \eqn{x_{max}}
##'
##' @export
##' @keywords classes
setClass(Class="LogisticKadane",
         contains="Model",
         representation=
         representation(theta="numeric",
                        xmin="numeric",
                        xmax="numeric"),
         validity=
         function(object){
             stopifnot(is.probability(object@theta,
                                      bounds=FALSE),
                       object@xmin < object@xmax,
                       is.scalar(object@xmin),
                       is.scalar(object@xmax))
         })


##' Initialization method for the "LogisticKadane" class
##'
##' @param .Object the \code{\linkS4class{LogisticKadane}} we want to
##' initialize
##' @param theta the target toxicity probability
##' @param xmin the minimum of the dose range
##' @param xmax the maximum of the dose range
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "LogisticKadane"),
          function(.Object,
                   theta,
                   xmin,
                   xmax,
                   ...){
              ## go to the general initialize method now
              callNextMethod(.Object,
                             theta=theta,
                             xmin=xmin,
                             xmax=xmax,
                             datamodel=
                             function(){
                                 ## the logistic likelihood
                                 for (i in 1:nObs)
                                 {
                                     y[i] ~ dbern(p[i])
                                     logit(p[i]) <- (1/(gamma - xmin)) *
                                         (gamma*logit(rho0) - xmin*logit(theta)
                                          + (logit(theta) - logit(rho0)) * x[i])
                                 }
                             },
                             priormodel=
                             function(){
                                 ## priors
                                 gamma ~ dunif(xmin, xmax)
                                 rho0 ~ dunif(0, theta)
                             },
                             datanames=c("nObs", "y", "x"),
                             modelspecs=
                             function(){
                                 list(theta=theta,
                                      xmin=xmin,
                                      xmax=xmax)
                             },
                             dose=
                             function(prob, rho0, gamma){
                                 ret <- gamma * (logit(prob) - logit(rho0)) +
                                     xmin * (logit(theta) - logit(prob))
                                 ret <- ret / (logit(theta) - logit(rho0))
                                 return(ret)
                             },
                             prob=
                             function(dose, rho0, gamma){
                                 ret <- (gamma*logit(rho0) - xmin*logit(theta)
                                         + (logit(theta) - logit(rho0)) * dose)
                                 ret <- plogis(ret / (gamma - xmin))
                                 return(ret)
                             },
                             init=
                             function(){
                                 list(rho0 = theta / 10,
                                      gamma = (xmax - xmin) / 2)},
                             sample=
                             c("rho0", "gamma"),
                             ...)
          })


## ============================================================


##' Dual endpoint model
##'
##' todo: describe the model
##'
##' @slot mu For the probit toxicity model, \code{mu} contains the prior mean
##' vector
##' @slot Sigma For the probit toxicity model, contains the prior covariance
##' matrix
##' @slot sigma2betaW For the biomarker model, contains the prior variance
##' factor of the random walk prior. If it is not a single number, it can also
##' contain a vector with elements \code{a} and {b} for the inverse-gamma prior
##' on \code{sigma2betaW}.
##' @slot sigma2W Either a fixed value for the biomarker variance, or a vector
##' with elements \code{a} and \code{b} for the inverse-gamma prior parameters.
##' @slot rho Either a fixed value for the correlation (between -1 and 1), or a
##' vector with elements \code{a} and \code{b} for the Beta prior on the
##' transformation kappa = (rho + 1) / 2, which is in (0, 1). For example,
##' \code{a=1,b=1} leads to a uniform prior on rho.
##' @slot useRW1 for specifying the random walk prior on the biomarker level: if
##' \code{TRUE}, RW1 is used, otherwise RW2.
##' @slot useFixed a list with logical value for each of the three parameters
##' \code{sigma2betaW}, \code{sigma2W} and \code{rho} indicating whether
##' a fixed value is used or not.
##'
##' @export
##' @keywords classes
setClass(Class="DualEndpoint",
         contains="Model",
         representation=
         representation(mu="numeric",
                        Sigma="matrix",
                        sigma2betaW="numeric",
                        sigma2W="numeric",
                        rho="numeric",
                        useRW1="logical",
                        useFixed="list"),
         validity=
         function(object){

             ## check the prior parameters with variable content
             for(parName in c("sigma2betaW", "sigma2W", "rho"))
             {
                 ## if we use a fixed value for this parameter
                 if(object@useFixed[[parName]])
                 {
                     ## check range of value
                     if(parName == "rho")
                     {
                         stopifnot((object@rho > -1) && (object@rho < 1))
                     } else {
                         stopifnot(slot(object, parName) > 0)
                     }
                 } else {
                     ## use a IG(a, b) or Beta(a, b)  prior
                     stopifnot(all(slot(object, parName) > 0),
                               identical(names(slot(object, parName)),
                                         c("a", "b")))
                 }
             }

             ## check the other prior parameters
             stopifnot(identical(length(object@mu), 2L),
                       identical(dim(object@Sigma), c(2L, 2L)),
                       is.scalar(object@useRW1))
         })


##' Initialization method for the "DualEndpoint" class
##'
##' @param .Object the \code{\linkS4class{DualEndpoint}} we want to
##' initialize
##' @param mu see \code{\linkS4class{DualEndpoint}}
##' @param Sigma see \code{\linkS4class{DualEndpoint}}
##' @param sigma2betaW see \code{\linkS4class{DualEndpoint}}
##' @param sigma2W see \code{\linkS4class{DualEndpoint}}
##' @param rho see \code{\linkS4class{DualEndpoint}}
##' @param smooth either \dQuote{RW1} (default) or \dQuote{RW2}, for
##' specifying the random walk prior on the biomarker level.
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "DualEndpoint"),
          function(.Object,
                   mu,
                   Sigma,
                   sigma2betaW,
                   sigma2W,
                   rho,
                   smooth=c("RW1", "RW2"),
                   ...){

              ## Find out RW choice
              smooth <- match.arg(smooth)
              .Object@useRW1 <- smooth == "RW1"

              ## Find out which parameters are fixed
              useFixed <- list()
              for(parName in c("sigma2betaW", "sigma2W", "rho"))
              {
                  useFixed[[parName]] <-
                      identical(length(get(parName)), 1L)
              }
              .Object@useFixed <- useFixed

              ## build together the prior model and the parameters
              ## to be saved during sampling
              ## ----------

              ## start with this:

              modelspecs <-
                  list(mu=mu,
                       PrecBetaZ=solve(Sigma),
                       low=c(-10000, 0),
                       high=c(0, 10000))

              priormodel <-
                  function(){
                      ## priors
                      betaW[1:nGrid] ~ car.normal(adj[], weights[],
                                                  num[], precBetaW)
                      ## note that "adj", "weights" and "num" come
                      ## from the data object! Because it depends on
                      ## the dose grid
                      betaWintercept ~ dflat()

                      ## the bivariate normal prior for the
                      ## probit coefficients
                      betaZ[1:2] ~ dmnorm(mu[], PrecBetaZ[,])

                      ## conditional precision for biomarker
                      condPrecW <- precW / (1 - pow(rho, 2))
                  }

              ## we will fill in more, depending on which parameters
              ## are fixed, in these two variables:
              sample <- c("betaZ", "betaWintercept", "betaW")
              initlist <- list()

              ## first the biomarker regression variance
              if(! useFixed[["sigma2W"]])
              {
                  priormodel <-
                      joinModels(priormodel,
                                 function(){
                                     ## gamma prior for biomarker precision
                                     precW ~ dgamma(precWa, precWb)
                                 })

                  sample <- c(sample,
                              "precW")

                  modelspecs <- c(modelspecs,
                                  list(precWa=sigma2W["a"],
                                       precWb=sigma2W["b"]))

                  initlist$precW <- 1

              } else {
                  modelspecs <- c(modelspecs,
                                  list(precW=1/sigma2W))
              }

              ## second the variance for the RW prior
              if(! useFixed[["sigma2betaW"]])
              {
                  priormodel <-
                      joinModels(priormodel,
                                 function(){
                                     ## gamma prior for RW precision
                                     precBetaW ~ dgamma(precBetaWa, precBetaWb)
                                 })

                  sample <- c(sample,
                              "precBetaW")

                  initlist$precBetaW <- 1

                  modelspecs <- c(modelspecs,
                                  list(precBetaWa=sigma2betaW["a"],
                                       precBetaWb=sigma2betaW["b"]))
              } else {
                  modelspecs <- c(modelspecs,
                                  list(precBetaW=1/sigma2betaW))
              }

              ## third the correlation
              if(! useFixed[["rho"]])
              {
                  priormodel <-
                      joinModels(priormodel,
                                 function(){
                                     ## transformed Beta prior for rho
                                     kappa ~ dbeta(rhoa, rhob)
                                     rho <- kappa * 2 - 1
                                 })

                  sample <- c(sample,
                              "rho")

                  initlist$kappa <- 1/2

                  modelspecs <- c(modelspecs,
                                  list(rhoa=rho["a"],
                                       rhob=rho["b"]))
              } else {
                  modelspecs <- c(modelspecs,
                                  list(rho=rho))
              }

              ## now build the *function* modelspecs, which computes
              ## from data slots the required RW matrices etc.
              modelspecsFun <- function(x, xLevel, nGrid)
              {
                  ## design matrices for tox and biomarker models:

                  ## tox
                  designZ <- cbind(1, x)
                  ## todo: note that this is the easiest case here.
                  ## we could in principle employ any probit regression model
                  ## for the toxicity! So later on, we can extend this
                  ## to make it more flexible.

                  ## biomarker
                  designW <-
                      model.matrix(~ - 1 +
                                   I(factor(xLevel,
                                            levels=seq_len(nGrid))))
                  dimnames(designW) <- list(NULL, NULL)

                  ## difference matrix of order 1:
                  D1mat <- cbind(0,
                                 diag(nGrid - 1)) -
                                     cbind(diag(nGrid - 1), 0)

                  ## we will compute the RW prior data in the following
                  weights <- adj <- num <- numeric()
                  ## Note that these vectors are sized during the computations
                  ## now.

                  if(.Object@useRW1)
                  {
                      ## RW1
                      ## ----------

                      ## the rank-deficient prior precision for the RW1 prior:
                      RWmat <- crossprod(D1mat)
                      ## Note that this needs to be divided by sigma2betaW to
                      ## obtain final prior precision

                      ## the rank of this matrix
                      RWmatRank <- nGrid - 1L

                      ## compute vectors
                      for(t in 1:1)
                      {
                          weights[t] <- 1;
                          adj[t] <- t+1;
                          num[t] <- 1
                      }
                      for(t in 2:(nGrid-1))
                      {
                          weights[2+(t-2)*2] <- 1;
                          adj[2+(t-2)*2] <- t-1
                          weights[3+(t-2)*2] <- 1;
                          adj[3+(t-2)*2] <- t+1;
                          num[t] <- 2
                      }
                      for(t in nGrid:nGrid)
                      {
                          weights[(nGrid-2)*2 + 2] <- 1;
                          adj[(nGrid-2)*2 + 2] <- t-1;
                          num[t] <- 1
                      }

                  } else {

                      ## RW2
                      ## ----------

                      ## for second-order differences:
                      D2mat <- D1mat[-1, -1] %*% D1mat

                      ## same for RW2
                      RWmat <- crossprod(D2mat)
                      RWmatRank <- nGrid - 2L

                      ## compute vectors
                      for(t in 1:1) {
                          weights[t] <- 2; adj[t] <- t+1
                          weights[t+1] <- -1; adj[t+1] <- t+2; num[t] <- 2
                      }
                      for(t in 2:2) {
                          weights[t+1] <- 2; adj[t+1] <- t-1
                          weights[t+2] <- 4; adj[t+2] <- t+1
                          weights[t+3] <- -1; adj[t+3] <- t+2; num[t] <- 3
                      }
                      for(t in 3:(nGrid-2)) {
                          weights[6+(t-3)*4] <- -1; adj[6+(t-3)*4] <- t-2
                          weights[7+(t-3)*4] <- 4; adj[7+(t-3)*4] <- t-1
                          weights[8+(t-3)*4] <- 4; adj[8+(t-3)*4] <- t+1
                          weights[9+(t-3)*4] <- -1; adj[9+(t-3)*4] <- t+2;
                          num[t] <- 4
                      }
                      for(t in (nGrid-1):(nGrid-1)) {
                          weights[(nGrid-4)*4 + 6] <- 2;
                          adj[(nGrid-4)*4 + 6] <- t+1
                          weights[(nGrid-4)*4 + 7] <- 4;
                          adj[(nGrid-4)*4 + 7] <- t-1
                          weights[(nGrid-4)*4 + 8] <- -1;
                          adj[(nGrid-4)*4 + 8] <- t-2;
                          num[t] <- 3
                      }
                      for(t in nGrid:nGrid) {
                          weights[(nGrid-4)*4 + 9] <- 2;
                          adj[(nGrid-4)*4 + 9] <- t-1
                          weights[(nGrid-4)*4 + 10] <- -1;
                          adj[(nGrid-4)*4 + 10] <- t-2;
                          num[t] <- 2
                      }
                  }

                  ## finally return the list
                  return(c(modelspecs,
                           list(## designZ=designZ,
                                ## designW=designW,
                                ## RWmat=RWmat,
                                ## RWmatRank=RWmatRank,
                                weights=weights,
                                adj=adj,
                                num=num)))
              }


              ## go to the general initialize method now
              callNextMethod(.Object,
                             mu=mu,
                             Sigma=Sigma,
                             sigma2betaW=sigma2betaW,
                             sigma2W=sigma2W,
                             rho=rho,
                             datamodel=
                             function(){
                                 ## the likelihood
                                 for (i in 1:nObs)
                                 {
                                     ## the toxicity model
                                     z[i] ~ dnorm(meanZ[i], 1) %_%
                                         I(low[y[i] + 1], high[y[i] + 1])

                                     ## the conditional biomarker model
                                     w[i] ~ dnorm(condMeanW[i], condPrecW)

                                     ## the moments
                                     meanZ[i] <- betaZ[1] + betaZ[2] * x[i]
                                     condMeanW[i] <- betaWintercept + betaW[xLevel[i]] +
                                         rho / sqrt(precW) * (z[i] - meanZ[i])
                                     ## Note that betaW has a sum to zero constraint here.
                                     ## Therefore we have to add an intercept with a flat prior
                                     ## on top.
                                 }
                             },
                             priormodel=priormodel,
                             datanames=
                             c("nObs", "w", "x", "xLevel", "y", "nGrid"),
                             modelspecs=modelspecsFun,
                             dose=
                             function(prob, betaZ){
                                 ret <- (qnorm(prob) - betaZ[, 1]) / betaZ[, 2]
                                 return(ret)
                             },
                             prob=
                             function(dose, betaZ){
                                 ret <- pnorm(betaZ[, 1] + betaZ[, 2] * dose)
                                 return(ret)
                             },
                             init=
                             function(y, w, nGrid){
                                 c(initlist,
                                   list(z=
                                        ifelse(y==0, -1, 1),
                                        betaZ=c(0,1),
                                        betaWintercept=mean(w),
                                        betaW=
                                        rep(0, nGrid)))},
                             sample=sample,
                             ...)
          })

## ============================================================


##' Standard logistic model with mixture of two bivariate normal priors
##'
##' This is stanardard logistic regression model with a mixture of two bivariate
##' normal priors on the intercept and slope parameters.
##' This type of prior is often used with a mixture of a minimal informative
##' and an informative component, in order to make the CRM more robust to
##' data deviations from the informative component.
##'
##' The covariate is the natural logarithm of the dose \eqn{x} divided by
##' the reference dose \eqn{x^{*}}:
##'
##' \deqn{logit[p(x)] = \alpha + \beta \cdot \log(x/x^{*})}
##' where \eqn{p(x)} is the probability of observing a DLT for a given dose
##' \eqn{x}.
##'
##' The prior is
##' \deqn{(\alpha, \beta) \sim
##' w * Normal(\mu_{1}, \Sigma_{1}) + (1 - w) * Normal(\mu_{2}, \Sigma_{2})}
##'
##' The weight w for the first component is assigned a beta prior B(a, b).
##'
##' The slots of this class comprise two lists, containing the mean vector, the
##' covariance and precision matrices of the two bivariate normal distributions
##' each, the parameters of the beta prior for the first component weight, as
##' well as the reference dose.
##'
##' @slot comp1 the specifications of the first component: a list with
##' \code{mean}, \code{cov} and \code{prec} for the first bivariate normal prior
##' @slot comp2 the specifications of the second component
##' @slot weightpar the beta parameters for the weight of the first component
##' @slot refDose the reference dose \eqn{x^{*}}
##'
##' @export
##' @keywords classes
setClass(Class="LogisticNormalMixture",
         contains="Model",
         representation=
         representation(comp1="list",
                        comp2="list",
                        weightpar="numeric",
                        refDose="numeric"),
         validity=
         function(object){
             stopifnot(identical(names(object@comp1),
                                 c("mean", "cov", "prec")),
                       identical(names(object@comp2),
                                 c("mean", "cov", "prec")),
                       identical(names(object@weightpar),
                                 c("a", "b")),
                       is.scalar(object@refDose))
         })


##' Initialization method for the "LogisticNormalMixture" class
##'
##' @param .Object the \code{\linkS4class{LogisticNormalMixture}} we want to
##' initialize
##' @param comp1 the specifications of the first component: a list with
##' \code{mean} and \code{cov} for the first bivariate normal prior
##' @param comp2 the specifications of the second component
##' @param weightpar the beta parameters for the weight of the first component
##' @param refDose the reference dose
##'
##' @export
##' @keywords methods
setMethod("initialize",
          signature(.Object = "LogisticNormalMixture"),
          function (.Object,
                    comp1,
                    comp2,
                    weightpar,
                    refDose,
                    ...){
              ## add precision matrices to component lists
              comp1 <- c(comp1,
                         list(prec=solve(comp1$cov)))
              comp2 <- c(comp2,
                         list(prec=solve(comp2$cov)))

              ## go to the general initialize method now
              callNextMethod(.Object,
                             comp1=comp1,
                             comp2=comp2,
                             weightpar=weightpar,
                             refDose=refDose,
                             datamodel=
                             function(){
                                 ## the logistic likelihood:
                                 ## not changed from non-mixture case
                                 for (i in 1:nObs)
                                 {
                                     y[i] ~ dbern(p[i])
                                     logit(p[i]) <- alpha0 + alpha1 * StandLogDose[i]
                                     StandLogDose[i] <- log(x[i] / refDose)
                                 }
                             },
                             priormodel=
                             function(){
                                 ## the multivariate normal prior on the coefficients
                                 theta[1:2] ~ dmnorm(priorMean[1:2, comp],
                                                     priorPrec[1:2, 1:2, comp])
                                 ## this is conditional on the component index
                                 ## "comp"

                                 ## component index is 1 or 2
                                 comp <- comp0 + 1

                                 ## it is 1 with probability w and
                                 ## 2 with probability 1 - w
                                 comp0 ~ dbern(wc)
                                 wc <- 1 - w

                                 ## we have a beta prior on w
                                 w ~ dbeta(weightpar[1], weightpar[2])

                                 ## extract actual coefficients
                                 alpha0 <- theta[1]
                                 alpha1 <- theta[2]

                                 ## dummy to use refDose here.
                                 ## It is contained in the modelspecs list below,
                                 ## so it must occur here
                                 bla <- refDose + 1
                             },
                             datanames=c("nObs", "y", "x"),
                             modelspecs=
                             function(){
                                 list(refDose=refDose,
                                      priorMean=
                                      cbind(comp1$mean,
                                            comp2$mean),
                                      priorPrec=
                                      array(data=
                                            c(comp1$prec,
                                              comp2$prec),
                                            dim=c(2, 2, 2)),
                                      weightpar=weightpar)
                             },
                             dose=
                             function(prob, alpha0, alpha1){
                                 StandLogDose <- (logit(prob) - alpha0) / alpha1
                                 return(exp(StandLogDose) * refDose)
                             },
                             prob=
                             function(dose, alpha0, alpha1){
                                 StandLogDose <- log(dose / refDose)
                                 return(plogis(alpha0 + alpha1 * StandLogDose))
                             },
                             init=
                             ## todo: find better starting values
                             function(){
                                 list(theta=c(0, 1))
                             },
                             sample=
                             c("alpha0", "alpha1", "w"),
                             ...)
          })


## ============================================================
