#' @include helpers.R
#' @include Samples-class.R
NULL

# mcmc ----

#' Obtaining Posterior Samples for all Model Parameters
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is the function that actually runs the `JAGS` MCMC machinery to produce
#' posterior samples from all model parameters and required derived values.
#' It is a generic function, so that customized versions may be conveniently
#' defined for specific subclasses of [`GeneralData`], [`GeneralModel`], and
#' [`McmcOptions`] input.
#'
#' @note The type of Random Number Generator (RNG) and its initial seed used by
#'   `JAGS` are taken from the `options` argument. If no initial values are
#'   supplied (i.e RNG kind or seed slot in `options` has `NA`), then they will
#'   be generated automatically by `JAGS`.
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
    standardGeneric("mcmc")
  },
  valueClass = "Samples"
)

# mcmc-GeneralData ----

#' @describeIn mcmc Standard method which uses JAGS.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData
#' @example examples/mcmc.R
#'
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
                 from_prior = data@nObs == 0L,
                 ...) {
    assert_flag(from_prior)

    model_fun <- if (from_prior) {
      model@priormodel
    } else {
      h_jags_join_models(model@datamodel, model@priormodel)
    }
    model_file <- h_jags_write_model(model_fun)
    model_inits <- h_jags_get_model_inits(model, data)
    model_data <- h_jags_get_data(model, data, from_prior)

    jags_model <- rjags::jags.model(
      file = model_file,
      data = model_data,
      inits = c(
        model_inits,
        .RNG.name = h_null_if_na(options@rng_kind),
        .RNG.seed = h_null_if_na(options@rng_seed)
      ),
      quiet = !is_logging_enabled(),
      n.adapt = 0 # No adaptation. Important for reproducibility.
    )
    update(jags_model, n.iter = options@burnin, progress.bar = "none")

    # This is necessary as some outputs are written directly from the JAGS
    # compiled code to the outstream.
    log_trace("Running rjags::jags.samples")
    if (is_logging_enabled()) {
      jags_samples <- rjags::jags.samples(
        model = jags_model,
        variable.names = model@sample,
        n.iter = (options@iterations - options@burnin),
        thin = options@step
      )
    } else {
      invisible(
        capture.output(
          jags_samples <- rjags::jags.samples(
            model = jags_model,
            variable.names = model@sample,
            n.iter = (options@iterations - options@burnin),
            thin = options@step,
            progress.bar = "none"
          )
        )
      )
    }
    log_trace("JAGS samples: ", jags_samples, capture = TRUE)
    samples <- lapply(jags_samples, h_jags_extract_samples)

    Samples(data = samples, options = options)
  }
)

# mcmc-GeneralData-DualEndpointRW ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointRW`] model, it is required that there are at least two (in
#'   case of random walk prior of the first order on the biomarker level) or
#'   three doses in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointRW
#' @example examples/mcmc-DualEndpointRW.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointRW",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 from_prior = data@nObs == 0L,
                 ...) {
    if (model@rw1) {
      assert_true(data@nGrid >= 2)
    } else {
      assert_true(data@nGrid >= 3)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-DualEndpointBeta ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointBeta`] model, it is required that the value of `ref_dose_beta`
#'   slot is greater than the maximum dose in a grid. This requirement comes from
#'   definition of the beta function that is used to model dose-biomarker
#'   relationship in [`DualEndpointBeta`] model. The other requirement is that
#'   there must be at least one dose in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointBeta
#' @example examples/mcmc-DualEndpointBeta.R
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointBeta",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 from_prior = data@nObs == 0L,
                 ...) {
    assert_true(data@nGrid >= 1)
    assert_true(model@ref_dose_beta > data@doseGrid[data@nGrid])

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-DualEndpointEmax ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`DualEndpointEmax`] model, it is required that there is at least one dose
#'   in the grid.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-DualEndpointEmax
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "DualEndpointEmax",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 from_prior = data@nObs == 0L,
                 ...) {
    assert_true(data@nGrid >= 1)

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-OneParLogNormalPrior ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`OneParLogNormalPrior`] model, it is required that the length of
#'   skeleton prior probabilities vector should be equal to the length of the
#'   number of doses.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-OneParLogNormalPrior
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "OneParLogNormalPrior",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 from_prior = data@nObs == 0L,
                 ...) {
    if (!from_prior) {
      assert_true(length(model@skel_probs) == data@nGrid)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# mcmc-GeneralData-OneParExpPrior ----

#' @describeIn mcmc Standard method which uses JAGS. For the
#'   [`OneParExpPrior`] model, it is required that the length of
#'   skeleton prior probabilities vector should be equal to the length of the
#'   number of doses.
#'
#' @param from_prior (`flag`)\cr sample from the prior only? Default to `TRUE`
#'   when number of observations in `data` is `0`. For some models it might be
#'   necessary to specify it manually here though.
#'
#' @aliases mcmc-GeneralData-OneParExpPrior
#'
setMethod(
  f = "mcmc",
  signature = signature(
    data = "GeneralData",
    model = "OneParExpPrior",
    options = "McmcOptions"
  ),
  def = function(data,
                 model,
                 options,
                 from_prior = data@nObs == 0L,
                 ...) {
    if (!from_prior) {
      assert_true(length(model@skel_probs) == data@nGrid)
    }

    callNextMethod(
      data = data,
      model = model,
      options = options,
      from_prior = from_prior,
      ...
    )
  }
)

# nolint start

## --------------------------------------------------
## The method for DataMixture usage
## --------------------------------------------------

##' @describeIn mcmc Method for DataMixture with different from_prior default
setMethod("mcmc",
  signature =
    signature(
      data = "DataMixture",
      model = "GeneralModel",
      options = "McmcOptions"
    ),
  def =
    function(data, model, options,
             from_prior = data@nObs == 0L & data@nObsshare == 0L,
             ...) {
      callNextMethod(data, model, options, from_prior = from_prior, ...)
    }
)


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
                         options) {
  ## assertions
  p <- length(m0)
  nObs <- length(y)
  stopifnot(
    is.vector(y),
    all(y %in% c(0, 1)),
    is.matrix(P0),
    identical(dim(P0), c(p, p)),
    is.matrix(X),
    identical(dim(X), c(nObs, p)),
    is(options, "McmcOptions")
  )

  ## get or set the seed
  rSeed <- try(get(".Random.seed", envir = .GlobalEnv),
    silent = TRUE
  )
  if (is(rSeed, "try-error")) {
    set.seed(floor(runif(n = 1, min = 0, max = 1e4)))
    rSeed <- get(".Random.seed", envir = .GlobalEnv)
  }
  ## .Random.seed contains two leading integers where the second
  ## gives the position in the following 624 long vector (see
  ## ?set.seed). Take the current position and ensure positivity
  rSeed <- abs(rSeed[-c(1:2)][rSeed[2]])

  ## build the model according to whether we sample from prior
  ## or not:
  bugsModel <- function() {
    for (i in 1:nObs)
    {
      y[i] ~ dbern(p[i])
      logit(p[i]) <- mu[i]
    }

    mu <- X[, ] %*% beta

    ## the multivariate normal prior on the coefficients
    beta ~ dmnorm(priorMean[], priorPrec[, ])
  }

  ## write the model file into it
  modelFileName <- h_jags_write_model(bugsModel)

  jagsModel <- rjags::jags.model(modelFileName,
    data = list(
      "X" = X,
      "y" = y,
      "nObs" = nObs,
      priorMean = m0,
      priorPrec = P0
    ),
    quiet = TRUE,
    inits =
    ## add the RNG seed to the inits list:
    ## (use Mersenne Twister as per R
    ## default)
      list(
        .RNG.name = "base::Mersenne-Twister",
        .RNG.seed = rSeed
      ),
    n.chains = 1,
    n.adapt = 0
  )
  ## burn in
  update(jagsModel,
    n.iter = options@burnin,
    progress.bar = "none"
  )

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
  capture.output(eval(parse(text = samplesCode)))

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
  signature =
    signature(
      data = "Data",
      model = "LogisticIndepBeta",
      options = "McmcOptions"
    ),
  def =
    function(data, model, options,
             ...) {

      ## update the DLE model first
      thismodel <- update(object = model, data = data)

      ## decide whether we sample from the prior or not
      from_prior <- data@nObs == 0L


      ## probabilities of risk of DLE at all dose levels
      pi <- (thismodel@binDLE) / (thismodel@DLEweights)
      ## scalar term for the covariance matrix
      scalarI <- thismodel@DLEweights * pi * (1 - pi)
      ##
      precision <- matrix(rep(0, 4), nrow = 2, ncol = 2)

      for (i in (1:(length(thismodel@binDLE)))) {
        precisionmat <- scalarI[i] * matrix(c(1, log(thismodel@DLEdose[i]), log(thismodel@DLEdose[i]), (log(thismodel@DLEdose[i]))^2), 2, 2)
        precision <- precision + precisionmat
      }

      if (from_prior) {
        ## sample from the (asymptotic) bivariate normal prior for theta

        tmp <- mvtnorm::rmvnorm(
          n = size(options),
          mean = c(slot(thismodel, "phi1"), slot(thismodel, "phi2")),
          sigma = solve(precision)
        )


        samples <- list(
          phi1 = tmp[, 1],
          phi2 = tmp[, 2]
        )
      } else {
        weights <- rep(1, length(data@y))
        ## probabilities of risk of DLE at all dose levels
        pi <- (data@y) / weights
        ## scalar term for the covariance matrix
        scalarI <- weights * pi * (1 - pi)
        ##

        priordle <- thismodel@binDLE
        priorw1 <- thismodel@DLEweights

        priordose <- thismodel@DLEdose
        FitDLE <- suppressWarnings(glm(priordle / priorw1 ~ log(priordose), family = binomial(link = "logit"), weights = priorw1))
        SFitDLE <- summary(FitDLE)
        ## Obtain parameter estimates for dose-DLE curve
        priorphi1 <- coef(SFitDLE)[1, 1]
        priorphi2 <- coef(SFitDLE)[2, 1]

        ## use fast special sampler here
        ## set up design matrix
        X <- cbind(1, log(data@x))
        initRes <- myBayesLogit(
          y = data@y,
          X = X,
          m0 = c(priorphi1, priorphi2),
          P0 = precision,
          options = options
        )

        ## then form the samples list
        samples <- list(
          phi1 = initRes[, 1],
          phi2 = initRes[, 2]
        )
      }

      ## form a Samples object for return:
      ret <- Samples(
        data = samples,
        options = options
      )

      return(ret)
    }
)

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
setMethod(
  f = "mcmc",
  signature = signature(
    data = "DataDual",
    model = "Effloglog",
    options = "McmcOptions"
  ),
  definition = function(data, model, options, ...) {
    model <- update(object = model, data = data)
    sample_size <- size(options)

    if (model@use_fixed) {
      nu <- model@nu
      nu_samples <- rep(nu, sample_size)
    } else {
      nu_samples <- rgamma(sample_size, shape = model@nu["a"], rate = model@nu["b"])
      nu <- mean(nu_samples)
    }

    # Sample from the (asymptotic) bivariate normal prior for theta1 and theta2.
    tmp <- mvtnorm::rmvnorm(
      n = sample_size,
      mean = model@mu,
      sigma = solve(nu * model@Q)
    )

    samples <- list(
      theta1 = tmp[, 1],
      theta2 = tmp[, 2],
      nu = nu_samples
    )

    Samples(
      data = samples,
      options = options
    )
  }
)
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
  signature =
    signature(
      data = "DataDual",
      model = "EffFlexi",
      options = "McmcOptions"
    ),
  def =
    function(data, model, options,
             ...) {
      ## update the model
      thismodel <- update(object = model, data = data)

      nSamples <- size(options)

      ## Prepare samples container
      ### List parameter samples to save
      samples <- list(
        ExpEff = matrix(ncol = data@nGrid, nrow = nSamples),
        sigma2W = matrix(nrow = nSamples),
        sigma2betaW = matrix(nrow = nSamples)
      )
      ## Prepare starting values
      ## Index of the next sample to be saved:

      iterSave <- 1L
      ## Monitoring the Metropolis-Hastings update for sigma2

      acceptHistory <- list(sigma2W = logical(options@iterations))

      ## Current parameter values and also the starting values for the MCMC are set
      ## EstEff: constant, the average of the observed efficacy values

      if (length(data@w) == 0) {
        w1 <- thismodel@eff
        x1 <- thismodel@eff_dose
      } else {
        ## Combine pseudo data with observed efficacy responses and no DLT observed
        w1 <- c(thismodel@eff, getEff(data)$w_no_dlt)
        x1 <- c(thismodel@eff_dose, getEff(data)$x_no_dlt)
      }
      x1Level <- matchTolerance(x1, data@doseGrid)
      ## betaW is constant, the average of the efficacy values
      betaW <- rep(mean(w1), data@nGrid)
      ## sigma2betaW use fixed value or prior mean
      sigma2betaW <-
        if (thismodel@use_fixed[["sigma2betaW"]]) {
          thismodel@sigma2betaW
        } else {
          thismodel@sigma2betaW["b"] / (thismodel@sigma2betaW["a"] - 1)
        }
      ## sigma2: fixed value or just the empirical variance
      sigma2W <- if (thismodel@use_fixed[["sigma2W"]]) {
        thismodel@sigma2W
      } else {
        var(w1)
      }
      ## Set up diagonal matrix with the number of patients in the corresponding dose levels on the diagonal
      designWcrossprod <- crossprod(thismodel@X)

      ### The MCMC cycle

      for (iterMcmc in seq_len(options@iterations))
      { ## 1) Generate coefficients for the Flexible Efficacy model
        ## the variance
        adjustedVar <- sigma2W
        ## New precision matrix
        thisPrecW <- designWcrossprod / adjustedVar + thismodel@RW / sigma2betaW
        ## draw random normal vector
        normVec <- rnorm(data@nGrid)
        ## and its Cholesky factor
        thisPrecWchol <- chol(thisPrecW)
        ## solve betaW for L^T * betaW = normVec
        betaW <- backsolve(r = thisPrecWchol, x = normVec)
        ## the residual
        adjustedW <- w1 - thismodel@X %*% betaW

        ## forward substitution
        ## solve L^T * tmp = designW ^T * adjustedW/ adjustedVar

        tmp <- forwardsolve(
          l = thisPrecWchol,
          x = crossprod(thismodel@X, adjustedW) / adjustedVar,
          upper.tri = TRUE,
          transpose = TRUE
        )
        ## Backward substitution solve R*tepNew =tmp
        tmp <- backsolve(
          r = thisPrecWchol,
          x = tmp
        )

        ## tmp is the mean vector of the distribution
        ## add tmp to betaW to obtain final sample

        betaW <- betaW + tmp

        ## 2) Generate prior variance factor for the random walk
        ## if fixed, do nothing
        ## Otherwise sample from full condition

        if (!thismodel@use_fixed[["sigma2betaW"]]) {
          sigma2betaW <- rinvGamma(
            n = 1L,
            a = thismodel@sigma2betaW["a"] + thismodel@RW_rank / 2,
            b = thismodel@sigma2betaW["b"] + crossprod(betaW, thismodel@RW %*% betaW) / 2
          )
        }
        ## 3) Generate variance for the flexible efficacy model
        ## if fixed variance is used
        if (thismodel@use_fixed[["sigma2W"]]) { ## do nothing
          acceptHistory$sigma2W[iterMcmc] <- TRUE
        } else {
          ## Metropolis-Hastings update step here, using
          ## an inverse gamma distribution
          aStar <- thismodel@sigma2W["a"] + length(x1) / 2
          ## Second paramter bStar depends on the value for sigma2W
          bStar <- function(x) {
            adjW <- w1
            ret <- sum((adjW - betaW[x1Level])^2) / 2 + thismodel@sigma2W["b"]
            return(ret)
          }
          ### Draw proposal:
          bStarProposal <- bStar(sigma2W)
          sigma2W <- rinvGamma(n = 1L, a = aStar, b = bStarProposal)
        }


        ## 4)Save Samples

        if (saveSample(options, iterMcmc)) {
          samples$ExpEff[iterSave, ] <- betaW
          samples$sigma2W[iterSave, 1] <- sigma2W
          samples$sigma2betaW[iterSave, 1] <- sigma2betaW
          iterSave <- iterSave + 1L
        }
      }


      ret <- Samples(
        data = samples,
        options = options
      )
      return(ret)
    }
)
# nolint end
