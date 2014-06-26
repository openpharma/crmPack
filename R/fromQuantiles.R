#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[fromQuantiles.R] by DSB Don 26/06/2014 14:17>
##
## Description:
## Find the best LogisticNormal model for a given set of quantiles at certain
## dose levels
##
## History:
## 11/02/2014   file creation
#####################################################################################

##' @include helpers.R
##' @include Model-class.R
{}


##' Convert prior quantiles (lower, median, upper) to LogisticNormal model
##'
##' Convert prior quantiles (lower, median, upper) to LogisticNormal model
##'
##' This function uses generalised simulated annealing to optimise
##' a \code{\linkS4class{LogisticNormal}} model to be as close as possible
##' to the given prior quantiles.
##'
##' @param dosegrid the dose grid
##' @param refDose the reference dose
##' @param lower the lower quantiles
##' @param median the medians
##' @param upper the upper quantiles
##' @param level the credible level of the (lower, upper) intervals (default:
##' 0.95)
##' @param parstart starting values for the parameters. By default, these
##' are determined from the medians supplied.
##' @param parlower lower bounds on the parameters (intercept alpha and the log
##' slope theta, the corresponding standard deviations and the correlation.)
##' @param parupper upper bounds on the parameters
##' @param control additional options for the optimisation routine, see
##' \code{\link[GenSA]{GenSA}} for more details
##' @return a list with the best approximating LogisticNormal \code{model}, the
##' resulting \code{quantiles}, the \code{required} quantiles and the
##' \code{distance} to the required quantiles, as well as the final
##' \code{parameters} (which could be used for running the algorithm a
##' second time)
##'
##' @importFrom GenSA GenSA
##' @importFrom mvtnorm rmvnorm
##' @export
##' @keywords programming
Quantiles2LogisticNormal <- function(dosegrid,
                                     refDose,
                                     lower,
                                     median,
                                     upper,
                                     level=0.95,
                                     parstart=NULL,
                                     parlower=c(-10, -10, 0, 0, -0.95),
                                     parupper=c(10, 10, 10, 10, 0.95),
                                     control=
                                     list(threshold.stop=0.01,
                                          maxit=50000,
                                          temperature=50000,
                                          max.time=600,
                                          verbose=TRUE))
{
    ## extracts and checks
    nDoses <- length(dosegrid)
    stopifnot(! is.unsorted(dosegrid, strictly=TRUE),
              (refDose > dosegrid[1]) && (refDose < dosegrid[nDoses]),
              ## the medians must be monotonically increasing:
              ! is.unsorted(median),
              identical(length(lower), nDoses),
              identical(length(median), nDoses),
              identical(length(upper), nDoses),
              all(lower < median),
              all(upper > median),
              is.probability(level, bounds=FALSE),
              identical(length(parlower), 5L),
              identical(length(parupper), 5L),
              all(parlower < parupper))

    ## parametrize in terms of the means for the intercept alpha and the log
    ## slope theta,
    ## the corresponding standard deviations and the correlation.
    ## Define start values for optimisation:
    startValues <-
        if(is.null(parstart))
        {
            ## find approximate means for alpha and slope beta
            ## from fitting logistic model to medians:
            startAlphaBeta <-
                coef(lm(I(logit(median)) ~ I(log(dosegrid / refDose))))

            ## overall starting values:
            c(meanAlpha=
              startAlphaBeta[1],
              meanTheta=
              log(startAlphaBeta[2]),
              sdAlpha=
              1,
              sdTheta=
              1,
              correlation=
              0)
        } else {
            parstart
        }

    ## what is the target function which we want to minimize?
    target <- function(param)
    {
        ## form the mean vector and covariance matrix
        mean <- param[1:2]
        cov <- matrix(c(param[3]^2,
                        prod(param[3:5]),
                        prod(param[3:5]),
                        param[4]^2),
                      nrow=2L, ncol=2L)

        ## simulate from the corresponding normal distribution
        normalSamples <- mvtnorm::rmvnorm(n=1e4L,
                                          mean=mean,
                                          sigma=cov)

        ## extract separate coefficients
        alphaSamples <- normalSamples[, 1L]
        betaSamples <- exp(normalSamples[, 2L])

        ## and compute resulting quantiles
        quants <- matrix(nrow=length(dosegrid),
                         ncol=3L)
        colnames(quants) <- c("lower", "median", "upper")

        ## process each dose after another:
        for(i in seq_along(dosegrid))
        {
            ## create samples of the probability
            probSamples <-
                plogis(alphaSamples + betaSamples * log(dosegrid[i] / refDose))

            ## compute lower, median and upper quantile
            quants[i, ] <-
                quantile(probSamples,
                         probs=c((1 - level) / 2, 0.5, (1 + level) / 2))
        }

        ## now we can compute the target value
        ret <- max(abs(quants - c(lower, median, upper)))
        return(structure(ret,
                         mean=mean,
                         cov=cov,
                         quantiles=quants))
    }

    ## now optimise the target
    genSAres <- GenSA::GenSA(par=startValues,
                        fn=target,
                        lower=parlower,
                        upper=parupper,
                        control=control)
    distance <- genSAres$value
    pars <- genSAres$par
    targetRes <- target(pars)

    ## and construct the model
    ret <- new("LogisticNormal",
               mean=attr(targetRes, "mean"),
               cov=attr(targetRes, "cov"),
               refDose=refDose)

    ## return it together with the resulting distance and the quantiles
    return(list(model=ret,
                parameters=pars,
                quantiles=attr(targetRes, "quantiles"),
                required=cbind(lower, median, upper),
                distance=distance))
}


##' Construct a minimally informative prior
##'
##' This function constructs a minimally informative prior, which is captured in
##' a \code{\linkS4class{LogisticNormal}} object.
##'
##' Based on the proposal by Neuenschwander et al (2008, Statistics in
##' Medicine), a minimally informative prior distribution is constructed. The
##' required key input is the minimum (\eqn{d_{1}} in the notation of the
##' Appendix A.1 of that paper) and the maximum value (\eqn{d_{J}}) of the dose
##' grid supplied to this function. Then \code{threshmin} is the probability
##' threshold \eqn{q_{1}}, such that any probability of DLT larger than
##' \eqn{q_{1}} has only 5\% probability. Likewise, \code{threshmax} is the
##' probability threshold \eqn{q_{J}}, such that any probability of DLT smaller
##' than \eqn{q_{J}} has only 5\% probability. Subsequently, for all doses
##' supplied in the \code{dosegrid} argument, Beta distributions are set up, and
##' \code{\link{Quantiles2LogisticNormal}} is used to transform the resulting
##' quantiles into an approximating \code{\linkS4class{LogisticNormal}} model.
##'
##' @param dosegrid the dose grid
##' @param refDose the reference dose
##' @param threshmin Any toxicity probability above this threshold would
##' be very unlikely (5\%) at the minimum dose (default: 0.2)
##' @param threshmax Any toxicity probability below this threshold would
##' be very unlikely (5\%) at the maximum dose (default: 0.3)
##' @param \dots additional arguments for computations, see
##' \code{\link{Quantiles2LogisticNormal}}
##' @return see \code{\link{Quantiles2LogisticNormal}}
##'
##' @export
##' @keywords programming
MinimalInformative <- function(dosegrid,
                               refDose,
                               threshmin=0.2,
                               threshmax=0.3,
                               ...)
{
    ## extracts and checks
    nDoses <- length(dosegrid)
    stopifnot(! is.unsorted(dosegrid, strictly=TRUE),
              is.probability(threshmin, bounds=FALSE),
              is.probability(threshmax, bounds=FALSE))
    xmin <- dosegrid[1]
    xmax <- dosegrid[nDoses]

    ## calculate the median probabilities at the min and max doses
    nSim <- 1e5
    medianMin <- median(rbeta(n=nSim,
                              1,
                              log(1-0.95) / log(1-threshmin)))
    medianMax <- median(rbeta(n=nSim,
                              log(0.05) / log(threshmax),
                              1))

    ## Assume prior medians are linear in log-dose on the logit scale
    beta <- (logit(medianMax) - logit(medianMin)) / (log(xmax) - log(xmin))
    alpha <- logit(medianMax) - beta * (log(xmax / refDose))
    medianDosegrid <- plogis(alpha + beta * log(dosegrid / refDose))

    ## For each dose, calculate the minimal informative beta distribution
    lower <- upper <- dosegrid
    for(i in seq_along(dosegrid))
    {
        probs <-
            if(medianDosegrid[i] < 0.5)
            {
                a <- log(0.5) / log(medianDosegrid[i])
                rbeta(n=nSim, a, 1)
            } else {
                b <- log(1-0.5) / log(1-medianDosegrid[i])
                rbeta(n=nSim, 1, b)
            }

        lower[i] <- quantile(probs, 0.025)
        upper[i] <- quantile(probs, 0.975)
    }

    ## now go to Quantiles2LogisticNormal
    Quantiles2LogisticNormal(dosegrid=dosegrid,
                             refDose=refDose,
                             lower=lower,
                             median=medianDosegrid,
                             upper=upper,
                             level=0.95,
                             ...)
}
