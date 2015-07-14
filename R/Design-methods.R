#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
##         Wai Yin Yeung [w*.* yeung1 *a*t* lancaster *.* ac *.* uk]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Design-methods.R] by DSB Son 03/05/2015 20:35>
##
## Description:
## Simulate outcomes from a CRM trial, assuming a true dose-toxicity
## relationship.
##
## History:
## 12/02/2014   file creation
## 07/04/2014   start with parallelization on cores
## 02/01/2015   rename: simulate.R --> Design-methods.R
## 10/07/2015   added simulate methods
#####################################################################################

##' @include Data-methods.R
##' @include Design-class.R
##' @include McmcOptions-class.R
##' @include Rules-methods.R
##' @include Simulations-class.R
##' @include helpers.R
##' @include mcmc.R
{}

##' Helper function to set and save the RNG seed
##'
##' This is basically copied from simulate.lm
##'
##' @param seed an object specifying if and how the random number generator
##' should be initialized (\dQuote{seeded}). Either \code{NULL} (default) or an
##' integer that will be used in a call to \code{\link{set.seed}} before
##' simulating the response vectors. If set, the value is saved as the
##' \code{seed} slot of the returned object. The default, \code{NULL} will
##' not change the random generator state.
##' @return The RNGstate will be returned, in order to call this function
##' with this input to reproduce the obtained simulation results
##'
##' @export
##' @keywords programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
setSeed <- function(seed=NULL)
{
    if(! exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    {
        runif(1)
    }

    if(is.null(seed))
    {
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        ## make sure R.seed exists in parent frame:
        assign("R.seed", R.seed, envir=parent.frame())
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        do.call("on.exit",
                list(quote(assign(".Random.seed", R.seed, envir = .GlobalEnv))),
                envir = parent.frame())
        ## here we need the R.seed in the parent.frame!
    }

    return(RNGstate)
}


##' Helper function to obtain simulation results list
##'
##' The function \code{fun} can use variables that are visible to itself. The
##' names of these variables have to given in the vector \code{vars}.
##'
##' @param fun the simulation function for a single iteration, which takes as
##' single parameter the iteration index
##' @param nsim number of simulations to be conducted
##' @param vars names of the variables
##' @param parallel shall the iterations be parallelized across the cores?
##' @return the list with all simulation results (one iteration corresponds
##' to one list element)
##'
##' @importFrom parallel detectCores makeCluster clusterApply stopCluster
##' @keywords internal programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
getResultList <- function(fun,
                          nsim,
                          vars,
                          parallel)
{
    ret <-
        if(! parallel)
        {
            lapply(X=seq_len(nsim),
                   FUN=fun)
        } else {

            ## now process all simulations
            cores <- parallel::detectCores()

            ## start the cluster
            cl <- parallel::makeCluster(cores)

            ## load the required R package
            parallel::clusterEvalQ(cl, {
                library(crmPack)
                NULL
            })

            ## export local variables
            parallel::clusterExport(cl=cl,
                                    varlist=vars,
                                    envir=parent.frame())
            ## parent.frame() gives back the caller environment
            ## (different from parent.env() which returns
            ## the environment where this function has been
            ## defined!)

            ## export all global variables
            parallel::clusterExport(cl=cl,
                                    varlist=ls(.GlobalEnv))

            ## now do the computations
            res <- parallel::parLapply(cl=cl,
                                       X=seq_len(nsim),
                                       fun=fun)

            ## stop the cluster
            parallel::stopCluster(cl)

            res
        }

    return(ret)
}


## ============================================================

##' Simulate outcomes from a CRM design
##'
##' @param object the \code{\linkS4class{Design}} object we want to simulate
##' data from
##' @param nsim the number of simulations (default: 1)
##' @param seed see \code{\link{setSeed}}
##' @param truth a function which takes as input a dose (vector) and returns the
##' true probability (vector) for toxicity. Additional arguments can be supplied
##' in \code{args}.
##' @param args data frame with arguments for the \code{truth} function. The
##' column names correspond to the argument names, the rows to the values of the
##' arguments. The rows are appropriately recycled in the \code{nsim}
##' simulations. In order to produce outcomes from the posterior predictive
##' distribution, e.g, pass an \code{object} that contains the data observed so
##' far, \code{truth} contains the \code{prob} function from the model in
##' \code{object}, and \code{args} contains posterior samples from the model.
##' @param firstSeparate enroll the first patient separately from the rest of
##' the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
##' in this patient.
##' @param mcmcOptions object of class \code{\linkS4class{McmcOptions}},
##' giving the MCMC options for each evaluation in the trial. By default,
##' the standard options are used
##' @param parallel should the simulation runs be parallelized across the
##' clusters of the computer? (not default)
##' @param \dots not used
##'
##' @return an object of class \code{\linkS4class{Simulations}}
##'
##' @export
##' @keywords methods
setMethod("simulate",
          signature=
              signature(object="Design",
                        nsim="ANY",
                        seed="ANY"),
          def=
              function(object, nsim=1L, seed=NULL,
                       truth, args=NULL, firstSeparate=FALSE,
                       mcmcOptions=McmcOptions(),
                       parallel=FALSE, ...){

              nsim <- safeInteger(nsim)

              ## checks and extracts
              stopifnot(is.function(truth),
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))

              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)

              ## seed handling
              RNGstate <- setSeed(seed)

              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)

              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                  ## set the seed for this run
                  set.seed(simSeeds[iterSim])

                  ## what is now the argument for the truth?
                  ## (appropriately recycled)
                  thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]

                  ## so this truth is...
                  thisTruth <- function(dose)
                  {
                      do.call(truth,
                              ## First argument: the dose
                              c(dose,
                                ## Following arguments
                                thisArgs))
                  }

                  ## start the simulated data with the provided one
                  thisData <- object@data

                  ## shall we stop the trial?
                  ## First, we want to continue with the starting dose.
                  ## This variable is updated after each cohort in the loop.
                  stopit <- FALSE

                  ## what is the next dose to be used?
                  ## initialize with starting dose
                  thisDose <- object@startingDose

                  ## inside this loop we simulate the whole trial, until stopping
                  while(! stopit)
                  {
                      ## what is the probability for tox. at this dose?
                      thisProb <- thisTruth(thisDose)

                      ## what is the cohort size at this dose?
                      thisSize <- size(cohortSize=object@cohortSize,
                                       dose=thisDose,
                                       data=thisData)

                      ## simulate DLTs: depends on whether we
                      ## separate the first patient or not.
                      if(firstSeparate && (thisSize > 1L))
                      {
                          ## dose the first patient
                          thisDLTs <- rbinom(n=1L,
                                             size=1L,
                                             prob=thisProb)
                          ## if there is no DLT:
                          if(thisDLTs == 0)
                          {
                              ## enroll the remaining patients
                              thisDLTs <- c(thisDLTs,
                                            rbinom(n=thisSize - 1L,
                                                   size=1L,
                                                   prob=thisProb))
                          }
                      } else {
                          ## we can directly dose all patients
                          thisDLTs <- rbinom(n=thisSize,
                                             size=1L,
                                             prob=thisProb)
                      }

                      ## update the data with this cohort
                      thisData <- update(object=thisData,
                                         x=thisDose,
                                         y=thisDLTs)

                      ## what is the dose limit?
                      doselimit <- maxDose(object@increments,
                                           data=thisData)

                      ## generate samples from the model
                      thisSamples <- mcmc(data=thisData,
                                          model=object@model,
                                          options=mcmcOptions)

                      ## => what is the next best dose?
                      thisDose <- nextBest(object@nextBest,
                                           doselimit=doselimit,
                                           samples=thisSamples,
                                           model=object@model,
                                           data=thisData)$value

                      ## evaluate stopping rules
                      stopit <- stopTrial(object@stopping,
                                          dose=thisDose,
                                          samples=thisSamples,
                                          model=object@model,
                                          data=thisData)
                  }

                  ## get the fit
                  thisFit <- fit(object=thisSamples,
                                    model=object@model,
                                    data=thisData)

                  ## return the results
                  thisResult <-
                      list(data=thisData,
                           dose=thisDose,
                           fit=
                           subset(thisFit,
                                  select=c(middle, lower, upper)),
                           stop=
                           attr(stopit,
                                "message"))
                  return(thisResult)
              }

              resultList <- getResultList(fun=runSim,
                                          nsim=nsim,
                                          vars=
                                          c("simSeeds",
                                            "args",
                                            "nArgs",
                                            "firstSeparate",
                                            "truth",
                                            "object",
                                            "mcmcOptions"),
                                          parallel=parallel)

              ## put everything in the Simulations format:

              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")

              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "dose"))

              ## setup the list for the final fits
              fitList <- lapply(resultList, "[[", "fit")

              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")

              ## return the results in the Simulations class object
              ret <- Simulations(data=dataList,
                                 doses=recommendedDoses,
                                 fit=fitList,
                                 stopReasons=stopReasons,
                                 seed=RNGstate)

              return(ret)
          })




##' Simulate outcomes from a rule-based design
##'
##' @param object the \code{\linkS4class{RuleDesign}} object we want to simulate
##' data from
##' @param nsim the number of simulations (default: 1)
##' @param seed see \code{\link{setSeed}}
##' @param truth a function which takes as input a dose (vector) and returns the
##' true probability (vector) for toxicity. Additional arguments can be supplied
##' in \code{args}.
##' @param args data frame with arguments for the \code{truth} function. The
##' column names correspond to the argument names, the rows to the values of the
##' arguments. The rows are appropriately recycled in the \code{nsim}
##' simulations.
##' @param parallel should the simulation runs be parallelized across the
##' clusters of the computer? (not default)
##' @param \dots not used
##'
##' @return an object of class \code{\linkS4class{GeneralSimulations}}
##'
##' @export
##' @keywords methods
setMethod("simulate",
          signature=
              signature(object="RuleDesign",
                        nsim="ANY",
                        seed="ANY"),
          def=
              function(object, nsim=1L, seed=NULL,
                       truth, args=NULL,
                       parallel=FALSE, ...){

              nsim <- safeInteger(nsim)

              ## checks and extracts
              stopifnot(is.function(truth),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))

              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)

              ## seed handling
              RNGstate <- setSeed(seed)

              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)

              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                  ## set the seed for this run
                  set.seed(simSeeds[iterSim])

                  ## what is now the argument for the truth?
                  ## (appropriately recycled)
                  thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]

                  ## so this truth is...
                  thisTruth <- function(dose)
                  {
                      do.call(truth,
                              ## First argument: the dose
                              c(dose,
                                ## Following arguments
                                thisArgs))
                  }

                  ## start the simulated data with the provided one
                  thisData <- object@data

                  ## shall we stop the trial?
                  ## First, we want to continue with the starting dose.
                  ## This variable is updated after each cohort in the loop.
                  stopit <- FALSE

                  ## what is the next dose to be used?
                  ## initialize with starting dose
                  thisDose <- object@startingDose

                  ## inside this loop we simulate the whole trial, until stopping
                  while(! stopit)
                  {
                      ## what is the probability for tox. at this dose?
                      thisProb <- thisTruth(thisDose)

                      ## what is the cohort size at this dose?
                      thisSize <- size(cohortSize=object@cohortSize,
                                       dose=thisDose,
                                       data=thisData)

                      ## simulate DLTs
                      thisDLTs <- rbinom(n=thisSize,
                                         size=1L,
                                         prob=thisProb)

                      ## update the data with this cohort
                      thisData <- update(object=thisData,
                                         x=thisDose,
                                         y=thisDLTs)

                      ## evaluate the rule
                      thisOutcome <- nextBest(object@nextBest,
                                              data=thisData)

                      thisDose <- thisOutcome$value
                      stopit <- thisOutcome$stopHere
                  }

                  ## return the results
                  thisResult <-
                      list(data=thisData,
                           dose=thisDose)

                  return(thisResult)
              }

              resultList <- getResultList(fun=runSim,
                                          nsim=nsim,
                                          vars=
                                          c("simSeeds",
                                            "args",
                                            "nArgs",
                                            "truth",
                                            "object"),
                                          parallel=parallel)

              ## put everything in the GeneralSimulations format:

              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")

              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "dose"))

              ## return the results in the GeneralSimulations class object
              ret <- GeneralSimulations(data=dataList,
                                        doses=recommendedDoses,
                                        seed=RNGstate)

              return(ret)
          })


##' Simulate outcomes from a dual-endpoint design
##'
##' @param object the \code{\linkS4class{DualDesign}} object we want to simulate
##' data from
##' @param nsim the number of simulations (default: 1)
##' @param seed see \code{\link{setSeed}}
##' @param trueTox a function which takes as input a dose (vector) and returns the
##' true probability (vector) for toxicity. Additional arguments can be supplied
##' in \code{args}.
##' @param trueBiomarker a function which takes as input a dose (vector) and
##' returns the true biomarker level (vector). Additional arguments can be
##' supplied in \code{args}.
##' @param args data frame with arguments for the \code{trueTox} and
##' \code{trueBiomarker} function. The column names correspond to the argument
##' names, the rows to the values of the arguments. The rows are appropriately
##' recycled in the \code{nsim} simulations.
##' @param sigma2W variance for the biomarker measurements
##' @param rho correlation between toxicity and biomarker measurements (default:
##' 0)
##' @param firstSeparate enroll the first patient separately from the rest of
##' the cohort? (not default) If yes, the cohort will be closed if a DLT occurs
##' in this patient.
##' @param mcmcOptions object of class \code{\linkS4class{McmcOptions}},
##' giving the MCMC options for each evaluation in the trial. By default,
##' the standard options are used
##' @param parallel should the simulation runs be parallelized across the
##' clusters of the computer? (not default)
##' @param \dots not used
##'
##' @return an object of class \code{\linkS4class{DualSimulations}}
##'
##' @importFrom mvtnorm rmvnorm
##' @export
##' @keywords methods
setMethod("simulate",
          signature=
          signature(object="DualDesign"),
          def=
              function(object, nsim=1L, seed=NULL,
                       trueTox, trueBiomarker, args=NULL,
                       sigma2W, rho=0,
                       firstSeparate=FALSE,
                       mcmcOptions=McmcOptions(),
                       parallel=FALSE, ...){

              nsim <- safeInteger(nsim)

              ## checks and extracts
              stopifnot(is.function(trueTox),
                        is.function(trueBiomarker),
                        is.scalar(sigma2W), sigma2W > 0,
                        is.scalar(rho), rho < 1, rho > -1,
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))

              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)

              ## get names of arguments (excluding the first one which is the dose)
              trueToxArgnames <- names(formals(trueTox))[-1]
              trueBiomarkerArgnames <- names(formals(trueBiomarker))[-1]

              ## this is the covariance matrix we assume:
              trueCov <- matrix(c(sigma2W, sqrt(sigma2W) * rho,
                                  sqrt(sigma2W) * rho, 1),
                                nrow=2, byrow=TRUE)

              ## seed handling
              RNGstate <- setSeed(seed)

              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)

              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                  ## set the seed for this run
                  set.seed(simSeeds[iterSim])

                  ## what is now the argument for the true functions?
                  ## (appropriately recycled)
                  thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]

                  ## so the true tox function is:
                  thisTrueTox <- function(dose)
                  {
                      do.call(trueTox,
                              ## First argument: the dose
                              c(dose,
                                ## Following arguments: take only those that
                                ## are required by the tox function
                                as.list(thisArgs)[trueToxArgnames]))
                  }

                  ## and the true biomarker function is:
                  thisTrueBiomarker <- function(dose)
                  {
                      do.call(trueBiomarker,
                              ## First argument: the dose
                              c(dose,
                                ## Following arguments: take only those that
                                ## are required by the biomarker function
                                as.list(thisArgs)[trueBiomarkerArgnames]))
                  }

                  ## start the simulated data with the provided one
                  thisData <- object@data

                  ## shall we stop the trial?
                  ## First, we want to continue with the starting dose.
                  ## This variable is updated after each cohort in the loop.
                  stopit <- FALSE

                  ## what is the next dose to be used?
                  ## initialize with starting dose
                  thisDose <- object@startingDose

                  ## inside this loop we simulate the whole trial, until stopping
                  while(! stopit)
                  {
                      ## what is the probability for tox. at this dose?
                      thisProb <- thisTrueTox(thisDose)
                      ## and the transformation to the z scale is:
                      thisMeanZ <- qlogis(thisProb)

                      ## what is the biomarker mean at this dose?
                      thisMeanBiomarker <- thisTrueBiomarker(thisDose)

                      ## what is the cohort size at this dose?
                      thisSize <- size(cohortSize=object@cohortSize,
                                       dose=thisDose,
                                       data=thisData)

                      ## simulate tox and biomarker response: depends on whether we
                      ## separate the first patient or not.
                      tmp <-
                          if(firstSeparate && (thisSize > 1L))
                          {
                              ## dose the first patient
                              tmpStart <- mvtnorm::rmvnorm(n=1,
                                                           mean=
                                                           c(thisMeanBiomarker,
                                                             thisMeanZ),
                                                           sigma=trueCov)
                              ## if there is no DLT:
                              if(tmpStart[, 2] < 0)
                              {
                                  ## enroll the remaining patients
                                  tmpStart <-
                                      rbind(tmpStart,
                                            mvtnorm::rmvnorm(n=thisSize - 1,
                                                             mean=
                                                             c(thisMeanBiomarker,
                                                               thisMeanZ),
                                                             sigma=trueCov))
                              }
                              tmpStart
                          } else {
                              ## we can directly dose all patients
                              mvtnorm::rmvnorm(n=thisSize,
                                               mean=
                                               c(thisMeanBiomarker,
                                                 thisMeanZ),
                                               sigma=trueCov)
                          }

                      ## extract biomarker and DLT samples
                      thisBiomarkers <- tmp[, 1]
                      thisDLTs <- as.integer(tmp[, 2] > 0)

                      ## update the data with this cohort
                      thisData <- update(object=thisData,
                                         x=thisDose,
                                         y=thisDLTs,
                                         w=thisBiomarkers)

                      ## what is the dose limit?
                      doselimit <- maxDose(object@increments,
                                           data=thisData)

                      ## generate samples from the model
                      thisSamples <- mcmc(data=thisData,
                                          model=object@model,
                                          options=mcmcOptions)

                      ## => what is the next best dose?
                      thisDose <- nextBest(object@nextBest,
                                           doselimit=doselimit,
                                           samples=thisSamples,
                                           model=object@model,
                                           data=thisData)$value

                      ## evaluate stopping rules
                      stopit <- stopTrial(object@stopping,
                                          dose=thisDose,
                                          samples=thisSamples,
                                          model=object@model,
                                          data=thisData)
                  }

                  ## get the fit
                  thisFit <- fit(object=thisSamples,
                                 model=object@model,
                                 data=thisData)

                  ## return the results
                  thisResult <-
                      list(data=thisData,
                           dose=thisDose,
                           fitTox=
                           subset(thisFit,
                                  select=
                                  c(middle, lower, upper)),
                           fitBiomarker=
                           subset(thisFit,
                                  select=
                                  c(middleBiomarker, lowerBiomarker,
                                    upperBiomarker)),
                           rhoEst=median(thisSamples@data$rho),
                           sigma2West=median(1/thisSamples@data$precW),
                           stop=
                           attr(stopit,
                                "message"))

                  return(thisResult)
              }

              resultList <- getResultList(fun=runSim,
                                          nsim=nsim,
                                          vars=
                                          c("simSeeds",
                                            "args",
                                            "nArgs",
                                            "firstSeparate",
                                            "trueTox",
                                            "trueBiomarker",
                                            "trueCov",
                                            "object",
                                            "mcmcOptions"),
                                          parallel=parallel)

              ## put everything in the Simulations format:

              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")

              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "dose"))

              ## vector of rho estimates
              rhoEstimates <- as.numeric(sapply(resultList, "[[", "rhoEst"))

              ## vector of sigma2W estimates
              sigma2Westimates <- as.numeric(sapply(resultList, "[[", "sigma2West"))

              ## setup the list for the final tox fits
              fitToxList <- lapply(resultList, "[[", "fitTox")

              ## setup the list for the final biomarker fits
              fitBiomarkerList <- lapply(resultList, "[[", "fitBiomarker")

              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")

              ## return the results in the DualSimulations class object
              ret <- DualSimulations(
                         data=dataList,
                         doses=recommendedDoses,
                         rhoEst=rhoEstimates,
                         sigma2West=sigma2Westimates,
                         fit=fitToxList,
                         fitBiomarker=fitBiomarkerList,
                         stopReasons=stopReasons,
                         seed=RNGstate)

              return(ret)
          })


## ============================================================

##' Obtain hypothetical trial course table for a design
##'
##' This generic function takes a design and generates a dataframe
##' showing the beginning of several hypothetical trial courses under
##' the design. This means, from the generated dataframe one can read off:
##' - how many cohorts are required in the optimal case (no DLTs observed) in
##'   order to reach the highest dose of the specified dose grid
##' - assuming no DLTs are observed until a certain dose level, what the next
##'   recommended dose is for all possible number of DLTs observed
##' - the actual relative increments that will be used in these cases
##' - whether the trial would stop at a certain cohort
##' Examining the "single trial" behavior of a dose escalation design is
##' the first important step in evaluating a design, and cannot be replaced by
##' studying solely the operating characteristics in "many trials". The cohort
##' sizes are also taken from the design, assuming no DLTs occur until the dose
##' listed.
##'
##' @param object the design (\code{\linkS4class{Design}} or
##' \code{\linkS4class{RuleDesign}} object) we want to examine
##' @param \dots additional arguments (see methods)
##'
##' @return The data frame
##'
##' @export
##' @keywords methods regression
setGeneric("examine",
           def=
           function(object, ...){
               ## there should be no default method,
               ## therefore just forward to next method!
               standardGeneric("examine")
           },
           valueClass="data.frame")


##' @describeIn examine Examine a model-based CRM
##'
##' @param mcmcOptions object of class \code{\linkS4class{McmcOptions}},
##' giving the MCMC options for each evaluation in the trial. By default,
##' the standard options are used
setMethod("examine",
          signature=
              signature(object="Design"),
          def=
              function(object, mcmcOptions=McmcOptions(), ...){

                  ## start with the empty table
                  ret <- data.frame(dose=numeric(),
                                    DLTs=integer(),
                                    nextDose=numeric(),
                                    stop=logical(),
                                    increment=integer())

                  ## start the base data with the provided one
                  baseData <- object@data

                  ## are we finished and can stop?
                  stopit <- FALSE

                  ## what is the next dose to be used?
                  ## initialize with starting dose
                  thisDose <- object@startingDose

                  ## inside this loop we continue filling up the table, until
                  ## stopping
                  while(! stopit)
                  {
                      ## what is the cohort size at this dose?
                      thisSize <- size(cohortSize=object@cohortSize,
                                       dose=thisDose,
                                       data=baseData)

                      ## for all possible number of DLTs:
                      for(numDLTs in 0:thisSize)
                      {
                          ## update data with corresponding DLT vector
                          thisData <-
                              update(object=baseData,
                                     x=thisDose,
                                     y=
                                         rep(x=c(0, 1),
                                             times=
                                                 c(thisSize - numDLTs,
                                                   numDLTs)))

                          ## what is the dose limit?
                          doselimit <- maxDose(object@increments,
                                               data=thisData)

                          ## generate samples from the model
                          thisSamples <- mcmc(data=thisData,
                                              model=object@model,
                                              options=mcmcOptions)

                          ## => what is the next best dose?
                          nextDose <- nextBest(object@nextBest,
                                               doselimit=doselimit,
                                               samples=thisSamples,
                                               model=object@model,
                                               data=thisData)$value

                          ## compute relative increment in percent
                          thisIncrement <-
                              round((nextDose - thisDose) / thisDose * 100)

                          ## evaluate stopping rules
                          stopThisTrial <- stopTrial(object@stopping,
                                                     dose=nextDose,
                                                     samples=thisSamples,
                                                     model=object@model,
                                                     data=thisData)

                          ## append information to the data frame
                          ret <- rbind(ret,
                                       list(dose=thisDose,
                                            DLTs=numDLTs,
                                            nextDose=nextDose,
                                            stop=stopThisTrial,
                                            increment=as.integer(thisIncrement)))
                      }

                      ## change base data
                      baseData <-
                          update(object=baseData,
                                 x=thisDose,
                                 y=rep(0, thisSize))

                      ## what is the new dose according to table?
                      newDose <- as.numeric(subset(ret,
                                                   dose==thisDose & DLTs==0,
                                                   select=nextDose))

                      ## what is the difference to the previous dose?
                      doseDiff <- newDose - thisDose

                      ## update dose
                      thisDose <- newDose

                      ## check if we can stop:
                      ## either when we have reached the highest dose in the
                      ## next cohort, or when there is no improvement in dose
                      ## over the last cohort
                      stopit <- (thisDose >= max(object@data@doseGrid)) ||
                          (! (doseDiff > 0))
                  }

                  return(ret)
              })



##' @describeIn examine Examine a rule-based design
setMethod("examine",
          signature=
              signature(object="RuleDesign"),
          def=
              function(object, ...){

                  ## start with the empty table
                  ret <- data.frame(dose=numeric(),
                                    DLTs=integer(),
                                    nextDose=numeric(),
                                    stop=logical(),
                                    increment=integer())

                  ## start the base data with the provided one
                  baseData <- object@data

                  ## are we finished and can stop?
                  stopit <- FALSE

                  ## what is the next dose to be used?
                  ## initialize with starting dose
                  thisDose <- object@startingDose

                  ## inside this loop we continue filling up the table, until
                  ## stopping
                  while(! stopit)
                  {
                      ## what is the cohort size at this dose?
                      thisSize <- size(cohortSize=object@cohortSize,
                                       dose=thisDose,
                                       data=baseData)

                      ## for all possible number of DLTs:
                      for(numDLTs in 0:thisSize)
                      {
                          ## update data with corresponding DLT vector
                          thisData <-
                              update(object=baseData,
                                     x=thisDose,
                                     y=
                                         rep(x=c(0, 1),
                                             times=
                                                 c(thisSize - numDLTs,
                                                   numDLTs)))

                          ## evaluate the rule
                          thisOutcome <- nextBest(object@nextBest,
                                                  data=thisData)

                          ## next dose
                          nextDose <- thisOutcome$value

                          ## do we stop here?
                          stopThisTrial <- thisOutcome$stopHere

                          ## compute relative increment in percent
                          thisIncrement <-
                              round((nextDose - thisDose) / thisDose * 100)

                          ## append information to the data frame
                          ret <- rbind(ret,
                                       list(dose=thisDose,
                                            DLTs=numDLTs,
                                            nextDose=nextDose,
                                            stop=stopThisTrial,
                                            increment=as.integer(thisIncrement)))
                      }

                      ## change base data
                      baseData <-
                          update(object=baseData,
                                 x=thisDose,
                                 y=rep(0, thisSize))

                      ## what is the new dose according to table?
                      newDose <- as.numeric(subset(ret,
                                                   dose==thisDose & DLTs==0,
                                                   select=nextDose))

                      ## what is the difference to the previous dose?
                      doseDiff <- newDose - thisDose

                      ## update dose
                      thisDose <- newDose

                      ## check if we can stop:
                      ## either when we have reached the highest dose in the
                      ## next cohort, or when there is no improvement in dose
                      ## over the last cohort
                      stopit <- (thisDose >= max(object@data@doseGrid)) ||
                          (! (doseDiff > 0))
                  }

                  return(ret)
              })

## ============================================================
## ----------------------------------------------------------
## Methods for simulate using Pseudo Models
## ----------------------------------------------------------
##'  Methods to simulate using 'LogisticIndepBeta' model with samples 
##'  
##'  @export
##'  @keywords methods
setMethod("simulate",
          signature=
            signature(object="TDsamplesDesign",
                      nsim="ANY",
                      seed="ANY"),
          def=
            function(object, nsim=1L, seed=NULL,
                     truth, args=NULL, firstSeparate=FALSE,
                     mcmcOptions=McmcOptions(),
                     parallel=FALSE, ...){
              
              nsim <- safeInteger(nsim)
              
              ## checks and extracts
              stopifnot(is.function(truth),
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))
              
              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)
              
              
              ## seed handling
              RNGstate <- setSeed(seed)
              
              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)
              
              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                ## set the seed for this run
                set.seed(simSeeds[iterSim])
                
                ## what is now the argument for the truth?
                ## (appropriately recycled)
                thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]
                
                ## so this truth is...
                thisTruth <- function(dose)
                {
                  do.call(truth,
                          ## First argument: the dose
                          c(dose,
                            ## Following arguments
                            thisArgs))
                }
                
                ## start the simulated data with the provided one
                thisData <- object@data
                
                ## shall we stop the trial?
                ## First, we want to continue with the starting dose.
                ## This variable is updated after each cohort in the loop.
                stopit <- FALSE
                
                ## what is the next dose to be used?
                ## initialize with starting dose
                thisDose <- object@startingDose
                
                ## inside this loop we simulate the whole trial, until stopping
                while(! stopit)
                {
                  ## what is the probability for tox. at this dose?
                  thisProb <- thisTruth(thisDose)
                  
                  
                  ## what is the cohort size at this dose?
                  thisSize <- size(cohortSize=object@cohortSize,
                                   dose=thisDose,
                                   data=thisData)
                  
                  
                  ## simulate DLTs: depends on whether we
                  ## separate the first patient or not.
                  if(firstSeparate && (thisSize > 1L))
                  {
                    ## dose the first patient
                    thisDLTs <- rbinom(n=1L,
                                       size=1L,
                                       prob=thisProb)
                    ## if there is no DLT:
                    if(thisDLTs == 0)
                    {
                      ## enroll the remaining patients
                      thisDLTs <- c(thisDLTs,
                                    rbinom(n=thisSize - 1L,
                                           size=1L,
                                           prob=thisProb))
                    }
                  } else {
                    ## we can directly dose all patients
                    thisDLTs <- rbinom(n=thisSize,
                                       size=1L,
                                       prob=thisProb)    
                  }
                  
                  ## update the data with this cohort
                  thisData <- update(object=thisData,
                                     x=thisDose,
                                     y=thisDLTs)
                  
                  ##Update the model with thisData
                  thisModel <- update(object@model,
                                      data=thisData)
                  
                  ## what is the dose limit?
                  doselimit <- maxDose(object@increments,
                                       data=thisData)
                  
                  ## generate samples from the model
                  thisSamples <- mcmc(data=thisData,
                                      model=thisModel,
                                      options=mcmcOptions)
                  
                  ## => what is the next best dose?
                  
                  NEXT<-nextBest(object@nextBest,
                                 doselimit=doselimit,
                                 samples=thisSamples,
                                 model=thisModel,
                                 data=thisData)
                  
                  thisDose <- NEXT$nextdose
                  
                  
                  thistargetDuringTrialDose<- NEXT$TDtargetDuringTrialEstimate
                  
                  
                  thistargetEndOfTrialDose<- NEXT$TDtargetEndOfTrialEstimate
                  
                  
                  
                  thisTDtargetEndOfTrialatdoseGrid <- NEXT$TDtargetEndOfTrialAtDoseGrid
                  
                  
                  
                  ## evaluate stopping rules
                  stopit <- stopTrial(object@stopping,
                                      dose=thisDose,
                                      samples=thisSamples,
                                      model=thisModel,
                                      data=thisData)
                }
                
                ## get the fit
                thisFit <- fit(object=thisSamples,
                               model=thisModel,
                               data=thisData)
                
                
                ## return the results
                thisResult <-
                  list(data=thisData,
                       dose=thisDose,
                       TDtargetDuringTrial=thistargetDuringTrialDose,
                       TDtargetEndOfTrial=thistargetEndOfTrialDose,
                       TDtargetEndOfTrialatdoseGrid=thisTDtargetEndOfTrialatdoseGrid,
                       fit=
                         subset(thisFit,
                                select=c(middle, lower, upper)),
                       stop=
                         attr(stopit,
                              "message"))
                return(thisResult)
              }
              
              resultList <- crmPack:::getResultList(fun=runSim,
                                                    nsim=nsim,
                                                    vars=
                                                      c("simSeeds",
                                                        "args",
                                                        "nArgs",
                                                        "firstSeparate",
                                                        "truth",
                                                        "object",
                                                        "mcmcOptions"),
                                                    parallel=parallel)
              
              ## put everything in the Simulations format:
              
              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")
              
              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "TDtargetEndOfTrialatdoseGrid"))
              
              ## setup the list for the final fits
              fitList <- lapply(resultList, "[[", "fit")
              
              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")
              
              ## return the results in the Simulations class object
              ret <- Simulations(data=dataList,
                                 doses=recommendedDoses,
                                 fit=fitList,
                                 stopReasons=stopReasons,
                                 seed=RNGstate)
              
              return(ret)
            })
## -------------------------------------------------------------------------------------
## ----------------------------------------------------------
## Methods for simulate using Pseudo Models
## ----------------------------------------------------------
##'  Methods to simulate using 'LogisticIndepBeta' model without samples 
##'  
##'  @export
##'  @keywords methods
setMethod("simulate",
          signature=
            signature(object="TDDesign",
                      nsim="ANY",
                      seed="ANY"),
          def=
            function(object, nsim=1L, seed=NULL,
                     truth, args=NULL, firstSeparate=FALSE,
                     parallel=FALSE, ...){
              
              nsim <- safeInteger(nsim)
              
              ## checks and extracts
              stopifnot(is.function(truth),
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))
              
              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)
              
              ## seed handling
              RNGstate <- crmPack:::setSeed(seed)
              
              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)
              
              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                ## set the seed for this run
                set.seed(simSeeds[iterSim])
                
                ## what is now the argument for the truth?
                ## (appropriately recycled)
                thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]
                
                ## so this truth is...
                thisTruth <- function(dose)
                {
                  do.call(truth,
                          ## First argument: the dose
                          c(dose,
                            ## Following arguments
                            thisArgs))
                }
                
                ## start the simulated data with the provided one
                thisData <- object@data
                
                ## shall we stop the trial?
                ## First, we want to continue with the starting dose.
                ## This variable is updated after each cohort in the loop.
                stopit <- FALSE
                
                ## what is the next dose to be used?
                ## initialize with starting dose
                thisDose <- object@startingDose
                
                ## inside this loop we simulate the whole trial, until stopping
                while(! stopit)
                {
                  ## what is the probability for tox. at this dose?
                  thisProb <- thisTruth(thisDose)
                  
                  ## what is the cohort size at this dose?
                  thisSize <- size(cohortSize=object@cohortSize,
                                   dose=thisDose,
                                   data=thisData)
                  
                  ## simulate DLTs: depends on whether we
                  ## separate the first patient or not.
                  if(firstSeparate && (thisSize > 1L))
                  {
                    ## dose the first patient
                    thisDLTs <- rbinom(n=1L,
                                       size=1L,
                                       prob=thisProb)
                    ## if there is no DLT:
                    if(thisDLTs == 0)
                    {
                      ## enroll the remaining patients
                      thisDLTs <- c(thisDLTs,
                                    rbinom(n=thisSize - 1L,
                                           size=1L,
                                           prob=thisProb))
                    }
                  } else {
                    ## we can directly dose all patients
                    thisDLTs <- rbinom(n=thisSize,
                                       size=1L,
                                       prob=thisProb)
                  }
                  
                  ## update the data with this cohort
                  thisData <- update(object=thisData,
                                     x=thisDose,
                                     y=thisDLTs)
                  
                  
                  ##Update model estimates with thisData
                  thisModel <- update(object@model,
                                      data=thisData)
                  
                  ## what is the dose limit?
                  doselimit <- maxDose(object@increments,data=thisData)
                  
                  
                  ## => what is the next best dose?
                  NEXT<-nextBest(object@nextBest,
                                 doselimit=doselimit,
                                 model=thisModel,
                                 data=thisData)
                  
                  thistargetDuringTrialDose<- NEXT$TDtargetDuringTrialEstimate
                  
                  
                  thistargetEndOfTrialDose<- NEXT$TDtargetEndOfTrialEstimate
                  
                  
                  
                  thisTDtargetEndOfTrialatdoseGrid <- NEXT$TDtargetEndOfTrialatdoseGrid
                  
                  
                  ## evaluate stopping rules
                  stopit <- stopTrial(object@stopping,
                                      dose=thisDose,
                                      model=thisModel,
                                      data=thisData)
                }
                
                
                ## return the results
                thisResult <-
                  list(data=thisData,
                       dose=thisDose,
                       TDtargetDuringTrial=thistargetDuringTrialDose,
                       TDtargetEndOfTrial=thistargetEndOfTrialDose,
                       TDtargetEndOfTrialatdoseGrid=thisTDtargetEndOfTrialatdoseGrid,
                       stop=
                         attr(stopit,
                              "message"))
                return(thisResult)
              }
              
              
              resultList <- crmPack:::getResultList(fun=runSim,
                                                    nsim=nsim,
                                                    vars=
                                                      c("simSeeds",
                                                        "args",
                                                        "nArgs",
                                                        "firstSeparate",
                                                        "truth",
                                                        "object"),
                                                    parallel=parallel)
              
              ## put everything in the Simulations format:
              
              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")
              
              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "TDtargetEndOfTrialatdoseGrid"))
              
              
              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")
              
              ## return the results in the Simulations class object
              ret <- PseudoSimulations(data=dataList,
                                       doses=recommendedDoses,
                                       stopReasons=stopReasons,
                                       seed=RNGstate)
              
              return(ret)
            })
## -----------------------------------------------------------------------------------------------
## Method for simulate based on Dual responses on Pseudo models without samples
## ----------------------------------------------------------------------------------
##' Methods to simulate without any samples based on one Pseudo DLE and one Pseudo Efficacy model
##' 
##' @export
##' @keywords method

setMethod("simulate",
          signature=
            signature(object="DualResponsesDesign",
                      nsim="ANY",
                      seed="ANY"),
          def=
            function(object, nsim=1L, seed=NULL,
                     trueDLE, trueEff, trueNu,
                     args=NULL, firstSeparate=FALSE,
                     parallel=FALSE, ...){
              
              nsim <- safeInteger(nsim)
              
              ## checks and extracts
              stopifnot(is.function(trueDLE),
                        is.function(trueEff),
                        trueNu > 0,
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel))
              
              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)
              
              ## get names of arguments (excluding the first one which is the dose)
              trueDLEArgnames <- names(formals(trueDLE))[-1]
              trueEffArgnames <- names(formals(trueEff))[-1]
              
              
              
              ## seed handling
              RNGstate <- crmPack:::setSeed(seed)
              
              ## from this,
              ## generate the individual seeds for the simulation runs
              simSeeds <- sample(x=seq_len(1e5), size=nsim)
              
              ## the function to produce the run a single simulation
              ## with index "iterSim"
              runSim <- function(iterSim)
              {
                ## set the seed for this run
                set.seed(simSeeds[iterSim])
                
                ## what is now the argument for the truth?
                ## (appropriately recycled)
                thisArgs <- args[(iterSim - 1) %% nArgs + 1, , drop=FALSE]
                
                ## so this truth DLE function is...
                thisTruthDLE <- function(dose)
                { do.call(trueDLE,
                          ## First argument: the dose
                          c(dose,
                            ## Following arguments: take only those that
                            ## are required by the DLE function
                            as.list(thisArgs)[trueDLEArgnames]))
                }
                
                ##and the truth Eff function is:
                thisTruthEff <- function (dose)
                { 
                  do.call(trueEff,
                          ## First argument: the dose
                          c(dose,
                            ## Following arguments: take only those that
                            ## are required by the Eff function
                            as.list(thisArgs)[trueEffArgnames]))
                }
                
                ## start the simulated data with the provided one
                thisData <- object@data
                
              ## find true sigma2 to generate responses
                
                trueSigma2<-1/trueNu
                
                
                ## shall we stop the trial?
                ## First, we want to continue with the starting dose.
                ## This variable is updated after each cohort in the loop.
                stopit <- FALSE
                
                ## what is the next dose to be used?
                ## initialize with starting dose
                thisDose <- object@startingDose
                
                ## inside this loop we simulate the whole trial, until stopping
                while(! stopit)
                {
                  ## what is the probability for tox. at this dose?
                  thisDLEProb <- thisTruthDLE(thisDose)
                  thisEff<-thisTruthEff(thisDose)
                  
                  ## what is the cohort size at this dose?
                  thisSize <- size(cohortSize=object@cohortSize,
                                   dose=thisDose,
                                   data=thisData)
                  
                  ## simulate DLTs: depends on whether we
                  ## separate the first patient or not.
                  if(firstSeparate && (thisSize > 1L))
                  {
                    ## dose the first patient
                    thisDLTs <- rbinom(n=1L,
                                       size=1L,
                                       prob=thisDLEProb)
                    thisEff <- rnorm(n=1L,
                                     mean=thisEff,
                                     sd=sqrt(trueSigma2))
                    
                    ## if there is no DLT:
                    if(thisDLTs == 0)
                    {
                      ## enroll the remaining patients
                      thisDLTs <- c(thisDLTs,
                                    rbinom(n=thisSize - 1L,
                                           size=1L,
                                           prob=thisDLEProb))
                      thisEff<-c(thisEff,
                                 rnorm(n=thisSize - 1L,
                                       mean=thisEff,
                                       sd=sqrt(trueSigma2)))
                    }
                  } else {
                    ## we can directly dose all patients
                    thisDLTs <- rbinom(n=thisSize,
                                       size=1L,
                                       prob=thisDLEProb)
                    thisEff <- rnorm(n=thisSize,
                                     mean=thisEff,
                                     sd=sqrt(trueSigma2))
                  }
                  
                  
                  
                  ## update the data with this cohort
                  thisData <- update(object=thisData,
                                     x=thisDose,
                                     y=thisDLTs,
                                     w=thisEff)
                  
                  
                  ##Update model estimate in DLE and Eff models
                  thisDLEModel <- update(object=object@DLEmodel,
                                         data=thisData)
                  
                  thisEffModel <- update(object=object@Effmodel,
                                         data=thisData)
                  
                  thisNu<-thisEffModel@nu
                  
                  
                  if (thisEffModel@useFixed==FALSE){
                    thisSigma2 <- as.numeric(thisNu["a"]/thisNu["b"])} else {
                      thisSigma2 <- 1/thisNu}
                  
                  
                  ## what is the dose limit?
                  doselimit <- maxDose(object@increments,data=thisData)
                  
                  
                  
                  ## => what is the next best dose?
                  NEXT<-nextBest(object@nextBestGain,
                                 doselimit=doselimit,
                                 model=thisDLEModel,
                                 data=thisData,
                                 Effmodel=thisEffModel)
                  
                  thisDose <- NEXT$nextdose
                  
                  thisTDtarget<- NEXT$TDtargetEstimate
                  
                  thisTDtargetatdoseGrid <- NEXT$TDtargetAtDoseGrid
                  
                  thisTD30<-NEXT$TD30Estimate
                  
                  
                  thisTD30atdoseGrid <-NEXT$TD30AtDoseGrid
                  
                  thisGstar<-NEXT$GstarEstimate
                  
                  thisGstaratdosegrid <- NEXT$GstarAtDoseGrid
                  
                  
                  Recommend<-min(thisTD30atdoseGrid,thisGstaratdosegrid)
                  
                  ## evaluate stopping rules
                  stopit <- stopTrial(object@stopping,
                                      dose=thisDose,
                                      data=thisData)
                  
                }
                
                ## return the results
                thisResult <- list(data=thisData,
                                   dose=thisDose,
                                   TDtargetDose=thisTDtarget,
                                   TDtargetDoseAtDoseGrid=thisTDtargetatdoseGrid,
                                   TD30=thisTD30,
                                   TD30AtDoseGrid=thisTD30atdoseGrid,
                                   Gstar=thisGstar,
                                   GstarAtDoseGrid=thisGstaratdosegrid,
                                   Recommend=Recommend,
                                   nu=thisNu,
                                   sigma2est=thisSigma2,
                                   stop=attr(stopit,
                                             "message"))
                
                return(thisResult)
              }
              
              
              resultList <- getResultList(fun=runSim,
                                                    nsim=nsim,
                                                    vars=
                                                      c("simSeeds",
                                                        "args",
                                                        "nArgs",
                                                        "firstSeparate",
                                                        "trueDLE",
                                                        "trueEff",
                                                        "trueNu",
                                                        "object"),
                                                    parallel=parallel)
              
              
              ## put everything in the Simulations format:
              
              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")
              
              ## the vector of the final dose recommendations
              recommendedDoses <- as.numeric(sapply(resultList, "[[", "Recommend"))
              
              ## the vector of the sigma2
              sigma2Estimates <- as.numeric(sapply(resultList, "[[", "sigma2est"))
              
              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")
              
              ## return the results in the Simulations class object
              ret <- PseudoDualSimulations(data=dataList,
                                           doses=recommendedDoses,
                                           sigma2est=sigma2Estimates,
                                           stopReasons=stopReasons,
                                           seed=RNGstate)
              return(ret)
              print(ret)
              
              
            })

##=========================================================================
