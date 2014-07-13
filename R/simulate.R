#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[simulate.R] by DSB Don 10/07/2014 14:13>
##
## Description:
## Simulate outcomes from a CRM trial, assuming a true dose-toxicity
## relationship.
##
## History:
## 12/02/2014   file creation
## 07/04/2014   start with parallelization on cores
#####################################################################################

##' @include Data-methods.R
##' @include Design-class.R
##' @include McmcOptions-class.R
##' @include Rules-methods.R
##' @include Simulations-class.R
##' @include helpers.R
##' @include mcmc.R
{}

##' Simulate outcomes from a CRM design
##'
##' @param object the \code{\linkS4class{Design}} object we want to simulate
##' data from
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
##' @param nsim the number of simulations (default: 1)
##' @param mcmcOptions object of class \code{\linkS4class{McmcOptions}},
##' giving the MCMC options for each evaluation in the trial. By default,
##' the standard options are used
##' @param seed an object specifying if and how the random number generator
##' should be initialized (\dQuote{seeded}). Either \code{NULL} (default) or an
##' integer that will be used in a call to \code{\link{set.seed}} before
##' simulating the response vectors. If set, the value is saved as the
##' \code{seed} slot of the returned object. The default, \code{NULL} will
##' not change the random generator state, and \code{.Random.seed} will be
##' saved.
##' @param parallel should the simulation runs be parallelized across the
##' clusters of the computer? (not default)
##'
##' @return an object of class \code{\linkS4class{Simulations}}
##'
##' @importFrom parallel detectCores makeCluster clusterApply stopCluster
##' mclapply
##'
##' @export
##' @keywords methods
setMethod("simulate",
          signature=
          signature(object="Design"),
          def=
          function(object, truth, args=NULL, firstSeparate=FALSE, nsim=1L,
                   mcmcOptions=new("McmcOptions"), seed=NULL,
                   parallel=FALSE, ...){

              ## checks and extracts
              stopifnot(is.function(truth),
                        is.bool(firstSeparate),
                        is.scalar(nsim),
                        nsim > 0,
                        is.bool(parallel),
                        is.scalar(parallel))

              args <- as.data.frame(args)
              nArgs <- max(nrow(args), 1L)

              ## seed handling: copied from simulate.lm
              if(! exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                  runif(1)
              if(is.null(seed))
              {
                  RNGstate <- get(".Random.seed", envir = .GlobalEnv)
              } else {
                  R.seed <- get(".Random.seed", envir = .GlobalEnv)
                  set.seed(seed)
                  RNGstate <- structure(seed, kind = as.list(RNGkind()))
                  on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
              }

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
                  thisFit <- fitted(object=thisSamples,
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

              resultList <-
                  if(! parallel)
                  {
                      lapply(X=seq_len(nsim),
                             FUN=runSim)
                  } else {

                      ## now process all simulations
                      cores <- parallel::detectCores()

                      ## clusterApply() for Windows
                      if (Sys.info()[1] == "Windows")
                      {
                          ## start the cluster
                          cl <- parallel::makeCluster(cores)

                          ## load the required R package
                          parallel::clusterEvalQ(cl, {
                              library(crmPack)
                              NULL
                          })

                          ## export local variables
                          parallel::clusterExport(cl=cl,
                                                  varlist=
                                                  c("simSeeds",
                                                    "args",
                                                    "nArgs",
                                                    "firstSeparate",
                                                    "truth",
                                                    "object",
                                                    "mcmcOptions"),
                                                  envir=environment())

                          ## export all global variables
                          parallel::clusterExport(cl=cl,
                                                  varlist=ls(.GlobalEnv))

                          ## now do the computations
                          res <- parallel::parLapply(cl=cl,
                                                     X=seq_len(nsim),
                                                     fun=runSim)

                          ## stop the cluster
                          parallel::stopCluster(cl)

                          res
                      } else {

                          ## mclapply() for everybody else
                          res <- parallel::mclapply(X=seq_len(nsim),
                                                    FUN=runSim,
                                                    mc.cores=cores)
                          res
                      }
                  }


              ## put everything in this format:

              ## setup the list for the simulated data objects
              dataList <- lapply(resultList, "[[", "data")

              ## the vector of the final dose recommendations
              recommendedDoses <- sapply(resultList, "[[", "dose")

              ## setup the list for the final fits
              fitList <- lapply(resultList, "[[", "fit")

              ## the reasons for stopping
              stopReasons <- lapply(resultList, "[[", "stop")

              ## return the results in the Simulations class object
              ret <- new("Simulations",
                         data=dataList,
                         doses=recommendedDoses,
                         fit=fitList,
                         stopReasons=stopReasons,
                         seed=RNGstate)

              return(ret)
          })



