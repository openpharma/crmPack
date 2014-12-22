#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[Rules-class.R] by DSB Mon 22/12/2014 16:47>
##
## Description:
## Encapsulate the rules in formal classes.
##
## History:
## 07/02/2014   file creation
###################################################################################

##' @include helpers.R
{}

## ============================================================

## --------------------------------------------------
## Virtual class for finding next best dose
## --------------------------------------------------

##' The virtual class for finding next best dose
##'
##' @seealso \code{\linkS4class{NextBestMTD}},
##' \code{\linkS4class{NextBestNCRM}},
##' \code{\linkS4class{NextBestDualEndpoint}}
##'
##' @export
##' @keywords classes
setClass(Class="NextBest",
         contains=list("VIRTUAL"))


## --------------------------------------------------
## Next best dose based on MTD estimate
## --------------------------------------------------

##' The class with the input for finding the next best MTD estimate
##'
##' @slot target the target toxicity probability
##' @slot derive the function which derives from the input, a vector of
##' posterior MTD samples called \code{mtdSamples}, the final next best MTD
##' estimate.
##'
##' @export
##' @keywords classes
setClass(Class="NextBestMTD",
         contains=list("NextBest"),
         representation=
         representation(target="numeric",
                        derive="function"),
         validity=
         function(object){
             stopifnot(is.probability(object@target,
                                      bounds=FALSE),
                       identical(names(formals(object@derive)),
                                 c("mtdSamples")))
         })


## --------------------------------------------------
## Next best dose based on NCRM rule
## --------------------------------------------------

##' The class with the input for finding the next dose in target interval
##'
##' Note that to avoid numerical problems, the dose selection algorithm has been
##' implemented as follows: First admissible doses are found, which are those
##' with probability to fall in \code{overdose} category being below
##' \code{maxOverdoseProb}. Next, within the admissible doses, the maximum
##' probability to fall in the \code{target} category is calculated. If that is
##' above 5\% (i.e., it is not just numerical error), then the corresponding
##' dose is the next recommended dose. Otherwise, the highest admissible dose is
##' the next recommended dose.
##'
##' @slot target the target toxicity interval
##' @slot overdose the overdose toxicity interval
##' @slot maxOverdoseProb maximum overdose probability that is allowed
##'
##' @export
##' @keywords classes
setClass(Class="NextBestNCRM",
         contains=list("NextBest"),
         representation=
         representation(target="numeric",
                        overdose="numeric",
                        maxOverdoseProb="numeric"),
         validity=
         function(object){
             stopifnot(is.probRange(object@target),
                       is.probRange(object@overdose),
                       is.probability(object@maxOverdoseProb))
         })


## --------------------------------------------------
## Next best dose based on dual endpoint model
## --------------------------------------------------

##' The class with the input for finding the next dose
##' based on the dual endpoint model
##'
##' @slot target the biomarker level, relative to the maximum, that
##' needs to be reached. For example, 0.9 means that a dose with 90%
##' of the maximum biomarker level is considered as having reached
##' sufficient biomarker level.
##' @slot overdose the overdose toxicity interval
##' @slot maxOverdoseProb maximum overdose probability that is allowed
##'
##' @export
##' @keywords classes
setClass(Class="NextBestDualEndpoint",
         contains=list("NextBest"),
         representation=
         representation(target="numeric",
                        overdose="numeric",
                        maxOverdoseProb="numeric"),
         validity=
         function(object){
             stopifnot(is.probability(object@target),
                       is.probRange(object@overdose),
                       is.probability(object@maxOverdoseProb))
         })


## ============================================================

## --------------------------------------------------
## Virtual class for increments control
## --------------------------------------------------

##' The virtual class for controlling increments
##'
##' @seealso \code{\linkS4class{IncrementsRelative}},
##' \code{\linkS4class{IncrementsRelativeDLT}},
##' \code{\linkS4class{IncrementsRelativeParts}}
##'
##' @export
##' @keywords classes
setClass(Class="Increments",
         contains=list("VIRTUAL"))


## --------------------------------------------------
## Increments control based on relative differences in intervals
## --------------------------------------------------

##' Increments control based on relative differences in intervals
##'
##' Note that \code{intervals} is to be read as follows. If for example,
##' we want to specify three intervals: First 0 to less than 50, second at least
##' 50 up to less than 100 mg, and third at least 100 mg, then we specify
##' \code{intervals} to be \code{c(0, 50, 100, Inf)}. That means, the right
##' bound of the intervals are exclusive to the interval.
##'
##' @slot intervals a vector with the bounds of the relevant intervals of length
##' \code{n}.
##' @slot increments a vector of length \code{n-1} with the maximum allowable
##' relative increments in the \code{intervals}
##'
##' @export
##' @keywords classes
setClass(Class="IncrementsRelative",
         contains="Increments",
         representation=
         representation(intervals="numeric",
                        increments="numeric"),
         validity=
         function(object){
             n <- length(object@intervals)
             stopifnot(identical(length(object@increments),
                                 n-1L),
                       ! is.unsorted(object@intervals, strictly=TRUE))
         })


## --------------------------------------------------
## Increments control based on relative differences in intervals,
## with special rules for part 1 and beginning of part 2
## --------------------------------------------------

##' Increments control based on relative differences in intervals,
##' with special rules for part 1 and beginning of part 2
##'
##' Note that this only works in conjunction with \code{\linkS4class{DataParts}}
##' objects. If the part 2 will just be started in the next cohort, then the
##' next maximum dose will be either \code{dltStart} (e.g. -1) shift of the last
##' part 1 dose in case of a DLT in part 1, or \code{cleanStart} shift (e.g. 0)
##' in case of no DLTs in part 1. If part 1 will still be on in the next cohort,
##' then the next dose level will be the next higher dose level in the
##' \code{part1Ladder} of the data object. If part 2 has been started before,
##' the usual relative increment rules apply, see
##' \code{\linkS4class{IncrementsRelative}}.
##'
##' @slot dltStart integer giving the dose level increment for starting part 2
##' in case of a DLT in part 1
##' @slot cleanStart integer giving the dose level increment for starting part 2
##' in case of a DLT in part 1. If this is less or equal to 0, then the part 1
##' ladder will be used to find the maximum next dose. If this is larger than 0,
##' then the relative increment rules will be applied to find the next maximum
##' dose level.
##'
##' @export
##' @keywords classes
setClass(Class="IncrementsRelativeParts",
         contains="IncrementsRelative",
         representation=
         representation(dltStart="integer",
                        cleanStart="integer"),
         validity=
         function(object){
             stopifnot(is.scalar(object@dltStart),
                       is.scalar(object@cleanStart),
                       object@cleanStart >= object@dltStart) # meaningful
                                        # assumption
         })



## --------------------------------------------------
## Increments control based on relative differences in terms of DLTs
## --------------------------------------------------

##' Increments control based on relative differences in terms of DLTs
##'
##' Note that \code{DLTintervals} is to be read as follows. If for example,
##' we want to specify three intervals: First 0 DLTs, second 1 or 2 DLTs, and
##' third at least 3 DLTs, then we specify
##' \code{DLTintervals} to be \code{c(0, 1, 3, Inf)}. That means, the right
##' bound of the intervals are exclusive to the interval.
##'
##' @slot DLTintervals a vector with the bounds of the relevant DLT intervals of
##' length \code{n}
##' @slot increments a vector of length \code{n-1} with the maximum allowable
##' relative increments in the \code{DLTintervals}
##'
##' @export
##' @keywords classes
setClass(Class="IncrementsRelativeDLT",
         contains="Increments",
         representation=
         representation(DLTintervals="numeric",
                        increments="numeric"),
         validity=
         function(object){
             n <- length(object@DLTintervals)
             stopifnot(identical(length(object@increments),
                                 n-1L),
                       ! is.unsorted(object@DLTintervals, strictly=TRUE))
         })

## ============================================================

## --------------------------------------------------
## Virtual class for stopping rules
## --------------------------------------------------

##' The virtual class for stopping rules
##'
##' @seealso \code{\linkS4class{StoppingList}},
##' \code{\linkS4class{StoppingMaxPatients}},
##' \code{\linkS4class{StoppingCohortsNearDose}},
##' \code{\linkS4class{StoppingPatientsNearDose}},
##' \code{\linkS4class{StoppingMinCohorts}},
##' \code{\linkS4class{StoppingMinPatients}},
##' \code{\linkS4class{StoppingTargetProb}}
##' \code{\linkS4class{StoppingMTDdistribution}},
##' \code{\linkS4class{StoppingTargetBiomarker}}
##'
##' @export
##' @keywords classes
setClass(Class="Stopping",
         contains=list("VIRTUAL"))


## --------------------------------------------------
## Stopping based on multiple stopping rules
## --------------------------------------------------

##' Stop based on multiple stopping rules
##'
##' This class can be used to combine multiple stopping rules.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}, and the \code{summary} is a function
##' taking a logical vector of the size of \code{stopList} and returning a
##' single logical value. For example, if the function \code{all} is given as
##' \code{summary} function, then this means that all stopping rules must be
##' fulfilled in order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##' @slot summary the summary function to combine the results
##' of the stopping rules into a single result
##'
##' @keywords classes
##' @export
setClass(Class="StoppingList",
         contains="Stopping",
         representation=
         representation(stopList="list",
                        summary="function"),
         validity=
         function(object){
             stopifnot(all(sapply(object@stopList, is, "Stopping")))
         })


## --------------------------------------------------
## Stopping based on fulfillment of all multiple stopping rules
## --------------------------------------------------

##' Stop based on fullfillment of all multiple stopping rules
##'
##' This class can be used to combine multiple stopping rules with an AND
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. All stopping rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##' of the stopping rules into a single result
##'
##' @keywords classes
##' @export
setClass(Class="StoppingAll",
         contains="Stopping",
         representation=
         representation(stopList="list"),
         validity=
         function(object){
             stopifnot(all(sapply(object@stopList, is, "Stopping")))
         })

## --------------------------------------------------
## Stopping based on fulfillment of any stopping rule
## --------------------------------------------------

##' Stop based on fullfillment of any stopping rule
##'
##' This class can be used to combine multiple stopping rules with an OR
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. Any of these rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##' of the stopping rules into a single result
##'
##' @keywords classes
##' @export
setClass(Class="StoppingAny",
         contains="Stopping",
         representation=
         representation(stopList="list"),
         validity=
         function(object){
             stopifnot(all(sapply(object@stopList, is, "Stopping")))
         })


## --------------------------------------------------
## Stopping based on maximum number of patients
## --------------------------------------------------

##' Stop based on maximum number of patients
##'
##' @slot nPatients maximum allowed number of patients
##'
##' @keywords classes
##' @export
setClass(Class="StoppingMaxPatients",
         contains="Stopping",
         representation=
         representation(nPatients="integer"))


## --------------------------------------------------
## Stopping based on number of cohorts near to next best dose
## --------------------------------------------------

##' Stop based on number of cohorts near to next best dose
##'
##' @slot nCohorts number of required cohorts
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the cohorts must lie
##'
##' @keywords classes
##' @export
setClass(Class="StoppingCohortsNearDose",
         contains="Stopping",
         representation=
         representation(nCohorts="integer",
                        percentage="numeric"))

## --------------------------------------------------
## Stopping based on number of patients near to next best dose
## --------------------------------------------------

##' Stop based on number of patients near to next best dose
##'
##' @slot nPatients number of required patients
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the patients must lie
##'
##' @keywords classes
##' @export
setClass(Class="StoppingPatientsNearDose",
         contains="Stopping",
         representation=
         representation(nPatients="integer",
                        percentage="numeric"))

## --------------------------------------------------
## Stopping based on minimum number of cohorts
## --------------------------------------------------

##' Stop based on minimum number of cohorts
##'
##' @slot nCohorts minimum required number of cohorts
##'
##' @keywords classes
##' @export
setClass(Class="StoppingMinCohorts",
         contains="Stopping",
         representation=
         representation(nCohorts="integer"))

## --------------------------------------------------
## Stopping based on minimum number of patients
## --------------------------------------------------

##' Stop based on minimum number of patients
##'
##' @slot nPatients minimum required number of patients
##'
##' @keywords classes
##' @export
setClass(Class="StoppingMinPatients",
         contains="Stopping",
         representation=
         representation(nPatients="integer"))


## --------------------------------------------------
## Stopping based on probability of target tox interval
## --------------------------------------------------

##' Stop based on probability of target tox interval
##'
##' @slot target the target toxicity interval
##' @slot prob required target toxicity probability
##' for reaching sufficient precision
##'
##' @keywords classes
##' @export
setClass(Class="StoppingTargetProb",
         contains="Stopping",
         representation=
         representation(target="numeric",
                        prob="numeric"),
         validity=
         function(object){
             stopifnot(is.probRange(object@target),
                       is.probability(object@prob,
                                      bounds=FALSE))
         })


## --------------------------------------------------
## Stopping based on MTD distribution
## --------------------------------------------------

##' Stop based on MTD distribution
##'
##' Has 90\% probability above a threshold of 50\% of the current
##' MTD been reached? This class is used for this question.
##'
##' @slot target the target toxicity probability (e.g. 0.33) defining the MTD
##' @slot thresh the threshold relative to the MTD (e.g. 0.5)
##' @slot prob required probability (e.g. 0.9)
##'
##' @keywords classes
##' @export
setClass(Class="StoppingMTDdistribution",
         contains="Stopping",
         representation=
         representation(target="numeric",
                        thresh="numeric",
                        prob="numeric"),
         validity=
         function(object){
             stopifnot(is.probability(object@target,
                                      bounds=FALSE),
                       is.probability(object@thresh,
                                      bounds=FALSE),
                       is.probability(object@prob,
                                      bounds=FALSE))
         })


## --------------------------------------------------
## Stopping based on probability of target biomarker
## --------------------------------------------------

##' Stop based on probability of target biomarker
##'
##' @slot target the biomarker level, relative to the maximum, that
##' needs to be reached
##' @slot prob required target probability
##' for reaching sufficient precision
##'
##' @keywords classes
##' @export
setClass(Class="StoppingTargetBiomarker",
         contains="Stopping",
         representation=
         representation(target="numeric",
                        prob="numeric"),
         validity=
         function(object){
             stopifnot(is.probability(object@target),
                       is.probability(object@prob,
                                      bounds=FALSE))
         })


## ============================================================



## --------------------------------------------------
## Virtual class for cohort sizes
## --------------------------------------------------

##' The virtual class for cohort sizes
##'
##' @seealso \code{\linkS4class{CohortSizeMax}},
##' \code{\linkS4class{CohortSizeMin}},
##' \code{\linkS4class{CohortSizeRange}},
##' \code{\linkS4class{CohortSizeDLT}},
##' \code{\linkS4class{CohortSizeConst}},
##' \code{\linkS4class{CohortSizeParts}}
##'
##' @export
##' @keywords classes
setClass(Class="CohortSize",
         contains=list("VIRTUAL"))


## --------------------------------------------------
## Size based on maximum of multiple cohort size rules
## --------------------------------------------------

##' Size based on maximum of multiple cohort size rules
##'
##' This class can be used to combine multiple cohort size rules with the MAX
##' operation.
##'
##' \code{cohortSizeList} contains all cohort size rules, which are again
##' objects of class \code{\linkS4class{CohortSize}}. The maximum of these
##' individual cohort sizes is taken to give the final cohort size.
##'
##' @slot cohortSizeList list of cohort size rules
##'
##' @keywords classes
##' @export
setClass(Class="CohortSizeMax",
         contains="CohortSize",
         representation=
         representation(cohortSizeList="list"),
         validity=
         function(object){
             stopifnot(all(sapply(object@cohortSizeList, is, "CohortSize")))
         })

## --------------------------------------------------
## Size based on minimum of multiple cohort size rules
## --------------------------------------------------

##' Size based on minimum of multiple cohort size rules
##'
##' This class can be used to combine multiple cohort size rules with the MIN
##' operation.
##'
##' \code{cohortSizeList} contains all cohort size rules, which are again
##' objects of class \code{\linkS4class{CohortSize}}. The minimum of these
##' individual cohort sizes is taken to give the final cohort size.
##'
##' @slot cohortSizeList list of cohort size rules
##'
##' @keywords classes
##' @export
setClass(Class="CohortSizeMin",
         contains="CohortSize",
         representation=
         representation(cohortSizeList="list"),
         validity=
         function(object){
             stopifnot(all(sapply(object@cohortSizeList, is, "CohortSize")))
         })


## --------------------------------------------------
## Cohort size based on dose range
## --------------------------------------------------

##' Cohort size based on dose range
##'
##' @slot intervals a vector with the bounds of the relevant dose intervals of
##' length \code{n}
##' @slot cohortSize an integer vector of length \code{n-1} with the cohort
##' sizes in the \code{intervals}
##'
##' @export
##' @keywords classes
setClass(Class="CohortSizeRange",
         contains="CohortSize",
         representation=
         representation(intervals="numeric",
                        cohortSize="integer"),
         validity=
         function(object){
             n <- length(object@intervals)
             stopifnot(identical(length(object@cohortSize),
                                 n-1L),
                       all(object@cohortSize > 0),
                       ! is.unsorted(object@intervals, strictly=TRUE))
         })

## --------------------------------------------------
## Cohort size based on number of DLTs
## --------------------------------------------------

##' Cohort size based on number of DLTs
##'
##' @slot DLTintervals a vector with the bounds of the relevant DLT intervals of
##' length \code{n}
##' @slot cohortSize an integer vector of length \code{n-1} with the cohort
##' sizes in the \code{DLTintervals}
##'
##' @export
##' @keywords classes
setClass(Class="CohortSizeDLT",
         contains="CohortSize",
         representation=
         representation(DLTintervals="numeric",
                        cohortSize="integer"),
         validity=
         function(object){
             n <- length(object@DLTintervals)
             stopifnot(identical(length(object@cohortSize),
                                 n-1L),
                       all(object@cohortSize > 0),
                       ! is.unsorted(object@DLTintervals, strictly=TRUE))
         })

## --------------------------------------------------
## Constant cohort size
## --------------------------------------------------

##' Constant cohort size
##'
##' This class is used when the cohort size should be kept constant.
##'
##' @slot size the constant integer size
##'
##' @keywords classes
##' @export
setClass(Class="CohortSizeConst",
         contains="CohortSize",
         representation=
         representation(size="integer"),
         validity=
         function(object){
             stopifnot(is.scalar(size),
                       object@size > 0)
         })

## --------------------------------------------------
## Cohort size based on the parts
## --------------------------------------------------

##' Cohort size based on the parts
##'
##' This class is used when the cohort size should change for the second part of
##' the dose escalation. Only works in conjunction with
##' \code{\linkS4class{DataParts}} objects.
##'
##' @slot sizes the two sizes for part 1 and part 2
##'
##' @keywords classes
##' @export
setClass(Class="CohortSizeParts",
         contains="CohortSize",
         representation=
         representation(sizes="integer"),
         validity=
         function(object){
             stopifnot(all(object@sizes > 0),
                       identical(length(object@sizes), 2L))
         })


## ============================================================
