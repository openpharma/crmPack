# nolint start

#' @include helpers.R
#' @include Rules-validity.R
NULL

## ============================================================

## --------------------------------------------------
## Virtual class for finding next best dose
## --------------------------------------------------

##' The virtual class for finding next best dose
##'
##' @seealso \code{\linkS4class{NextBestMTD}},
##' \code{\linkS4class{NextBestNCRM}},
##' \code{\linkS4class{NextBestDualEndpoint}},
##' \code{\linkS4class{NextBestThreePlusThree}}
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
##' @example examples/Rules-class-NextBestMTD.R
##' @export
##' @keywords classes
.NextBestMTD <-
    setClass(Class="NextBestMTD",
             representation(target="numeric",
                            derive="function"),
             prototype(target=0.3,
                       derive=
                           function(mtdSamples){
                               quantile(mtdSamples,
                                        probs=0.3)}),
             contains=list("NextBest"),
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probability(object@target,
                                            bounds=FALSE),
                             "target must be probability > 0 and < 1")
                     o$check(identical(names(formals(object@derive)),
                                       c("mtdSamples")),
                             "derive must have as single argument 'mtdSamples'")

                     o$result()
                 })
validObject(.NextBestMTD())

##' Initialization function for class "NextBestMTD"
##'
##' @param target see \code{\linkS4class{NextBestMTD}}
##' @param derive see \code{\linkS4class{NextBestMTD}}
##' @return the \code{\linkS4class{NextBestMTD}} object
##'
##' @export
##' @keywords methods
NextBestMTD <- function(target,
                        derive)
{
    .NextBestMTD(target=target,
                 derive=derive)
}


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
##' above 5% (i.e., it is not just numerical error), then the corresponding
##' dose is the next recommended dose. Otherwise, the highest admissible dose is
##' the next recommended dose.
##'
##' @slot target the target toxicity interval (limits included)
##' @slot overdose the overdose toxicity interval (lower limit excluded, upper
##' limit included)
##' @slot maxOverdoseProb maximum overdose probability that is allowed
##'
##' @example examples/Rules-class-NextBestNCRM.R
##' @export
##' @keywords classes
.NextBestNCRM <-
    setClass(Class="NextBestNCRM",
             representation(target="numeric",
                            overdose="numeric",
                            maxOverdoseProb="numeric"),
             prototype(target=c(0.2, 0.35),
                       overdose=c(0.35, 1),
                       maxOverdoseProb=0.25),
             contains=list("NextBest"),
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probRange(object@target),
                             "target has to be a probability range")
                     o$check(is.probRange(object@overdose),
                             "overdose has to be a probability range")
                     o$check(is.probability(object@maxOverdoseProb),
                             "maxOverdoseProb has to be a probability")

                     o$result()
                 })
validObject(.NextBestNCRM())


##' Initialization function for "NextBestNCRM"
##'
##' @param target see \code{\linkS4class{NextBestNCRM}}
##' @param overdose see \code{\linkS4class{NextBestNCRM}}
##' @param maxOverdoseProb see \code{\linkS4class{NextBestNCRM}}
##' @return the \code{\linkS4class{NextBestNCRM}} object
##'
##' @export
##' @keywords methods
NextBestNCRM <- function(target,
                         overdose,
                         maxOverdoseProb)
{
    .NextBestNCRM(target=target,
                  overdose=overdose,
                  maxOverdoseProb=maxOverdoseProb)
}

## --------------------------------------------------
## Next best dose based on 3+3 rule
## --------------------------------------------------

##' The class with the input for finding the next dose in target interval
##'
##' Implements the classical 3+3 dose recommendation.
##' No input is required, hence this class has no slots.
##'
##' @example examples/Rules-class-NextBestThreePlusThree.R
##' @export
##' @keywords classes
.NextBestThreePlusThree <-
    setClass(Class="NextBestThreePlusThree",
             contains=list("NextBest"))

##' Initialization function for "NextBestThreePlusThree"
##'
##' @return the \code{\linkS4class{NextBestThreePlusThree}} object
##'
##' @export
##' @keywords methods
NextBestThreePlusThree <- function()
{
    .NextBestThreePlusThree()
}


## --------------------------------------------------
## Next best dose based on dual endpoint model
## --------------------------------------------------

##' The class with the input for finding the next dose
##' based on the dual endpoint model
##'
##' This rule first excludes all doses that exceed the probability
##' \code{maxOverdoseProb} of having an overdose toxicity, as specified by the
##' overdose interval \code{overdose}. Then, it picks under the remaining
##' admissible doses the one that maximizes the probability to be in the
##' \code{target} biomarker range, by default relative to the maximum biomarker level
##' across the dose grid or relative to the Emax parameter in case a parametric
##' model was selected (e.g. \code{\linkS4class{DualEndpointBeta}},
##' \code{\linkS4class{DualEndpointEmax}})) However, is \code{scale} is set to
##' "absolute" then the natural absolute biomarker scale can be used to set a target.
##'
##' @slot target the biomarker target range, that
##' needs to be reached. For example, (0.8, 1.0) and \code{scale="relative"}
##' means we target a dose
##' with at least 80% of maximum biomarker level. As an other example,
##' (0.5, 0.8) would mean that we target a dose between 50% and 80% of
##' the maximum biomarker level.
##' @slot scale either \code{relative} (default, then the \code{target} is interpreted
##' relative to the maximum, so must be a probability range) or \code{absolute}
##' (then the \code{target} is interpreted as absolute biomarker range)
##' @slot overdose the overdose toxicity interval (lower limit excluded, upper
##' limit included)
##' @slot maxOverdoseProb maximum overdose probability that is allowed
##' @slot targetThresh which target probability threshold needs to be fulfilled before the
##' target probability will be used for deriving the next best dose (default: 0.01)
##'
##' @example examples/Rules-class-NextBestDualEndpoint.R
##' @export
##' @keywords classes
.NextBestDualEndpoint <-
    setClass(Class="NextBestDualEndpoint",
             representation(target="numeric",
                            scale="character",
                            overdose="numeric",
                            maxOverdoseProb="numeric",
                            targetThresh="numeric"),
             prototype(target=c(0.9,1),
                       scale="relative",
                       overdose=c(0.35, 1),
                       maxOverdoseProb=0.25,
                       targetThresh=0.01),
             contains=list("NextBest"),
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@scale) && object@scale %in% c("relative", "absolute"),
                             "scale must be either 'relative' or 'absolute'")
                     if(object@scale == "relative")
                     {
                       o$check(is.probRange(object@target),
                               "target has to be a probability range when scale='relative'")
                     } else {
                       o$check(is.range(object@target),
                               "target must be a numeric range")
                     }
                     o$check(is.probRange(object@overdose),
                             "overdose has to be a probability range")
                     o$check(is.probability(object@maxOverdoseProb),
                             "maxOverdoseProb has to be a probability")
                     o$check(is.probability(object@targetThresh),
                             "targetThresh has to be a probability")

                     o$result()
                 })
validObject(.NextBestDualEndpoint())

##' Initialization function for "NextBestDualEndpoint"
##'
##' @param target see \code{\linkS4class{NextBestDualEndpoint}}
##' @param scale see \code{\linkS4class{NextBestDualEndpoint}}
##' @param overdose see \code{\linkS4class{NextBestDualEndpoint}}
##' @param maxOverdoseProb see \code{\linkS4class{NextBestDualEndpoint}}
##' @param targetThresh see \code{\linkS4class{NextBestDualEndpoint}}
##' @return the \code{\linkS4class{NextBestDualEndpoint}} object
##'
##' @export
##' @keywords methods
NextBestDualEndpoint <- function(target,
                                 scale=c("relative", "absolute"),
                                 overdose,
                                 maxOverdoseProb,
                                 targetThresh=0.01)
{
  scale <- match.arg(scale)
  .NextBestDualEndpoint(target=target,
                        scale=scale,
                        overdose=overdose,
                        maxOverdoseProb=maxOverdoseProb,
                        targetThresh=targetThresh)
}

## ============================================================


#' Class for Next Best Dose based on Minimum Distance to Target Probability
#'
#' This is typically used for CRM designs where a single target DLT probability
#' is targeted.
#'
#' @slot target target DLT probability.
#'
#' @export
#' @aliases NextBestMinDist
.NextBestMinDist <- setClass(Class = "NextBestMinDist",
                             contains = "NextBest",
                             representation(target = "numeric"))
validObject(.NextBestMinDist())

#' @describeIn NextBestMinDist-class Initialization function for `NextBestMinDist`.
#' @param target target DLT probability.
#' @export
NextBestMinDist <- function(target){ .NextBestMinDist(target = target) }
validObject(NextBestMinDist(0.1))

## ============================================================

## --------------------------------------------------
## Virtual class for increments control
## --------------------------------------------------

##' The virtual class for controlling increments
##'
##' @seealso \code{\linkS4class{IncrementsRelative}},
##' \code{\linkS4class{IncrementsRelativeDLT}},
##' \code{\linkS4class{IncrementsRelativeParts}},
##' [`IncrementsNumDoseLevels`]
##' [`IncrementsHSRBeta`]
##'
##' @aliases Increments
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
##' \code{intervals} to be \code{c(0, 50, 100)}. That means, the right
##' bound of the intervals are exclusive to the interval, and the last interval
##' goes from the last value until infinity.
##'
##' @slot intervals a vector with the left bounds of the relevant intervals
##' @slot increments a vector of the same length with the maximum allowable
##' relative increments in the \code{intervals}
##'
##' @example examples/Rules-class-IncrementsRelative.R
##' @export
##' @keywords classes
.IncrementsRelative <-
    setClass(Class="IncrementsRelative",
             representation(intervals="numeric",
                            increments="numeric"),
             prototype(intervals=c(0, 2),
                       increments=c(2, 1)),
             contains="Increments",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@increments),
                                       length(object@intervals)),
                             "increments must have same length as intervals")
                     o$check(! is.unsorted(object@intervals, strictly=TRUE),
                             "intervals has to be sorted and have unique values")

                     o$result()
                 })
validObject(.IncrementsRelative())

##' Initialization function for "IncrementsRelative"
##'
##' @param intervals see \code{\linkS4class{IncrementsRelative}}
##' @param increments see \code{\linkS4class{IncrementsRelative}}
##' @return the \code{\linkS4class{IncrementsRelative}} object
##'
##' @export
##' @keywords methods
IncrementsRelative <- function(intervals,
                               increments)
{
    .IncrementsRelative(intervals=intervals,
                        increments=increments)
}

# nolint end

# IncrementsNumDoseLevels-class ----

#' `IncrementsNumDoseLevels`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @slot maxLevels (`count`)\cr corresponding to the number of maximum
#'   dose levels to increment for the next dose. It defaults to 1,
#' which means that no dose skipping is allowed - the next dose
#' can be maximum one level higher than the current dose.
#' @slot basisLevel (`string`)\cr corresponding to the dose level used to increment from.
#' It can take two possible values `last` or `max`. If `last` (default)
#' is specified the increments is applied to the last given dose and if
#' `max` is specified the increment is applied from the max given dose
#' level.
#'
#' @example examples/Rules-class-IncrementsNumDoseLevels.R
#' @export
.IncrementsNumDoseLevels <- setClass(
  Class = "IncrementsNumDoseLevels",
  contains = "Increments",
  representation = representation(
    maxLevels = "integer",
    basisLevel = "character"
  ),
  prototype(
    maxLevels = 1L,
    basisLevel = "last"
  ),
  validity = validate_increments_numdoselevels
)

# IncrementsNumDoseLevels-constructor ----

#' @rdname IncrementsNumDoseLevels-class
#' @param maxLevels see below.
#' @param basisLevel see below.
#' @export
IncrementsNumDoseLevels <- function(maxLevels=1,
                                    basisLevel="last"){
  .IncrementsNumDoseLevels(
    maxLevels=safeInteger(maxLevels),
    basisLevel=basisLevel
  )
}


# IncrementsHSRBeta-class ----

#' `IncrementsHSRBeta`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`IncrementsHSRBeta`] is a class for limiting further increments using
#' a Hard Safety Rule based on the Bin-Beta model.
#' Increment control is based on the number of observed DLTs and number of
#' subjects at each dose level. The probability of toxicity is calculated
#' using a Bin-Beta model with prior (a,b). If the probability exceeds
#' the threshold for a given dose, that dose and all doses above are excluded
#' from further escalation.
#' This is a hard safety rule that limits further escalation based on the
#' observed data per dose level, independent from the underlying model.
#'
#' @slot target (`proportion`)\cr the target toxicity.
#' @slot prob (`proportion`)\cr the threshold probability for a dose being toxic.
#' @slot a (`number`)\cr shape parameter a>0 of probability
#'  distribution Beta (a,b).
#' @slot b (`number`)\cr shape parameter b>0 of probability
#'  distribution Beta (a,b).
#'
#' @aliases IncrementsHSRBeta
#' @export
#'
.IncrementsHSRBeta <- setClass(
  Class = "IncrementsHSRBeta",
  contains = "Increments",
  representation(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  validity = validate_increments_hsr_beta
)

# IncrementsHSRBeta-constructor ----

#' @rdname IncrementsHSRBeta-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param a (`number`)\cr see slot definition.
#' @param b (`number`)\cr see slot definition.
#'
#' @example examples/Rules-class-IncrementsHSRBeta.R
#' @export
#'
IncrementsHSRBeta <- function(target = 0.3,
                              prob = 0.95,
                              a = 1,
                              b = 1) {
  .IncrementsHSRBeta(
    target = target,
    prob = prob,
    a = a,
    b = b
  )
}

# nolint start

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
##' @example examples/Rules-class-IncrementsRelative-DataParts.R
##' @export
##' @keywords classes
.IncrementsRelativeParts <-
    setClass(Class="IncrementsRelativeParts",
             representation(dltStart="integer",
                            cleanStart="integer"),
             prototype(dltStart=-1L,
                       cleanStart=1L),
             contains="IncrementsRelative",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@dltStart),
                             "dltStart must be scalar integer")
                     o$check(is.scalar(object@cleanStart),
                             "cleanStart must be scalar integer")
                     o$check(object@cleanStart >= object@dltStart,
                             "dltStart cannot be higher than cleanStart")

                     o$result()
                 })
validObject(.IncrementsRelativeParts())


##' Initialization function for "IncrementsRelativeParts"
##'
##' @param dltStart see \code{\linkS4class{IncrementsRelativeParts}}
##' @param cleanStart see \code{\linkS4class{IncrementsRelativeParts}}
##' @param \dots additional slots from \code{\linkS4class{IncrementsRelative}}
##' @return the \code{\linkS4class{IncrementsRelativeParts}} object
##'
##' @export
##' @keywords methods
IncrementsRelativeParts <- function(dltStart,
                                    cleanStart,
                                    ...)
{
    .IncrementsRelativeParts(dltStart=safeInteger(dltStart),
                             cleanStart=safeInteger(cleanStart),
                             ...)
}


## --------------------------------------------------
## Increments control based on relative differences in terms of DLTs
## --------------------------------------------------

##' Increments control based on relative differences in terms of DLTs
##'
##' Note that \code{DLTintervals} is to be read as follows. If for example,
##' we want to specify three intervals: First 0 DLTs, second 1 or 2 DLTs, and
##' third at least 3 DLTs, then we specify
##' \code{DLTintervals} to be \code{c(0, 1, 3)}. That means, the right
##' bound of the intervals are exclusive to the interval -- the vector only
##' gives the left bounds of the intervals. The last interval goes from 3 to
##' infinity.
##'
##' @slot DLTintervals an integer vector with the left bounds of the relevant
##' DLT intervals
##' @slot increments a vector of the same length with the maximum allowable
##' relative increments in the \code{DLTintervals}
##'
##' @example examples/Rules-class-IncrementsRelativeDLT.R
##' @export
##' @keywords classes
.IncrementsRelativeDLT <-
    setClass(Class="IncrementsRelativeDLT",
             representation(DLTintervals="integer",
                            increments="numeric"),
             prototype(DLTintervals=as.integer(c(0, 1)),
                       increments=c(2, 1)),
             contains="Increments",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@increments),
                                       length(object@DLTintervals)),
                             "increments must have same length as DLTintervals")
                     o$check(! is.unsorted(object@DLTintervals, strictly=TRUE),
                             "DLTintervals has to be sorted and have unique values")
                     o$check(all(object@DLTintervals >= 0),
                             "DLTintervals must only contain non-negative integers")

                     o$result()
         })
validObject(.IncrementsRelativeDLT())


##' Initialization function for "IncrementsRelativeDLT"
##'
##' @param DLTintervals see \code{\linkS4class{IncrementsRelativeDLT}}
##' @param increments see \code{\linkS4class{IncrementsRelativeDLT}}
##' @return the \code{\linkS4class{IncrementsRelativeDLT}} object
##'
##' @export
##' @keywords methods
IncrementsRelativeDLT <- function(DLTintervals,
                                  increments)
{
    .IncrementsRelativeDLT(DLTintervals=safeInteger(DLTintervals),
                           increments=increments)
}



## -----------------------------------------------------------
## Max increment based on minimum of multiple increment rules
## -----------------------------------------------------------

##' Max increment based on minimum of multiple increment rules
##'
##' This class can be used to combine multiple increment rules with the MIN
##' operation.
##'
##' \code{IncrementsList} contains all increment rules, which are again
##' objects of class \code{\linkS4class{Increments}}. The minimum of these
##' individual increments is taken to give the final maximum increment.
##'
##' @slot IncrementsList list of increment rules
##'
##' @example examples/Rules-class-IncrementMin.R
##' @keywords classes
##' @export
.IncrementMin <-
  setClass(Class="IncrementMin",
           representation(IncrementsList="list"),
           prototype(IncrementsList=
                       list(IncrementsRelativeDLT(DLTintervals=as.integer(c(0, 1)),
                                                  increments=c(2, 1)),
                            IncrementsRelative(intervals=c(0, 2),
                                               increments=c(2, 1)))),
           contains="Increments",
           validity=
             function(object){
               o <- Validate()

               o$check(all(sapply(object@IncrementsList, is,
                                  "Increments")),
                       "all IncrementsList elements have to be Increments objects")

               o$result()
             })
validObject(.IncrementMin())


##' Initialization function for "IncrementMin"
##'
##' @param IncrementsList see \code{\linkS4class{IncrementMin}}
##' @return the \code{\linkS4class{IncrementMin}} object
##'
##' @export
##' @keywords methods
IncrementMin <- function(IncrementsList)
{
  .IncrementMin(IncrementsList=IncrementsList)
}

# Stopping-class ----

#' `Stopping`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Stopping`] is a class for for stopping rules.
#'
#' @seealso [`StoppingList`], [`StoppingCohortsNearDose`], [`StoppingPatientsNearDose`],
#'   [`StoppingMinCohorts`], [`StoppingMinPatients`], [`StoppingTargetProb`],
#'   [`StoppingMTDdistribution`], [`StoppingTargetBiomarker`], [`StoppingHighestDose`]
#'   [`StoppingMTDCV`], [`StoppingLowestDoseHSRBeta`].
#'
#' @aliases Stopping
#' @export
#'
setClass(
  Class = "Stopping",
  contains = list("VIRTUAL")
)


## --------------------------------------------------
## Stopping based on number of cohorts near to next best dose
## --------------------------------------------------

##' Stop based on number of cohorts near to next best dose
##'
##' @slot nCohorts number of required cohorts
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the cohorts must lie
##'
##' @example examples/Rules-class-StoppingCohortsNearDose.R
##' @keywords classes
##' @export
.StoppingCohortsNearDose <-
    setClass(Class="StoppingCohortsNearDose",
             representation(nCohorts="integer",
                            percentage="numeric"),
             prototype(nCohorts=2L,
                       percentage=50),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nCohorts > 0L) && is.scalar(object@nCohorts),
                         "nCohorts must be positive scalar")
                 o$check(is.probability(object@percentage / 100),
                         "percentage must be between 0 and 100")

                 o$result()
             })
validObject(.StoppingCohortsNearDose())

##' Initialization function for "StoppingCohortsNearDose"
##'
##' @param nCohorts see \code{\linkS4class{StoppingCohortsNearDose}}
##' @param percentage see \code{\linkS4class{StoppingCohortsNearDose}}
##' @return the \code{\linkS4class{StoppingCohortsNearDose}} object
##'
##' @export
##' @keywords methods
StoppingCohortsNearDose <- function(nCohorts,
                                    percentage)
{
    .StoppingCohortsNearDose(nCohorts=safeInteger(nCohorts),
                             percentage=percentage)
}
## --------------------------------------------------
## Stopping based on number of patients near to next best dose
## --------------------------------------------------

##' Stop based on number of patients near to next best dose
##'
##' @slot nPatients number of required patients
##' @slot percentage percentage (between 0 and 100) within the next best dose
##' the patients must lie
##'
##' @example examples/Rules-class-StoppingPatientsNearDose.R
##' @keywords classes
##' @export
.StoppingPatientsNearDose <-
    setClass(Class="StoppingPatientsNearDose",
             representation(nPatients="integer",
                            percentage="numeric"),
             prototype(nPatients=10L,
                       percentage=50),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nPatients > 0L) && is.scalar(object@nPatients),
                         "nPatients must be positive scalar")
                 o$check(is.probability(object@percentage / 100),
                         "percentage must be between 0 and 100")

                 o$result()
             })
validObject(.StoppingPatientsNearDose())


##' Initialization function for "StoppingPatientsNearDose"
##'
##' @param nPatients see \code{\linkS4class{StoppingPatientsNearDose}}
##' @param percentage see \code{\linkS4class{StoppingPatientsNearDose}}
##' @return the \code{\linkS4class{StoppingPatientsNearDose}} object
##'
##' @export
##' @keywords methods
StoppingPatientsNearDose <- function(nPatients,
                                     percentage)
{
    .StoppingPatientsNearDose(nPatients=safeInteger(nPatients),
                              percentage=percentage)
}


## --------------------------------------------------
## Stopping based on minimum number of cohorts
## --------------------------------------------------

##' Stop based on minimum number of cohorts
##'
##' @slot nCohorts minimum required number of cohorts
##'
##' @example examples/Rules-class-StoppingMinCohorts.R
##' @keywords classes
##' @export
.StoppingMinCohorts <-
    setClass(Class="StoppingMinCohorts",
             representation(nCohorts="integer"),
             prototype(nCohorts=3L),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nCohorts > 0L) && is.scalar(object@nCohorts),
                         "nCohorts must be positive scalar")

                 o$result()
             })
validObject(.StoppingMinCohorts())



##' Initialization function for "StoppingMinCohorts"
##'
##' @param nCohorts see \code{\linkS4class{StoppingMinCohorts}}
##' @return the \code{\linkS4class{StoppingMinCohorts}} object
##'
##' @export
##' @keywords methods
StoppingMinCohorts <- function(nCohorts)
{
    .StoppingMinCohorts(nCohorts=safeInteger(nCohorts))
}


## --------------------------------------------------
## Stopping based on minimum number of patients
## --------------------------------------------------

##' Stop based on minimum number of patients
##'
##' @slot nPatients minimum allowed number of patients
##'
##' @example examples/Rules-class-StoppingMinPatients.R
##' @keywords classes
##' @export
.StoppingMinPatients <-
    setClass(Class="StoppingMinPatients",
             representation(nPatients="integer"),
             prototype(nPatients=20L),
             contains="Stopping",
             validity=function(object){
                 o <- Validate()

                 o$check((object@nPatients > 0L) && is.scalar(object@nPatients),
                         "nPatients must be positive scalar")

                 o$result()
             })
validObject(.StoppingMinPatients())

##' Initialization function for "StoppingMinPatients"
##'
##' @param nPatients see \code{\linkS4class{StoppingMinPatients}}
##' @return the \code{\linkS4class{StoppingMinPatients}} object
##'
##' @export
##' @keywords methods
StoppingMinPatients <- function(nPatients)
{
    .StoppingMinPatients(nPatients=safeInteger(nPatients))
}


## --------------------------------------------------
## Stopping based on probability of target tox interval
## --------------------------------------------------

##' Stop based on probability of target tox interval
##'
##' @slot target the target toxicity interval, e.g. \code{c(0.2, 0.35)}
##' @slot prob required target toxicity probability (e.g. \code{0.4})
##' for reaching sufficient precision
##'
##' @example examples/Rules-class-StoppingTargetProb.R
##' @keywords classes
##' @export
.StoppingTargetProb <-
    setClass(Class="StoppingTargetProb",
             representation(target="numeric",
                            prob="numeric"),
             prototype(target=c(0.2, 0.35),
                       prob=0.4),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probRange(object@target),
                             "target must be probability range")
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingTargetProb())


##' Initialization function for "StoppingTargetProb"
##'
##' @param target see \code{\linkS4class{StoppingTargetProb}}
##' @param prob see \code{\linkS4class{StoppingTargetProb}}
##' @return the \code{\linkS4class{StoppingTargetProb}} object
##'
##' @export
##' @keywords methods
StoppingTargetProb <- function(target,
                               prob)
{
    .StoppingTargetProb(target=target,
                        prob=prob)
}


## --------------------------------------------------
## Stopping based on MTD distribution
## --------------------------------------------------

##' Stop based on MTD distribution
##'
##' Has 90% probability above a threshold of 50% of the current
##' MTD been reached? This class is used for this question.
##'
##' @slot target the target toxicity probability (e.g. 0.33) defining the MTD
##' @slot thresh the threshold relative to the MTD (e.g. 0.5)
##' @slot prob required probability (e.g. 0.9)
##'
##' @example examples/Rules-class-StoppingMTDdistribution.R
##' @keywords classes
##' @export
.StoppingMTDdistribution <-
    setClass(Class="StoppingMTDdistribution",
             representation(target="numeric",
                            thresh="numeric",
                            prob="numeric"),
             prototype(target=0.33,
                       thresh=0.5,
                       prob=0.9),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.probability(object@target,
                                            bounds=FALSE),
                             "target must be probability > 0 and < 1")
                     o$check(is.probability(object@thresh,
                                            bounds=FALSE),
                             "thresh must be probability > 0 and < 1")
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingMTDdistribution())


##' Initialization function for "StoppingMTDdistribution"
##'
##' @param target see \code{\linkS4class{StoppingMTDdistribution}}
##' @param thresh see \code{\linkS4class{StoppingMTDdistribution}}
##' @param prob see \code{\linkS4class{StoppingMTDdistribution}}
##' @return the \code{\linkS4class{StoppingMTDdistribution}} object
##'
##' @export
##' @keywords methods
StoppingMTDdistribution <- function(target,
                                    thresh,
                                    prob)
{
    .StoppingMTDdistribution(target=target,
                             thresh=thresh,
                             prob=prob)
}

# nolint end

# StoppingMTDCV-class ----

#' `StoppingMTDCV`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingMTDCV`] is a class for stopping rule based on precision of MTD
#' which is calculated as the coefficient of variation (CV) of the MTD.
#'
#' @slot target (`proportion`)\cr toxicity target of MTD.
#' @slot thresh_cv (`number`)\cr threshold for CV to be considered accurate enough
#'   to stop the trial.
#'
#' @aliases StoppingMTDCV
#' @export
#'
.StoppingMTDCV <- setClass(
  Class = "StoppingMTDCV",
  contains = "Stopping",
  representation = representation(
    target = "numeric",
    thresh_cv = "numeric"
  ),
  prototype(
    target = 0.3,
    thresh_cv = 40
  ),
  validity = validate_stopping_mtd_cv
)

# StoppingMTDCV-constructor ----

#' @rdname StoppingMTDCV-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param thresh_cv (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingMTDCV.R
#'
StoppingMTDCV <- function(target = 0.3,
                          thresh_cv = 40) {
  .StoppingMTDCV(
    target = target,
    thresh_cv = thresh_cv
  )
}


# StoppingLowestDoseHSRBeta-class ----

#' `StoppingLowestDoseHSRBeta`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StoppingLowestDoseHSRBeta`] is a class for stopping based on a Hard Safety
#' Rule using the Beta posterior distribution with Beta(a,b) prior and a
#' Bin-Beta model based on the observed data at the lowest dose level.
#' The rule is triggered when the first dose is considered to be toxic
#' (i.e. above threshold probability) based on the observed data at the
#' lowest dose level and a Beta(a,b) prior distribution.
#' The default prior is Beta(1,1).
#' In case that placebo is used, the rule is evaluated at the second dose of the
#' dose grid, i.e. at the lowest non-placebo dose.
#' Note: this stopping rule is independent from the underlying model.
#'
#' @slot target (`proportion`)\cr the target toxicity.
#' @slot prob (`proportion`)\cr the threshold probability for the lowest
#'  dose being toxic.
#' @slot a (`number`)\cr shape parameter a>0 of probability
#'  distribution Beta (a,b).
#' @slot b (`number`)\cr shape parameter b>0 of probability
#'  distribution Beta (a,b).
#'
#' @aliases StoppingLowestDoseHSRBeta
#' @export
#'
.StoppingLowestDoseHSRBeta <- setClass(
  Class = "StoppingLowestDoseHSRBeta",
  contains = "Stopping",
  representation = representation(
    target = "numeric",
    prob = "numeric",
    a = "numeric",
    b = "numeric"
  ),
  prototype(
    target = 0.3,
    prob = 0.95,
    a = 1,
    b = 1
  ),
  validity = validate_stopping_lowest_dose_hsr_beta
)


# StoppingLowestDoseHSRBeta-constructor ----

#' @rdname StoppingLowestDoseHSRBeta-class
#'
#' @param target (`proportion`)\cr see slot definition.
#' @param prob (`proportion`)\cr see slot definition.
#' @param a (`number`)\cr see slot definition.
#' @param b (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Rules-class-StoppingLowestDoseHSRBeta.R
#'
StoppingLowestDoseHSRBeta <- function(target = 0.3,
                                      prob = 0.95,
                                      a = 1,
                                      b = 1) {
  .StoppingLowestDoseHSRBeta(
    target = target,
    prob = prob,
    a = a,
    b = b
  )
}

# nolint start

## --------------------------------------------------
## Stopping based on probability of target biomarker
## --------------------------------------------------

##' Stop based on probability of target biomarker
##'
##' @slot target the biomarker target range, that
##' needs to be reached. For example, (0.8, 1.0) and \code{scale="relative"}
##' means we target a dose with at least 80% of maximum biomarker level.
##' @slot scale either \code{relative} (default, then the \code{target} is interpreted
##' relative to the maximum, so must be a probability range) or \code{absolute}
##' (then the \code{target} is interpreted as absolute biomarker range)
##' @slot prob required target probability for reaching sufficient precision
##'
##' @example examples/Rules-class-StoppingTargetBiomarker.R
##'
##' @export
.StoppingTargetBiomarker <-
    setClass(Class="StoppingTargetBiomarker",
             representation(target="numeric",
                            scale="character",
                            prob="numeric"),
             prototype(target=c(0.9, 1),
                       scale="relative",
                       prob=0.3),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@scale) && object@scale %in% c("relative", "absolute"),
                             "scale must be either 'relative' or 'absolute'")
                     if(object@scale == "relative")
                     {
                       o$check(is.probRange(object@target),
                               "target has to be a probability range when scale='relative'")
                     } else {
                       o$check(is.range(object@target),
                               "target must be a numeric range")
                     }
                     o$check(is.probability(object@prob,
                                            bounds=FALSE),
                             "prob must be probability > 0 and < 1")

                     o$result()
                 })
validObject(.StoppingTargetBiomarker())


##' Initialization function for `StoppingTargetBiomarker`
##'
##' @param target see \code{\linkS4class{StoppingTargetBiomarker}}
##' @param scale see \code{\linkS4class{StoppingTargetBiomarker}}
##' @param prob see \code{\linkS4class{StoppingTargetBiomarker}}
##' @return the \code{\linkS4class{StoppingTargetBiomarker}} object
##'
##' @export
StoppingTargetBiomarker <- function(target,
                                    scale=c("relative", "absolute"),
                                    prob)
{
  scale <- match.arg(scale)
    .StoppingTargetBiomarker(target=target,
                             scale=scale,
                             prob=prob)
}

## --------------------------------------------------
## Stopping when the highest dose is reached
## --------------------------------------------------

##' Stop when the highest dose is reached
##'
##' @example examples/Rules-class-StoppingHighestDose.R
##' @keywords classes
##' @export
.StoppingHighestDose <-
  setClass(Class="StoppingHighestDose",
           contains="Stopping")
validObject(.StoppingHighestDose())

##' Initialization function for "StoppingHighestDose"
##'
##' @return the \code{\linkS4class{StoppingHighestDose}} object
##'
##' @export
##' @keywords methods
StoppingHighestDose <- function()
{
  .StoppingHighestDose()
}


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
##' @slot summary the summary function to combine the results of the stopping
##' rules into a single result
##'
##' @example examples/Rules-class-StoppingList.R
##' @keywords classes
##' @export
.StoppingList <-
    setClass(Class="StoppingList",
             representation(stopList="list",
                            summary="function"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5)),
                       summary=all),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")
                     testRes <- object@summary(rep(c(TRUE, FALSE),
                                                   length.out=length(object@stopList)))
                     o$check(is.bool(testRes),
                             "summary function must return a boolean value")

                     o$result()
                 })
validObject(.StoppingList())


##' Initialization function for "StoppingList"
##'
##' @param stopList see \code{\linkS4class{StoppingList}}
##' @param summary see \code{\linkS4class{StoppingList}}
##' @return the \code{\linkS4class{StoppingList}} object
##'
##' @export
##' @keywords methods
StoppingList <- function(stopList,
                         summary)
{
    .StoppingList(stopList=stopList,
                  summary=summary)
}


## --------------------------------------------------
## Stopping based on fulfillment of all multiple stopping rules
## --------------------------------------------------

##' Stop based on fulfillment of all multiple stopping rules
##'
##' This class can be used to combine multiple stopping rules with an AND
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. All stopping rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##'
##' @example examples/Rules-class-StoppingAll.R
##' @keywords classes
##' @export
.StoppingAll <-
    setClass(Class="StoppingAll",
             representation(stopList="list"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5))),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")

                     o$result()
                 })
validObject(.StoppingAll())


##' Initialization function for "StoppingAll"
##'
##' @param stopList see \code{\linkS4class{StoppingAll}}
##' @return the \code{\linkS4class{StoppingAll}} object
##'
##' @export
##' @keywords methods
StoppingAll <- function(stopList)
{
    .StoppingAll(stopList=stopList)
}


## --------------------------------------------------
## Stopping based on fulfillment of any stopping rule
## --------------------------------------------------

##' Stop based on fulfillment of any stopping rule
##'
##' This class can be used to combine multiple stopping rules with an OR
##' operator.
##'
##' \code{stopList} contains all stopping rules, which are again objects of
##' class \code{\linkS4class{Stopping}}. Any of these rules must be fulfilled in
##' order that the result of this rule is to stop.
##'
##' @slot stopList list of stopping rules
##'
##' @example examples/Rules-class-StoppingAny.R
##' @keywords classes
##' @export
.StoppingAny <-
    setClass(Class="StoppingAny",
             representation(stopList="list"),
             prototype(stopList=
                           list(StoppingMinPatients(50),
                                StoppingMinCohorts(5))),
             contains="Stopping",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@stopList, is, "Stopping")),
                             "all stopList elements have to Stopping objects")

                     o$result()
                 })
validObject(.StoppingAny())


##' Initialization function for "StoppingAny"
##'
##' @param stopList see \code{\linkS4class{StoppingAny}}
##' @return the \code{\linkS4class{StoppingAny}} object
##'
##' @export
##' @keywords methods
StoppingAny <- function(stopList)
{
    .StoppingAny(stopList=stopList)
}


##-------------------------------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the 95% credibility interval
## ---------------------------------------------------------------------------------------------------------------

##' Stop based on a target ratio, the ratio of the upper to the lower
##' 95% credibility interval of the estimate of TD end of trial, the dose with probability of DLE equals to the target
##' probability of DLE used at the end of a trial
##' @slot targetRatio the target ratio of the upper to the lower of the 95% credibility interval of the
##' estimate that required to stop a trial
##' @slot targetEndOfTrial the target probability of DLE to be used at the end of a trial
##'
##' @example examples/Rules-class-StoppingTDCIRatio.R
##' @export
##' @keywords classes
.StoppingTDCIRatio <-
  setClass(Class="StoppingTDCIRatio",
           representation(targetRatio="numeric",
                          targetEndOfTrial="numeric"),
           prototype(targetRatio=5,
                     targetEndOfTrial=0.3),
           contains="Stopping",
           validity=
             function(object){
               o <- Validate()

               o$check(is.numeric(object@targetRatio) & object@targetRatio > 0,
                       "targetRatio must be a positive numerical number")
               o$check(is.numeric(object@targetEndOfTrial) & object@targetEndOfTrial >= 0 & object@targetEndOfTrial <= 1,
                       "targetEndOfTrial must be a numerical number lies between 0 and 1")
               o$result()
             })

validObject(.StoppingTDCIRatio())

##' Initialization function for "StoppingTDCIRatio"
##'
##' @param targetRatio please refer to \code{\linkS4class{StoppingTDCIRatio}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{StoppingTDCIRatio}} class object
##' @return the \code{\linkS4class{StoppingTDCIRatio}} class object
##'
##' @export
##' @keywords methods
StoppingTDCIRatio <- function(targetRatio,
                              targetEndOfTrial)
{
  .StoppingTDCIRatio(targetRatio=targetRatio,
                     targetEndOfTrial=targetEndOfTrial)
}

## ----------------------------------------------------------------------------------------------------------------
##' Stop based on a target ratio, the ratio of the upper to the lower
##' 95% credibility interval of the estimate of the minimum of the dose which gives the maximum gain (Gstar) and
##' the TD end of trial, the dose with probability of DLE equals to the target
##' probability of DLE used at the end of a trial.
##' @slot targetRatio the target ratio of the upper to the lower of the 95% credibility interval of the
##' estimate that required to stop a trial
##' @slot targetEndOfTrial the target probability of DLE to be used at the end of a trial
##'
##' @example examples/Rules-class-StoppingGstarCIRatio.R
##' @export
##' @keywords classes
.StoppingGstarCIRatio <-
  setClass(Class="StoppingGstarCIRatio",
           representation(targetRatio="numeric",
                          targetEndOfTrial="numeric"),
           prototype(targetRatio=5,
                     targetEndOfTrial=0.3),
           contains="Stopping",
           validity=
             function(object){
               o <- Validate()

               o$check(is.numeric(object@targetRatio) & object@targetRatio > 0,
                       "targetRatio must be a positive numerical number")
               o$check(is.numeric(object@targetEndOfTrial) & object@targetEndOfTrial >= 0 & object@targetEndOfTrial <= 1,
                       "targetEndOfTrial must be a numerical number lies between 0 and 1")
               o$result()
             })

validObject(.StoppingGstarCIRatio())

##' Initialization function for "StoppingGstarCIRatio"
##'
##' @param targetRatio please refer to \code{\linkS4class{StoppingGstarCIRatio}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{StoppingGstarCIRatio}} class object
##' @return the \code{\linkS4class{StoppingGstarCIRatio}} class object
##'
##' @export
##' @keywords methods
StoppingGstarCIRatio <- function(targetRatio,
                                 targetEndOfTrial)
{
  .StoppingGstarCIRatio(targetRatio=targetRatio,
                        targetEndOfTrial=targetEndOfTrial)
}



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
## Cohort size based on dose range
## --------------------------------------------------

##' Cohort size based on dose range
##'
##' @slot intervals a vector with the left bounds of the relevant dose intervals
##' @slot cohortSize an integer vector of the same length with the cohort
##' sizes in the \code{intervals}
##'
##' @example examples/Rules-class-CohortSizeRange.R
##' @export
##' @keywords classes
.CohortSizeRange <-
    setClass(Class="CohortSizeRange",
             representation(intervals="numeric",
                            cohortSize="integer"),
             prototype(intervals=c(0, 20),
                       cohortSize=as.integer(c(1L, 3L))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@cohortSize),
                                       length(object@intervals)),
                             "cohortSize must have same length as intervals")
                     o$check(all(object@cohortSize >= 0),
                             "cohortSize must only contain positive integers")
                     o$check(! is.unsorted(object@intervals, strictly=TRUE),
                             "intervals has to be sorted and have unique values")

                     o$result()
                 })
validObject(.CohortSizeRange())

##' Initialization function for "CohortSizeRange"
##'
##' @param intervals see \code{\linkS4class{CohortSizeRange}}
##' @param cohortSize see \code{\linkS4class{CohortSizeRange}}
##' @return the \code{\linkS4class{CohortSizeRange}} object
##'
##' @export
##' @keywords methods
CohortSizeRange <- function(intervals,
                            cohortSize)
{
    .CohortSizeRange(intervals=intervals,
                     cohortSize=safeInteger(cohortSize))
}

## --------------------------------------------------
## Cohort size based on number of DLTs
## --------------------------------------------------

##' Cohort size based on number of DLTs
##'
##' @slot DLTintervals an integer vector with the left bounds of the relevant
##' DLT intervals
##' @slot cohortSize an integer vector of the same length with the cohort
##' sizes in the \code{DLTintervals}
##'
##' @example examples/Rules-class-CohortSizeDLT.R
##' @export
##' @keywords classes
.CohortSizeDLT <-
    setClass(Class="CohortSizeDLT",
             representation(DLTintervals="integer",
                            cohortSize="integer"),
             prototype(DLTintervals=as.integer(c(0, 1)),
                       cohortSize=as.integer(c(1, 3))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(identical(length(object@cohortSize),
                                       length(object@DLTintervals)),
                             "cohortSize must have same length as DLTintervals")
                     o$check(all(object@cohortSize >= 0),
                             "cohortSize must only contain positive integers")
                     o$check(! is.unsorted(object@DLTintervals, strictly=TRUE),
                             "DLTintervals has to be sorted and have unique values")
                     o$check(all(object@DLTintervals >= 0),
                             "DLTintervals must only contain non-negative integers")

                     o$result()
                 })
validObject(.CohortSizeDLT())

##' Initialization function for "CohortSizeDLT"
##'
##' @param DLTintervals see \code{\linkS4class{CohortSizeDLT}}
##' @param cohortSize see \code{\linkS4class{CohortSizeDLT}}
##' @return the \code{\linkS4class{CohortSizeDLT}} object
##'
##' @export
##' @keywords methods
CohortSizeDLT <- function(DLTintervals,
                          cohortSize)
{
    .CohortSizeDLT(DLTintervals=safeInteger(DLTintervals),
                   cohortSize=safeInteger(cohortSize))
}


## --------------------------------------------------
## Constant cohort size
## --------------------------------------------------

##' Constant cohort size
##'
##' This class is used when the cohort size should be kept constant.
##'
##' @slot size the constant integer size
##'
##' @example examples/Rules-class-CohortSizeConst.R
##' @keywords classes
##' @export
.CohortSizeConst <-
    setClass(Class="CohortSizeConst",
             representation(size="integer"),
             prototype(size=3L),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(is.scalar(object@size) && (object@size >= 0),
                             "size needs to be positive scalar")

                     o$result()
                 })
validObject(.CohortSizeConst())

##' Initialization function for "CohortSizeConst"
##'
##' @param size see \code{\linkS4class{CohortSizeConst}}
##' @return the \code{\linkS4class{CohortSizeConst}} object
##'
##' @export
##' @keywords methods
CohortSizeConst <- function(size)
{
    .CohortSizeConst(size=safeInteger(size))
}



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
##' @example examples/Rules-class-CohortSizeParts.R
##' @export
.CohortSizeParts <-
    setClass(Class="CohortSizeParts",
             representation(sizes="integer"),
             prototype(sizes=as.integer(c(1, 3))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(object@sizes > 0),
                             "the cohort sizes need to be positive")
                     o$check(identical(length(object@sizes), 2L),
                             "2 elements required in sizes")

                     o$result()
                 })
validObject(.CohortSizeParts())

##' Initialization function for "CohortSizeParts"
##'
##' @param sizes see \code{\linkS4class{CohortSizeParts}}
##' @return the \code{\linkS4class{CohortSizeParts}} object
##' @export
##'
##' @keywords methods
CohortSizeParts <- function(sizes)
{
    .CohortSizeParts(sizes=safeInteger(sizes))
}


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
##' @example examples/Rules-class-CohortSizeMax.R
##' @keywords classes
##' @export
.CohortSizeMax <-
    setClass(Class="CohortSizeMax",
             representation(cohortSizeList="list"),
             prototype(cohortSizeList=
                           list(CohortSizeRange(intervals=c(0, 30),
                                                cohortSize=c(1, 3)),
                                CohortSizeDLT(DLTintervals=c(0, 1),
                                              cohortSize=c(1, 3)))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@cohortSizeList, is,
                                        "CohortSize")),
                             "all cohortSizeList elements have to be CohortSize objects")

                     o$result()
                 })
validObject(.CohortSizeMax())


##' Initialization function for "CohortSizeMax"
##'
##' @param cohortSizeList see \code{\linkS4class{CohortSizeMax}}
##' @return the \code{\linkS4class{CohortSizeMax}} object
##'
##' @export
##' @keywords methods
CohortSizeMax <- function(cohortSizeList)
{
    .CohortSizeMax(cohortSizeList=cohortSizeList)
}


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
##' @example examples/Rules-class-CohortSizeMin.R
##' @keywords classes
##' @export
.CohortSizeMin <-
    setClass(Class="CohortSizeMin",
             representation(cohortSizeList="list"),
             prototype(cohortSizeList=
                           list(CohortSizeRange(intervals=c(0, 30),
                                                cohortSize=c(1, 3)),
                                CohortSizeDLT(DLTintervals=c(0, 1),
                                              cohortSize=c(1, 3)))),
             contains="CohortSize",
             validity=
                 function(object){
                     o <- Validate()

                     o$check(all(sapply(object@cohortSizeList, is,
                                        "CohortSize")),
                             "all cohortSizeList elements have to be CohortSize objects")

                     o$result()
                 })
validObject(.CohortSizeMin())


##' Initialization function for "CohortSizeMin"
##'
##' @param cohortSizeList see \code{\linkS4class{CohortSizeMin}}
##' @return the \code{\linkS4class{CohortSizeMin}} object
##'
##' @export
##' @keywords methods
CohortSizeMin <- function(cohortSizeList)
{
    .CohortSizeMin(cohortSizeList=cohortSizeList)
}



## ==========================================================================================
## ------------------------------------------------------------------------------------
## Class for next best based on Pseudo DLE Model with samples
## -----------------------------------------------------------------------------------------

##' Next best dose based on Pseudo DLE Model with samples
##'
##' The class is to find the next best dose for allocation and the dose for final recommendation
##' at the end of a trial. There are two input target probabilities of the occurrence of a DLE
##' used during trial and used at the end of trial to find the two doses. For this class, only
##' DLE response will be incorporated for the dose allocation and DLEsamples
##' must be used to obtain the next dose for allocation.
##'
##' @slot targetDuringTrial the target probability of the occurrence of a DLE to be used
##' during the trial
##' @slot targetEndOfTrial the target probability of the occurrence of a DLE to be used at the end
##' of the trial. This target is particularly used to recommend the dose at the end of a trial
##' for which its posterior
##' probability of the occurrence of a DLE is equal to this target
##' @slot derive the function which derives from the input, a vector of the posterior samples called
##' \code{TDsamples} of the dose
##' which has the probability of the occurrence of DLE equals to either the targetDuringTrial or
##' targetEndOfTrial, the final next best TDtargetDuringTrial (the dose with probability of the
##' occurrence of DLE equals to the targetDuringTrial)and TDtargetEndOfTrial estimate.
##'
##' @example examples/Rules-class-NextBestTDsamples.R
##' @export
##' @keywords class
.NextBestTDsamples<-
  setClass(Class="NextBestTDsamples",
           representation(targetDuringTrial="numeric",
                          targetEndOfTrial="numeric",
                          derive="function"),
           ##targetDuringTrial is the target DLE probability during the trial
           ##targetEndOfTrial is the target DLE probability at the End of the trial
           prototype(targetDuringTrial=0.35,
                     targetEndOfTrial=0.3,
                     derive=function(TDsamples){
                       quantile(TDsamples,prob=0.3)}),
           contains=list("NextBest"),
           validity=
             function(object){
               o<-Validate()
               o$check(is.probability(object@targetDuringTrial,
                                      bounds=FALSE),
                       "targetDuringTrial must be probability > 0 and < 1")
               o$check(is.probability(object@targetEndOfTrial,
                                      bounds=FALSE),
                       "targetEndOfTrial must be probability > 0 and < 1")
               o$check(identical(names(formals(object@derive)),
                                 c("TDsamples")),"derive must have as single argument 'TDsamples'")

               o$result()
             })
validObject(.NextBestTDsamples())

## ---------------------------------------------------------------------------
##' Initialization function for class "NextBestTDsamples"
##'
##' @param targetDuringTrial please refer to \code{\linkS4class{NextBestTDsamples}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{NextBestTDsamples}} class object
##' @param derive please refer to \code{\linkS4class{NextBestTDsamples}} class object
##' @return the \code{\linkS4class{NextBestTDsamples}} class object
##'
##' @export
##' @keywords methods
NextBestTDsamples<- function(targetDuringTrial,targetEndOfTrial,derive)
{
  .NextBestTDsamples(targetDuringTrial=targetDuringTrial,
                     targetEndOfTrial=targetEndOfTrial,
                     derive=derive)
}

## ------------------------------------------------------------------------------
## class for nextBest based on Pseudo DLE model without sample
## -----------------------------------------------------------------------------

##' Next best dose based on Pseudo DLE model without sample
##'
##' The class is to find the next best dose for allocation and the dose for final recommendation
##' at the end of a trial without involving any samples. This is a class for which only
##'  DLE response will be incorporated for the dose-allocation.
##' This is only based on the probabilities of
##' the occurrence of a DLE obtained by using the modal estimates of the model paramters.
##' There are two inputs inputs which are the two target
##' probabilities of the occurrence of a DLE used during trial
##' and used at the end of trial, for finding the next best dose for allocation and the dose
##' for recommendation at the end of the trial.
##' It is only suitable to use with the model specified in \code{ModelTox} class.
##'
##' @slot targetDuringTrial the target probability of the occurrence of a DLE to be used
##' during the trial
##' @slot targetEndOfTrial the target probability of the occurrence of a DLE to be used at the end
##' of the trial. This target is particularly used to recommend the dose for which its posterior
##' probability of the occurrence of a DLE is equal to this target
##'
##' @example examples/Rules-class-NextBestTD.R
##' @export
##' @keywords class
.NextBestTD<-
  setClass(Class="NextBestTD",
           representation(targetDuringTrial="numeric",
                          targetEndOfTrial="numeric"),
           ##targetDuringTrial is the target DLE probability during the trial
           ##targetEndOfTrial is the target DLE probability at the End of the trial
           prototype(targetDuringTrial=0.35,
                     targetEndOfTrial=0.3),
           contains=list("NextBest"),
           validity=
             function(object){
               o<-Validate()
               o$check(is.probability(object@targetDuringTrial,
                                      bounds=FALSE),
                       "targetDuringTrial must be probability > 0 and < 1")
               o$check(is.probability(object@targetEndOfTrial,
                                      bounds=FALSE),
                       "targetEndOfTrial must be probability > 0 and < 1")
               o$result()
             })
validObject(.NextBestTD())

##' Initialization function for the class "NextBestTD"
##'
##' @param targetDuringTrial please refer to \code{\linkS4class{NextBestTD}} class object
##' @param targetEndOfTrial please refer to \code{\linkS4class{NextBestTD}} class object
##' @return the \code{\linkS4class{NextBestTD}} class object
##'
##' @export
##' @keywords methods
NextBestTD <- function(targetDuringTrial,targetEndOfTrial)
{
  .NextBestTD(targetDuringTrial=targetDuringTrial,
              targetEndOfTrial=targetEndOfTrial)
}

##------------------------------------------------------------------------------------------------------
## Class for next best with maximum gain value based on a pseudo DLE and efficacy model without samples
## ----------------------------------------------------------------------------------------------------
##' Next best dose with maximum gain value based on a pseudo DLE and efficacy model without samples
##'
##' This is a class for which to find the next dose which is safe and give the maximum gain value
##' for allocation. This is a class where no DLE and efficacy samples are involved. This is only based
##' on the probabilities of the occurrence of a DLE and the values of the mean efficacy responses
##' obtained by using the modal estimates of the DLE and efficacy model parameters.
##' There are two inputs which are the two target
##' probabilities of the occurrence of a DLE used during trial
##' and used at the end of trial, for finding the next best dose that is safe and gives the maximum
##' gain value and the dose to recommend at the end of a trial. This is only suitable to use with DLE models
##' specified in 'ModelTox' class and efficacy models  specified in 'ModelEff' (except 'EffFlexi' model)
##' class
##'
##' @slot DLEDuringTrialtarget the target probability of the occurrence of a DLE to be used
##' during the trial
##' @slot DLEEndOfTrialtarget the target probability of the occurrence of a DLE to be used at the end
##' of the trial. This target is particularly used to recommend the dose for which its posterior
##' probability of the occurrence of a DLE is equal to this target
##'
##' @example examples/Rules-class-NextBestMaxGain.R
##' @export
##' @keywords class
.NextBestMaxGain<-
  setClass(Class="NextBestMaxGain",
           representation(DLEDuringTrialtarget="numeric",
                          DLEEndOfTrialtarget="numeric"),
           prototype(DLEDuringTrialtarget=0.35,
                     DLEEndOfTrialtarget=0.3),
           contains=list("NextBest"),
           validity=
             function(object){
               o <- Validate()
               o$check(is.probability(object@DLEDuringTrialtarget),
                       "DLE DuringTrialtarget has to be a probability")
               o$check(is.probability(object@DLEEndOfTrialtarget),
                       "DLE EndOfTrialtarget has to be a probability")
               o$result()
             })
validObject(.NextBestMaxGain())

##' Initialization function for the class 'NextBestMaxGain'
##'
##' @param DLEDuringTrialtarget please refer to \code{\linkS4class{NextBestMaxGain}} class object
##' @param DLEEndOfTrialtarget please refer to \code{\linkS4class{NextBestMaxGain}} class object
##' @return the \code{\linkS4class{NextBestMaxGain}} class object
##'
##' @export
##' @keywords methods
NextBestMaxGain <- function(DLEDuringTrialtarget,
                            DLEEndOfTrialtarget)
{.NextBestMaxGain(DLEDuringTrialtarget=DLEDuringTrialtarget,
                  DLEEndOfTrialtarget=DLEEndOfTrialtarget)}

##------------------------------------------------------------------------------------------------------
## Class for next best with maximum gain value based on a pseudo DLE and efficacy model with samples
## ----------------------------------------------------------------------------------------------------
##' Next best dose with maximum gain value based on a pseudo DLE and efficacy model with samples
##'
##' This is a class for which to find the next dose which is safe and give the maximum gain value
##' for allocation. This is a class where DLE and efficacy samples are involved.
##' There are two inputs which are the two target
##' probabilities of the occurrence of a DLE used during trial
##' and used at the end of trial, for finding the next best dose that is safe and gives the maximum
##' gain value and the dose to recommend at the end of a trial. This is only suitable to use with DLE models
##' specified in 'ModelTox' class and efficacy models  specified in 'ModelEff' class
##' class
##'
##' @slot DLEDuringTrialtarget the target probability of the occurrence of a DLE to be used
##' during the trial
##' @slot DLEEndOfTrialtarget the target probability of the occurrence of a DLE to be used at the end
##' of the trial. This target is particularly used to recommend the dose for which its posterior
##' probability of the occurrence of a DLE is equal to this target
##' @slot TDderive the function which derives from the input, a vector of the posterior samples called
##' \code{TDsamples} of the dose
##' which has the probability of the occurrence of DLE equals to either the targetDuringTrial or
##' targetEndOfTrial, the final next best TDtargetDuringTrial (the dose with probability of the
##' occurrence of DLE equals to the targetDuringTrial)and TDtargetEndOfTrial estimate.
##' @slot Gstarderive the function which derives from the input, a vector of the posterior Gstar (the dose
##' which gives the maximum gain value) samples
##' called \code{Gstarsamples}, the final next best Gstar estimate.
##'
##' @example examples/Rules-class-NextBestMaxGainSamples.R
##'
##' @export
##' @keywords class
.NextBestMaxGainSamples<-
  setClass(Class="NextBestMaxGainSamples",
           representation(DLEDuringTrialtarget="numeric",
                          DLEEndOfTrialtarget="numeric",
                          TDderive="function",
                          Gstarderive="function"),
           prototype(DLEDuringTrialtarget=0.35,
                     DLEEndOfTrialtarget=0.3,
                     TDderive=function(TDsamples){
                       quantile(TDsamples,prob=0.3)},
                     Gstarderive=function(Gstarsamples){
                       quantile(Gstarsamples,prob=0.5)}),
           contains=list("NextBest"),
           validity=
             function(object){
               o <- Validate()
               o$check(is.probability(object@DLEDuringTrialtarget),
                       "DLE DuringTrialtarget has to be a probability")
               o$check(is.probability(object@DLEEndOfTrialtarget),
                       "DLE EndOfTrialtarget has to be a probability")
               o$check(identical(names(formals(object@TDderive)),
                                 c("TDsamples")),"derive must have as single argument 'TDsamples'")
               o$check(identical(names(formals(object@Gstarderive)),
                                 c("Gstarsamples")),"derive must have as single argument 'Gstarsamples'")

               o$result()
             })
validObject(.NextBestMaxGainSamples)

##' Initialization function for class "NextBestMaxGainSamples"
##'
##' @param DLEDuringTrialtarget please refer to \code{\linkS4class{NextBestMaxGainSamples}} class object
##' @param DLEEndOfTrialtarget please refer to \code{\linkS4class{NextBestMaxGainSamples}} class object
##' @param TDderive please refer to \code{\linkS4class{NextBestMaxGainSamples}} class object
##' @param Gstarderive please refer to \code{\linkS4class{NextBestMaxGainSamples}} class object
##'
##' @return the \code{\linkS4class{NextBestMaxGainSamples}} class object
##'
##' @export
##' @keywords methods
NextBestMaxGainSamples <- function(DLEDuringTrialtarget,
                                   DLEEndOfTrialtarget,TDderive,Gstarderive)
{.NextBestMaxGainSamples(DLEDuringTrialtarget=DLEDuringTrialtarget,
                         DLEEndOfTrialtarget=DLEEndOfTrialtarget,
                         TDderive=TDderive,
                         Gstarderive=Gstarderive)
}


## ============================================================

## --------------------------------------------------
## Virtual class for safety window
## --------------------------------------------------

##' The virtual class for safety window
##'
##' @seealso \code{\linkS4class{SafetyWindowSize}},
##' \code{\linkS4class{SafetyWindowConst}},
##'
##' @export
##' @keywords classes
setClass(Class="SafetyWindow",
         contains=list("VIRTUAL"))


## ============================================================


## --------------------------------------------------
## Safety window length based on cohort size
## --------------------------------------------------

##' Safety window length based on cohort size.
##' This class is used to decide the rolling rule from the clinical perspective.
##'
##' \code{patientGap} is to be used as follows. If for example, the
##' cohort size is 4 and we want to specify three time intervals between these
##' four patients: The interval between the 1st and 2nd patient = 7 units of time
##' , the interval between the 2nd and 3rd patient = 5 units of time, the interval
##' between the 3rd and 4th patient = 3 units of time, then we specify
##' \code{patientGap} to be \code{c(7,5,3)}. Sometimes, we only think the interval
##' between the 1st and 2nd patient should be increased for the safety consideration
##' , and the rest time intervals can be the same, whatever the cohort size is, then
##' we specify \code{patientGap} to be \code{c(7,3)}. The package will automatically
##' repeat the last element of the vector for the rest time intervals.
##'
##' Note that \code{sizeIntervals} is to be read as follows. For instance, When we
##' want to change the `patientGap` based on the cohort size, i.e. the time interval
##' between the 1st and 2nd patient = 9 units of time and the rest time intervals are
##' 5 units of time when the cohort size is equal or larger than 4. And the time
##' interval between the 1st and 2nd patient = 7 units of time and the rest time
##' intervals are 3 units of time when the cohort size is smaller than 4, then we
##' specify \code{sizeIntervals} to be \code{c(0, 4)}. That means, the right
##' bound of the intervals are exclusive to the interval, and the last interval
##' goes from the last value until infinity.
##'
##' @slot patientGap Observed period of the previous patient before the next patient
##' can be dosed
##' @slot sizeIntervals An integer vector with the left bounds of the relevant
##' cohort size intervals
##' @slot patientFollow The period of time that each patient in the cohort needs to be
##' followed before the next cohort open
##' @slot patientFollowMin At least one patient in the cohort needs to be followed at
##' the minimal follow up time
##'
##' @example examples/Rules-class-SafetyWindowSize.R
##' @export
##' @keywords classes
.SafetyWindowSize <-
  setClass(Class="SafetyWindowSize",
           representation(patientGap="list",
                          sizeIntervals="numeric",
                          patientFollow="numeric",
                          patientFollowMin="numeric"),
           prototype(patientGap=list(0),
                     sizeIntervals=as.integer(c(1L,3L)),
                     patientFollow=1,
                     patientFollowMin=1),
           contains="SafetyWindow",
           validity=
             function(object){
               o <- Validate()

               o$check(all(sapply(object@patientGap,function(x){x>=0})),
                       "patientGap should be non-negative number")
               o$check(all(object@sizeIntervals > 0),
                       "sizeIntervals must only contain positive integers")
               o$check(all(object@patientFollow > 0),
                       "patientFollow should be positive number")
               o$check(all(object@patientFollowMin > 0),
                       "patientFollowMin should be positive number")

               o$result()
             })
validObject(.SafetyWindowSize())

##' Initialization function for `SafetyWindowSize`
##'
##' @param patientGap see \code{\linkS4class{SafetyWindowSize}}
##' @param sizeIntervals see \code{\linkS4class{SafetyWindowSize}}
##' @param patientFollow see \code{\linkS4class{SafetyWindowSize}}
##' @param patientFollowMin see \code{\linkS4class{SafetyWindowSize}}
##'
##' @return the \code{\linkS4class{SafetyWindowSize}} object
##'
##' @export
##' @keywords methods
SafetyWindowSize <- function(patientGap,
                             sizeIntervals,
                             patientFollow,
                             patientFollowMin)
{
  if(patientFollow > patientFollowMin)
  {
    warning("the value of patientFollowMin is typically larger than the value of patientFollow")
  }
  .SafetyWindowSize(patientGap=patientGap,
                    sizeIntervals=sizeIntervals,
                    patientFollow=patientFollow,
                    patientFollowMin=patientFollowMin)
}


## ============================================================


## --------------------------------------------------
## Constant safety window length
## --------------------------------------------------

##' Constant safety window length
##'
##' This class is used when the `patientGap` should be kept constant.
##'
##' @slot patientGap the constant gap between patients.
##' @slot patientFollow how long to follow each patient.
##' @slot patientFollowMin minimum follow up.
##'
##' @example examples/Rules-class-SafetyWindowConst.R
##' @keywords classes
##' @export
.SafetyWindowConst <-
  setClass(Class="SafetyWindowConst",
           representation(patientGap="numeric",
                          patientFollow="numeric",
                          patientFollowMin="numeric"),
           prototype(patientGap=0,
                     patientFollow=1,
                     patientFollowMin=1),
           contains="SafetyWindow",
           validity=
             function(object){
               o <- Validate()

               o$check(all(object@patientGap >= 0),
                       "patientGap should be non-negative number")
               o$check(all(object@patientFollow > 0),
                       "patientFollow should be positive number")
               o$check(all(object@patientFollowMin > 0),
                       "patientFollowMin should be positive number")

               o$result()
             })
validObject(.SafetyWindowConst())


##' Initialization function for `SafetyWindowConst`
##'
##' @param patientGap see \code{\linkS4class{SafetyWindowConst}}
##' @param patientFollow see \code{\linkS4class{SafetyWindowConst}}
##' @param patientFollowMin see \code{\linkS4class{SafetyWindowConst}}
##'
##' @return the \code{\linkS4class{SafetyWindowConst}} object
##'
##' @export
##' @keywords methods
SafetyWindowConst <- function(patientGap,
                              patientFollow,
                              patientFollowMin)
{
  if(patientFollow > patientFollowMin)
  {
    warning("the value of patientFollowMin is typically larger than the value of patientFollow")
  }
  .SafetyWindowConst(patientGap=patientGap,
                     patientFollow=patientFollow,
                     patientFollowMin=patientFollowMin)
}


## ============================================================

# nolint end
