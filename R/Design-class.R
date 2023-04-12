#' @include Design-validity.R
#' @include Model-class.R
#' @include Rules-class.R
#' @include Data-class.R
#' @include helpers.R
NULL

# RuleDesign ----

## class ----

#' `RuleDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`RuleDesign`] is the class for rule-based designs. The difference between
#' this class and the [`Design`] class is that [`RuleDesign`] does not contain
#' `model`, `stopping` and `increments` slots.
#'
#' @slot nextBest (`NextBest`)\cr how to find the next best dose.
#' @slot cohortSize (`CohortSize`)\cr rules for the cohort sizes.
#' @slot data (`Data`)\cr specifies dose grid, any previous data, etc.
#' @slot startingDose (`number`)\cr the starting dose, it must lie on the dose
#'   grid in `data`.
#'
#' @aliases RuleDesign
#' @export
#'
.RuleDesign <- setClass(
  Class = "RuleDesign",
  slots = c(
    nextBest = "NextBest",
    cohortSize = "CohortSize",
    data = "Data",
    startingDose = "numeric"
  ),
  prototype = prototype(
    nextBest = .NextBestThreePlusThree(),
    cohortSize = CohortSizeConst(3),
    data = Data(doseGrid = 1:3),
    startingDose = 1
  ),
  validity = v_rule_design
)

## constructor ----

#' @rdname RuleDesign-class
#'
#' @param nextBest (`NextBest`)\cr see slot definition.
#' @param cohortSize (`CohortSize`)\cr see slot definition.
#' @param data (`Data`)\cr see slot definition.
#' @param startingDose (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Design-class-RuleDesign.R
#'
RuleDesign <- function(nextBest,
                       cohortSize,
                       data,
                       startingDose) {
  new(
    "RuleDesign",
    nextBest = nextBest,
    cohortSize = cohortSize,
    data = data,
    startingDose = as.numeric(startingDose)
  )
}

# Design ----

## class ----

#' `Design`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`Design`] is the class for rule-based designs. The difference between
#' this class and its parent [`RuleDesign`] class is that [`Design`] class
#' contains additional `model`, `stopping` and `increments` slots.
#'
#' @slot model (`GeneralModel`)\cr the model to be used.
#' @slot stopping (`Stopping`)\cr stopping rule(s) for the trial.
#' @slot increments (`Increments`)\cr how to control increments between dose levels.
#' @slot PLcohortSize (`CohortSize`)\cr rules for the cohort sizes for placebo,
#'   if any planned (defaults to constant 0 placebo patients).
#'
#' @aliases Design
#' @export
#'
.Design <- setClass(
  Class = "Design",
  slots = c(
    model = "GeneralModel",
    stopping = "Stopping",
    increments = "Increments",
    PLcohortSize = "CohortSize"
  ),
  prototype = prototype(
    model = .LogisticNormal(),
    nextBest = .NextBestNCRM(),
    stopping = .StoppingMinPatients(),
    increments = .IncrementsRelative(),
    PLcohortSize = CohortSizeConst(0L)
  ),
  contains = "RuleDesign"
)

## constructor ----

#' @rdname Design-class
#'
#' @param model (`GeneralModel`)\cr see slot definition.
#' @param stopping (`Stopping`)\cr see slot definition.
#' @param increments (`Increments`)\cr see slot definition.
#' @param PLcohortSize (`CohortSize`)\cr see slot definition.
#' @inheritDotParams RuleDesign
#'
#' @export
#' @example examples/Design-class-Design.R
#'
#'
Design <- function(model,
                   stopping,
                   increments,
                   PLcohortSize = CohortSizeConst(0L),
                   ...) {
  start <- RuleDesign(...)
  new(
    "Design",
    start,
    model = model,
    stopping = stopping,
    increments = increments,
    PLcohortSize = PLcohortSize
  )
}

# DualDesign ----

## class ----

#' `DualDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`DualDesign`] is the class for the dual-endpoint CRM design. This class has
#' special requirements for the `model` and `data` slots in comparison to the
#' parent class [`Design`].
#'
#' @note the `nextBest` slot can be of any class, this allows for easy comparison
#'   with recommendation methods that don't use the biomarker information.
#'
#' @slot model (`DualEndpoint`)\cr the model to be used.
#' @slot data (`DataDual`)\cr specifies dose grid, any previous data, etc.
#'
#' @aliases DualDesign
#' @export
#'
.DualDesign <- setClass(
  Class = "DualDesign",
  slots = c(
    model = "DualEndpoint",
    data = "DataDual"
  ),
  prototype = prototype(
    model = .DualEndpoint(),
    nextBest = .NextBestDualEndpoint(),
    data = DataDual(doseGrid = 1:2),
    startingDose = 1
  ),
  contains = "Design"
)

## constructor ----

#' @rdname DualDesign-class
#'
#' @param model (`DualEndpoint`)\cr see slot definition.
#' @param data (`DataDual`)\cr see slot definition.
#' @inheritDotParams Design
#'
#' @export
#' @example examples/Design-class-DualDesign.R
#'
#'
DualDesign <- function(model,
                       data,
                       ...) {
  start <- Design(model = model, data = data, ...)
  new(
    "DualDesign",
    start,
    model = model,
    data = data
  )
}

# nolint start

##' Creates a new 3+3 design object from a dose grid
##'
##' @param doseGrid the dose grid to be used
##' @return the object of class \code{\linkS4class{RuleDesign}} with the
##' 3+3 design
##'
##' @example examples/design-class-ThreePlusThreeDesign.R
##' @export
##' @keywords programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
ThreePlusThreeDesign <- function(doseGrid) {
  emptydata <- Data(doseGrid = doseGrid)

  design <- RuleDesign(
    nextBest = NextBestThreePlusThree(),
    data = emptydata,
    cohortSize = CohortSizeConst(size = 3L),
    ## using a constant cohort size of 3,
    ## we obtain exactly the 3+3 design
    startingDose = head(emptydata@doseGrid, 1)
  )

  return(design)
}

## ===================================================================================
## -------------------------------------------------------------------------------
## Design class using DLE responses only based on the pseudo DLE model with samples
## ---------------------------------------------------------------------------
##' This is a class of design based only on DLE responses using the 'LogisticIndepBeta' class model
##' and DLE samples are also used.
##' In addition to the slots in the more simple \code{\linkS4class{RuleDesign}},
##' objects of this class contain:
##'
##' @slot model the pseudo DLE model to be used, an object class of
##' \code{\linkS4class{ModelTox}}
##' @slot stopping stopping rule(s) for the trial, an object class of \code{\linkS4class{Stopping}}
##' @slot increments how to control increments between dose levels, an object class of
##' \code{\linkS4class{Increments}}
##' @slot PLcohortSize rules for the cohort sizes for placebo, if any planned
##' an object of class \code{\linkS4class{CohortSize}}
##'
##' @example examples/design-class-TDsamplesDesign.R
##' @export
##' @keywords class
.TDsamplesDesign <-
  setClass(
    Class = "TDsamplesDesign",
    representation(
      model = "ModelTox",
      stopping = "Stopping",
      increments = "Increments",
      PLcohortSize = "CohortSize"
    ),
    prototype(
      model = .LogisticIndepBeta(),
      nextBest = .NextBestTDsamples(),
      stopping = .StoppingMinPatients(),
      increments = .IncrementsRelative(),
      PLcohortSize = CohortSizeConst(0L)
    ),
    contains = list("RuleDesign")
  )

validObject(.TDsamplesDesign())
##' Initialization function for 'TDsamplesDesign' class
##'
##' @param model see \code{\linkS4class{TDsamplesDesign}}
##' @param stopping see \code{\linkS4class{TDsamplesDesign}}
##' @param increments see \code{\linkS4class{TDsamplesDesign}}
##' @param PLcohortSize see \code{\linkS4class{TDsamplesDesign}}
##' @param \dots additional arguments for \code{\linkS4class{RuleDesign}}
##' @return the \code{\linkS4class{TDsamplesDesign}} class object
##'
##' @export
##' @keywords methods
TDsamplesDesign <- function(model, stopping, increments, PLcohortSize = CohortSizeConst(0L), ...) {
  start <- RuleDesign(...)
  .TDsamplesDesign(start, model = model, stopping = stopping, increments = increments, PLcohortSize = PLcohortSize)
}

## =============================================================================
## -------------------------------------------------------------------------------
##' Design class using DLE responses only based on the pseudo DLE model without sample
##'
##' This is a class of design based only on DLE responses using the 'LogisticIndepBeta' class model
##' are used without samples.
##' In addition to the slots in the more simple \code{\linkS4class{RuleDesign}},
##' objects of this class contain:
##'
##' @slot model the pseudo DLE model to be used, an object class of
##' \code{\linkS4class{ModelTox}}
##' @slot stopping stopping rule(s) for the trial, an object class of \code{\linkS4class{Stopping}}
##' @slot increments how to control increments between dose levels, an object class of
##' \code{\linkS4class{Increments}}
##' @slot PLcohortSize rules for the cohort sizes for placebo, if any planned
##' an object of class \code{\linkS4class{CohortSize}}
##'
##' @example examples/design-class-TDDesign.R
##' @export
##' @keywords class
.TDDesign <-
  setClass(
    Class = "TDDesign",
    representation(
      model = "ModelTox",
      stopping = "Stopping",
      increments = "Increments",
      PLcohortSize = "CohortSize"
    ),
    prototype(
      model = .LogisticIndepBeta(),
      nextBest = .NextBestTD(),
      stopping = .StoppingMinPatients(),
      increments = .IncrementsRelative(),
      PLcohortSize = CohortSizeConst(0L)
    ),
    contains = list("RuleDesign")
  )

validObject(.TDDesign())

##' Initialization function for 'TDDesign' class
##'
##' @param model please refer to \code{\linkS4class{TDDesign}} class object
##' @param stopping please refer to \code{\linkS4class{TDDesign}} class object
##' @param increments please refer to \code{\linkS4class{TDDesign}} class object
##' @param PLcohortSize see \code{\linkS4class{TDDesign}}
##' @param \dots additional arguments for \code{\linkS4class{RuleDesign}}
##' @return the \code{\linkS4class{TDDesign}} class object
##'
##' @export
##' @keywords methods
TDDesign <- function(model,
                     stopping,
                     increments,
                     PLcohortSize = CohortSizeConst(0L),
                     ...) {
  start <- RuleDesign(...)
  .TDDesign(start,
    model = model, stopping = stopping, increments = increments,
    PLcohortSize = PLcohortSize
  )
}



## ---------------------------------------------------------------------------------------------------
## class for design based on DLE and efficacy response with samples using pseudo DLE and efficacy models
## ----------------------------------------------------------------------------------------------------
##' This is a class of design based on DLE responses using the \code{\linkS4class{LogisticIndepBeta}} model
##' model and efficacy responses using \code{\linkS4class{ModelEff}}  model class
##' with DLE and efficacy samples.It contain all slots in
##' \code{\linkS4class{RuleDesign}} and \code{\linkS4class{TDsamplesDesign}} class object
##'
##' @slot data the data set of \code{\linkS4class{DataDual}} class object
##' @slot Effmodel the pseudo efficacy model to be used, an object class of
##' \code{\linkS4class{ModelEff}}
##'
##' @example examples/design-class-DualResponsesSamplesDesign.R
##' @export
##' @keywords class
##'
.DualResponsesSamplesDesign <-
  setClass(
    Class = "DualResponsesSamplesDesign",
    representation(
      Effmodel = "ModelEff",
      data = "DataDual"
    ),
    prototype(
      nextBest = .NextBestMaxGainSamples(),
      data = DataDual(doseGrid = 1:2),
      startingDose = 1,
      model = .LogisticIndepBeta()
    ),
    contains = list("TDsamplesDesign")
  )
validObject(.DualResponsesSamplesDesign())

##' Initialization function for 'DualResponsesSamplesDesign"
##' @param data please refer to \code{\linkS4class{DualResponsesSamplesDesign}} class object
##' @param Effmodel please refer to \code{\linkS4class{DualResponsesSamplesDesign}} class object
##' @param \dots additional arguments for \code{\link{TDsamplesDesign}}
##'
##' @return the \code{\linkS4class{DualResponsesSamplesDesign}} class object
##'
##' @export
##' @keywords methods
DualResponsesSamplesDesign <- function(Effmodel,
                                       data,
                                       ...) {
  start <- TDsamplesDesign(data = data, ...)
  .DualResponsesSamplesDesign(start,
    Effmodel = Effmodel,
    data = data
  )
}

## ---------------------------------------------------------------------------------------------------
## class for design based on DLE and efficacy response without  samples using pseudo DLE and efficacy models
## ----------------------------------------------------------------------------------------------------
##' This is a class of design based on DLE responses using the \code{\linkS4class{LogisticIndepBeta}} model
##' model and efficacy responses using \code{\linkS4class{ModelEff}}  model class
##' without DLE and efficacy samples. It contain all slots in
##' \code{\linkS4class{RuleDesign}} and \code{\linkS4class{TDDesign}} class object
##'
##' @slot data the data set of \code{\linkS4class{DataDual}} class object
##' @slot Effmodel the pseudo efficacy model to be used, an object class of
##' \code{\linkS4class{ModelEff}}
##'
##' @example examples/design-class-DualResponsesDesign.R
##' @export
##' @keywords class
.DualResponsesDesign <-
  setClass(
    Class = "DualResponsesDesign",
    representation(
      Effmodel = "ModelEff",
      data = "DataDual"
    ),
    prototype(
      nextBest = .NextBestMaxGain(),
      data = DataDual(doseGrid = 1:2),
      startingDose = 1,
      model = .LogisticIndepBeta()
    ),
    contains = list("TDDesign")
  )
validObject(.DualResponsesDesign())


##' Initialization function for 'DualResponsesDesign"
##' @param data please refer to \code{\linkS4class{DualResponsesDesign}} class object
##' @param Effmodel please refer to \code{\linkS4class{DualResponsesDesign}} class object
##' @param \dots additional arguments for \code{\link{TDDesign}}
##' @return the \code{\linkS4class{DualResponsesDesign}} class object
##'
##' @export
##' @keywords methods
DualResponsesDesign <- function(Effmodel,
                                data,
                                ...) {
  start <- TDDesign(data = data, ...)
  .DualResponsesDesign(start,
    Effmodel = Effmodel,
    data = data
  )
}

## ===============================================================================


##' Class for the time-to-DLT augmented CRM design
##'
##' This class has special requirements for the \code{model} and \code{data}
##' slots in comparison to the parent class \code{\linkS4class{Design}}:
##'
##' @slot model the model to be used, an object of or inheriting from class
##' \code{\linkS4class{GeneralModel}}, see in particular
##' \code{\linkS4class{DALogisticLogNormal}} and
##' \code{\linkS4class{TITELogisticLogNormal}} which make use of the
##' time-to-DLT data
##' @slot data what is the dose grid, any previous data, etc., contained
##' in an object of class \code{\linkS4class{DataDA}}
##' @slot safetyWindow still to be documented.
##'
##' @example examples/design-class-DADesign.R
##' @export
##' @keywords classes
.DADesign <-
  setClass(
    Class = "DADesign",
    representation(
      model = "GeneralModel",
      data = "DataDA",
      safetyWindow = "SafetyWindow"
    ),
    prototype(
      model = .DALogisticLogNormal(),
      nextBest = .NextBestNCRM(),
      data = DataDA(doseGrid = 1:2),
      safetyWindow = .SafetyWindowConst()
    ),
    contains = list("Design")
  )
validObject(.DADesign())


##' Initialization function for `DADesign`
##'
##' @param model see \code{\linkS4class{DADesign}}
##' @param data see \code{\linkS4class{DADesign}}
##' @param safetyWindow see \code{\linkS4class{DADesign}}
##' @param \dots additional arguments for \code{\link{Design}}
##' @return the \code{\linkS4class{DADesign}} object
##'
##' @export
##' @keywords methods
DADesign <- function(model,
                     data,
                     safetyWindow,
                     ...) {
  start <- Design(
    data = data,
    model = model,
    ...
  )
  .DADesign(start,
    safetyWindow = safetyWindow
  )
}
validObject(DADesign(
  model = .DALogisticLogNormal(),
  data = DataDA(doseGrid = 1:2),
  safetyWindow = .SafetyWindowConst(),
  nextBest = .NextBestNCRM(),
  startingDose = 1,
  cohortSize = CohortSizeConst(3),
  stopping = StoppingMinCohorts(10),
  increments = IncrementsDoseLevels(2)
))

# nolint end
