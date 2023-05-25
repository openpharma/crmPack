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

## ThreePlusThreeDesign ----

#' @describeIn RuleDesign-class creates a new 3+3 design object from a dose grid.
#'
#' @param doseGrid (`numeric`)\cr the dose grid to be used (sorted).
#'
#' @export
#' @example examples/Design-class-ThreePlusThreeDesign.R
#'
ThreePlusThreeDesign <- function(doseGrid) {
  empty_data <- Data(doseGrid = doseGrid)

  # Using a constant cohort size of 3 we obtain exactly the 3+3 design.
  RuleDesign(
    nextBest = NextBestThreePlusThree(),
    data = empty_data,
    cohortSize = CohortSizeConst(size = 3L),
    startingDose = doseGrid[1]
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
#' @slot pl_cohort_size (`CohortSize`)\cr rules for the cohort sizes for placebo,
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
    pl_cohort_size = "CohortSize"
  ),
  prototype = prototype(
    model = .LogisticNormal(),
    nextBest = .NextBestNCRM(),
    stopping = .StoppingMinPatients(),
    increments = .IncrementsRelative(),
    pl_cohort_size = CohortSizeConst(0L)
  ),
  contains = "RuleDesign"
)

## constructor ----

#' @rdname Design-class
#'
#' @param model (`GeneralModel`)\cr see slot definition.
#' @param stopping (`Stopping`)\cr see slot definition.
#' @param increments (`Increments`)\cr see slot definition.
#' @param pl_cohort_size (`CohortSize`)\cr see slot definition.
#' @inheritDotParams RuleDesign
#'
#' @export
#' @example examples/Design-class-Design.R
#'
#'
Design <- function(model,
                   stopping,
                   increments,
                   pl_cohort_size = CohortSizeConst(0L),
                   ...) {
  start <- RuleDesign(...)
  new(
    "Design",
    start,
    model = model,
    stopping = stopping,
    increments = increments,
    pl_cohort_size = pl_cohort_size
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

# TDsamplesDesign ----

## class ----

#' `TDsamplesDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`TDsamplesDesign`] is the class of design based only on DLT responses using
#' [`ModelTox`] class model (i.e. [`LogisticIndepBeta`]) as well as MCMC samples
#' obtained for this model.
#'
#' @slot model (`ModelTox`)\cr the pseudo DLT model to be used.
#' @slot stopping (`Stopping`)\cr stopping rule(s) for the trial.
#' @slot increments (`Increments`)\cr how to control increments between dose levels.
#' @slot pl_cohort_size (`CohortSize`)\cr rules for the cohort sizes for placebo,
#'   if any planned (defaults to constant 0 placebo patients).
#'
#' @aliases TDsamplesDesign
#' @export
#'
.TDsamplesDesign <- setClass(
  Class = "TDsamplesDesign",
  slots = c(
    model = "ModelTox",
    stopping = "Stopping",
    increments = "Increments",
    pl_cohort_size = "CohortSize"
  ),
  prototype = prototype(
    model = .LogisticIndepBeta(),
    nextBest = .NextBestTDsamples(),
    stopping = .StoppingMinPatients(),
    increments = .IncrementsRelative(),
    pl_cohort_size = CohortSizeConst(0L)
  ),
  contains = "RuleDesign"
)

## constructor ----

#' @rdname TDsamplesDesign-class
#'
#' @param model (`ModelTox`)\cr see slot definition.
#' @param stopping (`Stopping`)\cr see slot definition.
#' @param increments (`Increments`)\cr see slot definition.
#' @param pl_cohort_size (`CohortSize`)\cr see slot definition.
#' @inheritDotParams RuleDesign
#'
#' @export
#' @example examples/Design-class-TDsamplesDesign.R
#'
TDsamplesDesign <- function(model,
                            stopping,
                            increments,
                            pl_cohort_size = CohortSizeConst(0L),
                            ...) {
  start <- RuleDesign(...)
  new(
    "TDsamplesDesign",
    start,
    model = model,
    stopping = stopping,
    increments = increments,
    pl_cohort_size = pl_cohort_size
  )
}

# TDDesign ----

## class ----

#' `TDDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' [`TDDesign`] is the class of design based only on DLT responses using
#' [`ModelTox`] class model (i.e. [`LogisticIndepBeta`]) without MCMC samples.
#'
#' @slot model (`ModelTox`)\cr the pseudo DLT model to be used.
#' @slot stopping (`Stopping`)\cr stopping rule(s) for the trial.
#' @slot increments (`Increments`)\cr how to control increments between dose levels.
#' @slot pl_cohort_size (`CohortSize`)\cr rules for the cohort sizes for placebo,
#'   if any planned (defaults to constant 0 placebo patients).
#'
#' @aliases TDDesign
#' @export
#'
.TDDesign <- setClass(
  Class = "TDDesign",
  slots = c(
    model = "ModelTox",
    stopping = "Stopping",
    increments = "Increments",
    pl_cohort_size = "CohortSize"
  ),
  prototype = prototype(
    model = .LogisticIndepBeta(),
    nextBest = .NextBestTD(),
    stopping = .StoppingMinPatients(),
    increments = .IncrementsRelative(),
    pl_cohort_size = CohortSizeConst(0L)
  ),
  contains = "RuleDesign"
)

## constructor ----

#' @rdname TDDesign-class
#'
#' @param model (`ModelTox`)\cr see slot definition.
#' @param stopping (`Stopping`)\cr see slot definition.
#' @param increments (`Increments`)\cr see slot definition.
#' @param pl_cohort_size (`CohortSize`)\cr see slot definition.
#' @inheritDotParams RuleDesign
#'
#' @export
#' @example examples/Design-class-TDDesign.R
#'
TDDesign <- function(model,
                     stopping,
                     increments,
                     pl_cohort_size = CohortSizeConst(0L),
                     ...) {
  start <- RuleDesign(...)
  new(
    "TDDesign",
    start,
    model = model,
    stopping = stopping,
    increments = increments,
    pl_cohort_size = pl_cohort_size
  )
}

# DualResponsesSamplesDesign ----

## class ----

#' `DualResponsesSamplesDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a class of design based on DLE responses using the [`LogisticIndepBeta`] model
#  and efficacy responses using [`ModelEff`]  model class
#' with DLE and efficacy samples. It contain all slots in
#' [`RuleDesign`] and [`TDsamplesDesign`] class objects.
#
#' @slot data (`DataDual`)\cr the data set.
#' @slot eff_model (`ModelEff`)\cr the pseudo efficacy model to be used.
#'
#' @aliases DualResponsesSamplesDesign
#' @export
#'
.DualResponsesSamplesDesign <-
  setClass(
    Class = "DualResponsesSamplesDesign",
    slots = c(
      eff_model = "ModelEff",
      data = "DataDual"
    ),
    prototype = prototype(
      nextBest = .NextBestMaxGainSamples(),
      data = DataDual(doseGrid = 1:2),
      startingDose = 1,
      model = .LogisticIndepBeta()
    ),
    contains = "TDsamplesDesign"
  )

## constructor ----

#' @rdname DualResponsesSamplesDesign-class
#'
#' @param data (`DataDual`)\cr see slot definition.
#' @param eff_model (`ModelEff`)\cr see slot definition.
#' @inheritDotParams TDsamplesDesign
#'
#' @example examples/Design-class-DualResponsesSamplesDesign.R
#' @export
#'
DualResponsesSamplesDesign <- function(eff_model,
                                       data,
                                       ...) {
  start <- TDsamplesDesign(data = data, ...)
  .DualResponsesSamplesDesign(
    start,
    eff_model = eff_model,
    data = data
  )
}

# nolint start

# DualResponsesDesign ----

## class ----

#' `DualResponsesDesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a class of design based on DLE responses using the [`LogisticIndepBeta`] model
#' model and efficacy responses using [`ModelEff`]  model class
#' without DLE and efficacy samples. It contain all slots in
#' [`RuleDesign`] and [`TDDesign`] class object
#'
#' @slot data the data set of [`DataDual`] class object
#' @slot eff_model the pseudo efficacy model to be used, an object class of
#' [`ModelEff`]
#'
#' @example examples/design-class-DualResponsesDesign.R
#' @export
.DualResponsesDesign <-
  setClass(
    Class = "DualResponsesDesign",
    slots = c(
      eff_model = "ModelEff",
      data = "DataDual"
    ),
    prototype(
      nextBest = .NextBestMaxGain(),
      data = DataDual(doseGrid = 1:2),
      startingDose = 1,
      model = .LogisticIndepBeta()
    ),
    contains = "TDDesign"
  )

## constructor ----

#' @rdname DualResponsesDesign-class
#' @param data please refer to [`DualResponsesDesign`] class object
#' @param eff_model please refer to [`DualResponsesDesign`] class object
#' @param \dots additional arguments for \code{\link{TDDesign}}
#' @return the [`DualResponsesDesign`] class object
#'
#' @export
DualResponsesDesign <- function(eff_model,
                                data,
                                ...) {
  start <- TDDesign(data = data, ...)
  .DualResponsesDesign(start,
    eff_model = eff_model,
    data = data
  )
}

# ===============================================================================

# DADesign ----

## class ----


#' `DADesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This class has special requirements for the \code{model} and \code{data}
#' slots in comparison to the parent class [`Design`]:
#'
#' @slot model (`DADesign`), an object of or inheriting from class
#' [`GeneralModel`], see in particular
#' [`DALogisticLogNormal`] and
#' [`TITELogisticLogNormal`] which make use of the
#' time-to-DLT data
#' @slot data what is the dose grid, any previous data, etc., contained
#' in an object of class [`DataDA`]
#' @slot safetyWindow still to be documented.
#'
#' @aliases DADesign
#' @example examples/design-class-DADesign.R
#' @export
.DADesign <-
  setClass(
    Class = "DADesign",
    slots = c(
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
    contains = "Design"
  )


## constructor ----


#' @rdname DADesign-class
#'
#' @param model (`DALogisticLogNormal`)\cr see slot definition.
#' @param data see [`DADesign`]
#' @param safetyWindow see [`DADesign`]
#' @param \dots additional arguments for \code{\link{Design}}
#' @return the [`DADesign`] object
#'
#' @export
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
