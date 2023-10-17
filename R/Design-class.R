#' @include Design-validity.R
#' @include Model-class.R
#' @include Rules-class.R
#' @include Data-class.R
#' @include helpers.R
#' @include CrmPackClass-class.R
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
#' @slot cohort_size (`CohortSize`)\cr rules for the cohort sizes.
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
    cohort_size = "CohortSize",
    data = "Data",
    startingDose = "numeric"
  ),
  prototype = prototype(
    nextBest = .NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(3),
    data = Data(doseGrid = 1:3),
    startingDose = 1
  ),
  contains = "CrmPackClass",
  validity = v_rule_design
)

## constructor ----

#' @rdname RuleDesign-class
#'
#' @param nextBest (`NextBest`)\cr see slot definition.
#' @param cohort_size (`CohortSize`)\cr see slot definition.
#' @param data (`Data`)\cr see slot definition.
#' @param startingDose (`number`)\cr see slot definition.
#'
#' @export
#' @example examples/Design-class-RuleDesign.R
#'
RuleDesign <- function(nextBest,
                       cohort_size,
                       data,
                       startingDose) {
  new(
    "RuleDesign",
    nextBest = nextBest,
    cohort_size = cohort_size,
    data = data,
    startingDose = as.numeric(startingDose)
  )
}

#' @rdname RuleDesign-class
#' @note Typically, end users will not use the `.DefaultRuleDesign()` function.
#' @export

.DefaultRuleDesign  <- function() {
  RuleDesign(
    nextBest = NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(size = 3L),
    data = Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
    startingDose = 5
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
    cohort_size = CohortSizeConst(size = 3L),
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

## default constructor ----

#' @rdname Design-class
#' @note Typically, end users will not use the `.DefaultDesign()` function.
#' @export
.DefaultDesign <- function() {
  my_size1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
  )
  my_size2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
  )
  my_size <- maxSize(my_size1, my_size2)

  my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
  my_stopping2 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  )
  my_stopping3 <- StoppingMinPatients(nPatients = 20)
  my_stopping <- (my_stopping1 & my_stopping2) | my_stopping3

  # Initialize the design.
  design <- Design(
    model = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 56
    ),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = my_stopping,
    increments = IncrementsRelative(
      intervals = c(0, 20),
      increments = c(1, 0.33)
    ),
    cohort_size = my_size,
    data = Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100)),
    startingDose = 3
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

## default constructor ----

#' @rdname DualDesign-class
#' @note Typically, end users will not use the `.DefaultDualDesign()` function.
#' @export
.DefaultDualDesign <- function() {
  # my_size1 <- CohortSizeRange(
  #   intervals = c(0, 30),
  #   cohort_size = c(1, 3)
  # )
  # my_size2 <- CohortSizeDLT(
  #   intervals = c(0, 1),
  #   cohort_size = c(1, 3)
  # )
  # my_size <- maxSize(my_size1, my_size2)
  #
  # my_stopping1 <- StoppingTargetBiomarker(
  #   target = c(0.9, 1),
  #   prob = 0.5
  # )
  # my_stopping <- my_stopping1 | StoppingMinPatients(40)
  #
  # my_increments <- IncrementsRelative(
  #   intervals = c(0, 20),
  #   increments = c(1, 0.33)
  # )
  #
  # DualDesign(
  #   model = .DefaultDualEndpointRW(),
  #   data = DataDual(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100)),
  #   nextBest = NextBestDualEndpoint(
  #     target = c(0.9, 1),
  #     overdose = c(0.35, 1),
  #     max_overdose_prob = 0.25
  #   ),
  #   stopping = my_stopping,
  #   increments = my_increments,
  #   cohort_size = my_size,
  #   startingDose = 3
  # )

  my_model <- DualEndpointRW(
    mean = c(0, 1),
    cov = matrix(c(1, 0, 0, 1), nrow = 2),
    sigma2betaW = 0.01,
    sigma2W = c(a = 0.1, b = 0.1),
    rho = c(a = 1, b = 1),
    rw1 = TRUE
  )

  # Choose the rule for selecting the next dose.
  my_next_best <- NextBestDualEndpoint(
    target = c(0.9, 1),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  # Choose the rule for the cohort-size.
  my_size1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
  )
  my_size2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
  )
  my_size <- maxSize(my_size1, my_size2)

  # Choose the rule for stopping.
  my_stopping1 <- StoppingTargetBiomarker(
    target = c(0.9, 1),
    prob = 0.5
  )
  my_stopping <- my_stopping1 | StoppingMinPatients(40)

  # Choose the rule for dose increments.
  my_increments <- IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )

  # Initialize the design.
  DualDesign(
    model = my_model,
    data = DataDual(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100)),
    nextBest = my_next_best,
    stopping = my_stopping,
    increments = my_increments,
    cohort_size = my_size,
    startingDose = 3
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

## default constructor ----

#' @rdname TDDesign-class
#' @note Typically, end users will not use the `.DefaultTDDesign()` function.
#' @export
.DefaultTDDesign <- function() {
  empty_data <- Data(doseGrid = seq(25, 300, 25))

  my_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = empty_data
  )

  TDDesign(
    model = my_model,
    stopping = StoppingMinPatients(nPatients = 36),
    increments = IncrementsRelative(
      intervals = range(empty_data@doseGrid),
      increments = c(2, 2)
    ),
    nextBest = NextBestTD(
      prob_target_drt = 0.35,
      prob_target_eot = 0.3
    ),
    cohort_size = CohortSizeConst(size = 3),
    data = empty_data,
    startingDose = 25
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

## default constructor ----

#' @rdname DualResponsesSamplesDesign-class
#' @note Typically, end users will not use the `.DefaultDualResponsesSamplesDesign()` function.
#' @export
.DefaultDualResponsesSamplesDesign <- function() {
    empty_data <- DataDual(doseGrid = seq(25, 300, 25))

  tox_model <- LogisticIndepBeta(
    binDLE = c(1.05, 1.8),
    DLEweights = c(3, 3),
    DLEdose = c(25, 300),
    data = empty_data
  )
  options <- McmcOptions(burnin = 100, step = 2, samples = 200)
  tox_samples <- mcmc(empty_data, tox_model, options)

  eff_model <- Effloglog(
    eff = c(1.223, 2.513),
    eff_dose = c(25, 300),
    nu = c(a = 1, b = 0.025),
    data = empty_data
  )
  eff_samples <- mcmc(empty_data, eff_model, options)

  my_next_best <- NextBestMaxGainSamples(
    prob_target_drt = 0.35,
    prob_target_eot = 0.3,
    derive = function(samples) {
      as.numeric(quantile(samples, prob = 0.3))
    },
    mg_derive = function(mg_samples) {
      as.numeric(quantile(mg_samples, prob = 0.5))
    }
  )

  DualResponsesSamplesDesign(
    nextBest = my_next_best,
    cohort_size = CohortSizeConst(size = 3),
    startingDose = 25,
    model = tox_model,
    eff_model = eff_model,
    data = empty_data,
    stopping = StoppingMinPatients(nPatients = 36),
    increments = IncrementsRelative(
      intervals = c(25, 300),
      increments = c(2, 2)
    )
  )
}

# DualResponsesDesign.R ----

## class ----

#' `DualResponsesDesign.R`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a class of design based on DLE responses using the [`LogisticIndepBeta`] model
#  and efficacy responses using the [`ModelEff`]  model class
#' without DLE and efficacy samples. It contains all slots from the
#' [`RuleDesign`] and [`TDsamplesDesign`] classes.
#
#' @slot data (`DataDual`)\cr the data set.
#' @slot eff_model (`ModelEff`)\cr the pseudo efficacy model to be used.
#'
#' @aliases DualResponsesDesign
#' @export
#'
.DualResponsesDesign <-
  setClass(
    Class = "DualResponsesDesign",
    slots = c(
      eff_model = "ModelEff",
      data = "DataDual"
    ),
    prototype = prototype(
      nextBest = .NextBestMaxGain(),
      data = DataDual(doseGrid = 1:2),
      startingDose = 1,
      model = .LogisticIndepBeta()
    ),
    contains = "TDDesign"
  )

## constructor ----

#' @rdname DualResponsesDesign-class
#'
#' @param data (`DataDual`)\cr see slot definition.
#' @param eff_model (`ModelEff`)\cr see slot definition.
#' @inheritDotParams TDDesign
#'
#' @example examples/Design-class-DualResponsesDesign.R
#' @export
#'
DualResponsesDesign <- function(eff_model,
                                data,
                                ...) {
  start <- TDDesign(data = data, ...)
  .DualResponsesDesign(
    start,
    eff_model = eff_model,
    data = data
  )
}

## default constructor ----

#' @rdname DualResponsesDesign-class
#' @note Typically, end users will not use the `.DefaultDualResponsesDesign()` function.
#' @export
.DefaultDualResponsesDesign <- function() {
  empty_data <- DataDual(doseGrid = seq(25, 300, 25))

    DualResponsesDesign(
    nextBest = NextBestMaxGain(
      prob_target_drt = 0.35,
      prob_target_eot = 0.3
    ),
    cohort_size = CohortSizeConst(size = 3),
    startingDose = 25,
    model = LogisticIndepBeta(
      binDLE = c(1.05, 1.8),
      DLEweights = c(3, 3),
      DLEdose = c(25, 300),
      data = empty_data
    ),
    eff_model = Effloglog(
      eff = c(1.223, 2.513),
      eff_dose = c(25, 300),
      nu = c(a = 1, b = 0.025),
      data = empty_data
    ),
    data = empty_data,
    stopping = StoppingMinPatients(nPatients = 36),
    increments = IncrementsRelative(
      intervals = c(25, 300),
      increments = c(2, 2)
    )
  )
}


# DADesign ----

## class ----

#' `DADesign`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This class has special requirements for the `model` and `data`
#' slots in comparison to the parent class [`Design`]:
#'
#' @slot model (`GeneralModel`)\cr the model to use, see in particular [`DALogisticLogNormal`] and
#' [`TITELogisticLogNormal`] which make use of the time-to-DLT data.
#' @slot data (`DataDA`)\cr what is the dose grid, any previous data, etc.
#' @slot safetyWindow (`SafetyWindow`)\cr the safety window to apply between cohorts.
#'
#' @aliases DADesign
#' @export
#'
.DADesign <-
  setClass(
    Class = "DADesign",
    slots = c(
      model = "GeneralModel",
      data = "DataDA",
      safetyWindow = "SafetyWindow"
    ),
    prototype = prototype(
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
#' @param model (`GeneralModel`)\cr see slot definition.
#' @param data (`DataDA`)\cr see slot definition.
#' @param safetyWindow (`SafetyWindow`)\cr see slot definition.
#' @inheritDotParams Design
#'
#' @example examples/Design-class-DADesign.R
#' @export
#'
DADesign <- function(model, data,
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

## default constructor ----

#' @rdname DADesign-class
#' @note Typically, end users will not use the `.DefaultDADesign()` function.
#' @export
.DefaultDADesign <- function() {
  emptydata <- DataDA(
    doseGrid = c(0.1, 0.5, 1, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
    Tmax = 60
  )

  npiece_ <- 10
  Tmax_ <- 60

lambda_prior <- function(k) {
  npiece_ / (Tmax_ * (npiece_ - k + 0.5))
}

  model <- DALogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56,
    npiece = npiece_,
    l = as.numeric(t(apply(as.matrix(c(1:npiece_), 1, npiece_), 2, lambda_prior))),
    c_par = 2
  )

  mySize1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
  )
  mySize2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
  )
  mySize <- maxSize(mySize1, mySize2)

  myStopping1 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  )
  myStopping2 <- StoppingMinPatients(nPatients = 50)
  myStopping <- (myStopping1 | myStopping2)

  DADesign(
    model = model,
    increments = IncrementsRelative(
      intervals = c(0, 20),
      increments = c(1, 0.33)
    ),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = myStopping,
    cohort_size = mySize,
    data = emptydata,
    safetyWindow = SafetyWindowConst(c(6, 2), 7, 7),
    startingDose = 3
  )
}
# DesignGrouped ----

## class ----

#' `DesignGrouped`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DesignGrouped`] combines two [`Design`] objects: one for the mono and one
#' for the combo arm of a joint dose escalation design.
#'
#' @slot model (`LogisticLogNormalGrouped`)\cr the model to be used, currently only one
#'   class is allowed.
#' @slot mono (`Design`)\cr defines the dose escalation rules for the mono arm, see
#'   details.
#' @slot combo (`Design`)\cr defines the dose escalation rules for the combo arm, see
#'   details.
#' @slot first_cohort_mono_only (`flag`)\cr whether first test one mono agent cohort, and then
#'   once its DLT data has been collected, we proceed from the second cohort onwards with
#'   concurrent mono and combo cohorts.
#' @slot same_dose (`flag`)\cr whether the lower dose of the separately determined mono and combo
#'   doses should be used as the next dose for both mono and combo.
#'
#' @details Note that the model slots inside the `mono` and `combo` parameters
#'   are ignored (because we don't fit separate regression models for the mono and
#'   combo arms). Instead, the `model` parameter is used to fit a joint regression
#'   model for the mono and combo arms together.
#'
#' @aliases DesignGrouped
#' @export
#'
.DesignGrouped <- setClass(
  Class = "DesignGrouped",
  slots = c(
    model = "LogisticLogNormalGrouped",
    mono = "Design",
    combo = "Design",
    first_cohort_mono_only = "logical",
    same_dose = "logical"
  ),
  prototype = prototype(
    model = .DefaultLogisticLogNormalGrouped(),
    mono = .Design(),
    combo = .Design(),
    first_cohort_mono_only = TRUE,
    same_dose = TRUE
  ),
  validity = v_design_grouped,
  contains = "CrmPackClass"
)

## constructor ----

#' @rdname DesignGrouped-class
#'
#' @param model (`LogisticLogNormalGrouped`)\cr see slot definition.
#' @param mono (`Design`)\cr see slot definition.
#' @param combo (`Design`)\cr see slot definition.
#' @param first_cohort_mono_only (`flag`)\cr see slot definition.
#' @param same_dose (`flag`)\cr see slot definition.
#' @param ... not used.
#'
#' @export
#' @example examples/Design-class-DesignGrouped.R
#'
DesignGrouped <- function(model,
                          mono,
                          combo = mono,
                          first_cohort_mono_only = TRUE,
                          same_dose = TRUE,
                          ...) {
  .DesignGrouped(
    model = model,
    mono = mono,
    combo = combo,
    first_cohort_mono_only = first_cohort_mono_only,
    same_dose = same_dose
  )
}

## default constructor ----

#' @rdname DesignGrouped-class
#' @note Typically, end-users will not use the `.DefaultDesignGrouped()` function.
#' @export
.DefaultDesignGrouped <- .DesignGrouped
