#' @include Design-class.R
#' @include HierarchicalDesign-validity.R
NULL

# ArmCondition ----

## class ----

#' `ArmCondition`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmCondition`] is a virtual class for criteria controlling when a
#' [`DesignArm`] opens for enrollment in a [`HierarchicalDesign`].
#'
#' @seealso [`NoArmCondition`], [`ArmFinishedCondition`],
#'   [`ArmMinDoseCondition`], [`ArmConditionAll`], [`ArmConditionAny`].
#'
#' @aliases ArmCondition
#' @export
#'
.ArmCondition <- setClass(
  Class = "ArmCondition",
  contains = "CrmPackClass"
)

## default constructor ----

#' @rdname ArmCondition-class
#' @note Typically, end users will not use the `.DefaultArmCondition()`
#'   function.
#' @export
.DefaultArmCondition <- function() {
  stop(
    paste(
      "Class ArmCondition should not be instantiated directly.",
      "Please use one of its subclasses instead."
    )
  )
}

# NoArmCondition ----

## class ----

#' `NoArmCondition`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`NoArmCondition`] always opens an active arm immediately. It is the default
#' opening condition for [`DesignArm`] objects.
#'
#' @aliases NoArmCondition
#' @export
#'
.NoArmCondition <- setClass(
  Class = "NoArmCondition",
  contains = "ArmCondition"
)

## constructor ----

#' @rdname NoArmCondition-class
#'
#' @export
NoArmCondition <- function() {
  .NoArmCondition()
}

## default constructor ----

#' @rdname NoArmCondition-class
#' @note Typically, end users will not use the `.DefaultNoArmCondition()`
#'   function.
#' @export
.DefaultNoArmCondition <- function() {
  NoArmCondition()
}

# ArmFinishedCondition ----

## class ----

#' `ArmFinishedCondition`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmFinishedCondition`] opens an arm when the named arm has finished dose
#' escalation.
#'
#' @slot arm_name (`string`)\cr the name of the arm that must have finished.
#'
#' @aliases ArmFinishedCondition
#' @export
#'
.ArmFinishedCondition <- setClass(
  Class = "ArmFinishedCondition",
  slots = c(arm_name = "character"),
  prototype = prototype(arm_name = "Arm"),
  contains = "ArmCondition",
  validity = v_arm_finished_condition
)

## constructor ----

#' @rdname ArmFinishedCondition-class
#'
#' @param arm_name (`string`)\cr see slot definition.
#'
#' @export
ArmFinishedCondition <- function(arm_name) {
  assert_string(arm_name)
  .ArmFinishedCondition(arm_name = arm_name)
}

## default constructor ----

#' @rdname ArmFinishedCondition-class
#' @note Typically, end users will not use the
#'   `.DefaultArmFinishedCondition()` function.
#' @export
.DefaultArmFinishedCondition <- function() {
  ArmFinishedCondition("Arm")
}

# ArmMinDoseCondition ----

## class ----

#' `ArmMinDoseCondition`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmMinDoseCondition`] opens an arm when the named arm has enrolled at
#' least one patient at the specified minimum dose or higher.
#'
#' @slot arm_name (`string`)\cr the name of the arm whose data are checked.
#' @slot min_dose (`numeric`)\cr the minimum dose that must have been reached.
#'   For combination arms, this can be a vector of minimum doses for each agent.
#'
#' @aliases ArmMinDoseCondition
#' @export
#'
.ArmMinDoseCondition <- setClass(
  Class = "ArmMinDoseCondition",
  slots = c(
    arm_name = "character",
    min_dose = "numeric"
  ),
  prototype = prototype(
    arm_name = "Arm",
    min_dose = 0
  ),
  contains = "ArmCondition",
  validity = v_arm_min_dose_condition
)

## constructor ----

#' @rdname ArmMinDoseCondition-class
#'
#' @param arm_name (`string`)\cr see slot definition.
#' @param min_dose (`numeric`)\cr see slot definition.
#'
#' @export
ArmMinDoseCondition <- function(arm_name, min_dose) {
  assert_string(arm_name)
  assert_numeric(
    min_dose,
    lower = 0,
    finite = TRUE,
    min.len = 1L,
    any.missing = FALSE
  )
  .ArmMinDoseCondition(arm_name = arm_name, min_dose = min_dose)
}

## default constructor ----

#' @rdname ArmMinDoseCondition-class
#' @note Typically, end users will not use the `.DefaultArmMinDoseCondition()`
#'   function.
#' @export
.DefaultArmMinDoseCondition <- function() {
  ArmMinDoseCondition("Arm", 0)
}

# ArmConditionList and logical operators ----

## ArmConditionList ----

#' `ArmConditionList`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmConditionList`] is a virtual class for combining multiple
#' [`ArmCondition`] objects using logical operators. It is used as a base class
#' for [`ArmConditionAll`] and [`ArmConditionAny`].
#'
#' @slot condition_list (`list`)\cr a list of [`ArmCondition`] objects to be
#'   combined.
#'
#' @seealso [`ArmCondition`], [`ArmConditionAll`], [`ArmConditionAny`].
#'
#' @aliases ArmConditionList
#' @export
#'
.ArmConditionList <- setClass(
  Class = "ArmConditionList",
  contains = "ArmCondition",
  slots = list(condition_list = "list")
)

## constructor ----

#' @rdname ArmConditionList-class
#'
#' @param ... (`ArmCondition`)\cr arm condition objects to combine.
#'
#' @export
ArmConditionList <- function(...) {
  args <- list(...)
  assert_list(args, min.len = 1L)
  for (arg in args) {
    assert_class(arg, "ArmCondition")
  }
  .ArmConditionList(condition_list = args)
}

## default constructor ----

#' @rdname ArmConditionList-class
#' @note Typically, end users will not use the `.DefaultArmConditionList()`
#'   function.
#' @export
.DefaultArmConditionList <- function() {
  ArmConditionList(NoArmCondition(), NoArmCondition())
}

## ArmConditionAll ----

#' `ArmConditionAll`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmConditionAll`] combines multiple [`ArmCondition`] objects using AND
#' logic. An arm opens only if ALL conditions in the list are satisfied.
#'
#' @slot condition_list (`list`)\cr a list of [`ArmCondition`] objects to be
#'   combined with AND logic.
#'
#' @seealso [`ArmCondition`], [`ArmConditionAny`], [`ArmConditionList`].
#'
#' @aliases ArmConditionAll
#' @export
#'
.ArmConditionAll <- setClass(
  Class = "ArmConditionAll",
  contains = "ArmConditionList"
)

## constructor ----

#' @rdname ArmConditionAll-class
#'
#' @param ... (`ArmCondition`)\cr arm condition objects to combine with AND
#'   logic.
#'
#' @export
ArmConditionAll <- function(...) {
  start <- ArmConditionList(...)
  .ArmConditionAll(start)
}

## default constructor ----

#' @rdname ArmConditionAll-class
#' @note Typically, end users will not use the `.DefaultArmConditionAll()`
#'   function.
#' @export
.DefaultArmConditionAll <- function() {
  ArmConditionAll(NoArmCondition(), NoArmCondition())
}

## ArmConditionAny ----

#' `ArmConditionAny`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`ArmConditionAny`] combines multiple [`ArmCondition`] objects using OR
#' logic. An arm opens if ANY condition in the list is satisfied.
#'
#' @slot condition_list (`list`)\cr a list of [`ArmCondition`] objects to be
#'   combined with OR logic.
#'
#' @seealso [`ArmCondition`], [`ArmConditionAll`], [`ArmConditionList`].
#'
#' @aliases ArmConditionAny
#' @export
#'
.ArmConditionAny <- setClass(
  Class = "ArmConditionAny",
  contains = "ArmConditionList"
)

## constructor ----

#' @rdname ArmConditionAny-class
#'
#' @param ... (`ArmCondition`)\cr arm condition objects to combine with OR
#'   logic.
#'
#' @export
ArmConditionAny <- function(...) {
  start <- ArmConditionList(...)
  .ArmConditionAny(start)
}

## default constructor ----

#' @rdname ArmConditionAny-class
#' @note Typically, end users will not use the `.DefaultArmConditionAny()`
#'   function.
#' @export
.DefaultArmConditionAny <- function() {
  ArmConditionAny(NoArmCondition(), NoArmCondition())
}

# DesignArm ----

## class ----

#' `DesignArm`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DesignArm`] is a light-weight wrapper around `Design` object
#' for use in a [`HierarchicalDesign`]. Use [`DesignArm()`] for active arms
#' and [`HistoricalArm()`] for non-enrolling historical arms.
#'
#' @slot name (`string`)\cr the name of the arm.
#' @slot active (`flag`)\cr whether the arm is enrolling or not (historical).
#' @slot borrow (`flag`)\cr whether this arm may borrow information from other
#'   arms when making dose escalation decisions. Only relevant if `active = TRUE`.
#' @slot open_when (`ArmCondition`)\cr the condition that must be satisfied
#'   before this arm opens for enrollment.
#' @slot design (`Design`)\cr the design object for this arm.
#'
#' @aliases DesignArm
#' @export
#'
.DesignArm <- setClass(
  Class = "DesignArm",
  slots = c(
    name = "character",
    active = "logical",
    borrow = "logical",
    open_when = "ArmCondition",
    design = "ANY"
  ),
  prototype = prototype(
    name = "Arm",
    active = TRUE,
    borrow = TRUE,
    open_when = NoArmCondition(),
    design = .Design()
  ),
  contains = "CrmPackClass",
  validity = v_design_arm
)

## constructor ----

#' @rdname DesignArm-class
#'
#' @param name (`string`)\cr see slot definition.
#' @param design (`Design`)\cr see slot definition.
#' @param borrow (`flag`)\cr see slot definition.
#' @param open_when (`ArmCondition`)\cr see slot definition.
#' @param data (`Data` or `DataCombo`)\cr arm data for historical arms.
#' @param model (`GeneralModel` or `TwoDrugsCombo`)\cr arm model for
#'   historical arms.
#'
#' @aliases HistoricalArm
#' @export
#' @example examples/Design-class-DesignArm.R
#'
DesignArm <- function(
  name,
  design,
  borrow = TRUE,
  open_when = NoArmCondition()
) {
  new(
    "DesignArm",
    name = name,
    active = TRUE,
    borrow = borrow,
    open_when = open_when,
    design = design
  )
}

#' @rdname DesignArm-class
#' @export
HistoricalArm <- function(name, data, model, borrow = TRUE) {
  design <- h_historical_arm_design(data = data, model = model)

  new(
    "DesignArm",
    name = name,
    active = FALSE,
    borrow = borrow,
    open_when = NoArmCondition(),
    design = design
  )
}

#' Construct a simplified DesignArm with hardcoded rule objects
#'
#' @param data (`Data` or `DataCombo`)\cr arm data for historical arms.
#' @param model (`GeneralModel` or `TwoDrugsCombo`)\cr arm model for historical arms.
#' @return A `Design` object with hardcoded rules for historical arms. The rules
#'   don't matter because they won't be used, but they are required to construct a `Design` object.
#'
#' @keywords internal
h_historical_arm_design <- function(data, model) {
  if (is.null(data) || is.null(model)) {
    stop("Historical arms must supply both `data` and `model`.")
  }

  if (is(data, "Data")) {
    assert_class(model, "GeneralModel")

    return(Design(
      model = model,
      nextBest = .NextBestNCRM(),
      stopping = StoppingMinPatients(nPatients = max(1L, data@nObs)),
      increments = IncrementsRelative(intervals = 0, increments = 1),
      cohort_size = CohortSizeConst(size = 1L),
      data = data,
      startingDose = min(data@doseGrid)
    ))
  }

  if (is(data, "DataCombo")) {
    assert_class(model, "TwoDrugsCombo")

    return(DesignCombo(
      model = model,
      nextBest = .NextBestNCRM(),
      stopping = StoppingMinPatients(nPatients = max(1L, data@nObs)),
      increments = IncrementsComboCartesian(
        drug1 = IncrementsRelative(intervals = 0, increments = 1),
        drug2 = IncrementsRelative(intervals = 0, increments = 1)
      ),
      cohort_size = CohortSizeConst(size = 1L),
      data = data,
      startingDose = vapply(data@doseGrid, min, numeric(1L))
    ))
  }

  stop("Historical arms require `data` to be `Data` or `DataCombo`.")
}

## default constructor ----

#' @rdname DesignArm-class
#' @note Typically, end users will not use the `.DefaultDesignArm()` function.
#' @export
.DefaultDesignArm <- function() {
  DesignArm(
    name = "Arm",
    design = .DefaultDesign()
  )
}

# HierarchicalDesign ----

## class ----

#' `HierarchicalDesign`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`HierarchicalDesign`] stores a collection of named [`DesignArm`] objects
#' and derives the corresponding [`HierarchicalData`] and [`HierarchicalModel`]
#' objects from them.
#'
#' @slot arms (`list`)\cr a named list of [`DesignArm`] objects.
#' @slot data (`HierarchicalData`)\cr the hierarchical data object assembled
#'   from the arm-specific design data.
#' @slot model (`HierarchicalModel`)\cr the hierarchical model object assembled
#'   from the arm-specific design models.
#'
#' @aliases HierarchicalDesign
#' @export
#'
.HierarchicalDesign <- setClass(
  Class = "HierarchicalDesign",
  slots = c(
    arms = "list",
    data = "HierarchicalData",
    model = "HierarchicalModel"
  ),
  prototype = prototype(
    arms = list(),
    data = .HierarchicalData(),
    model = .HierarchicalModel()
  ),
  contains = "CrmPackClass",
  validity = v_hierarchical_design
)

## constructor ----

#' @rdname HierarchicalDesign-class
#'
#' @param ... [`DesignArm`] objects describing the trial arms.
#'   These can be created with [`DesignArm()`] or [`HistoricalArm()`].
#' @param exchangeable_parameters (`list`)\cr see
#'   [`HierarchicalModel()`].
#' @param pool_correlations (`list`)\cr see [`HierarchicalModel()`].
#' @param pool_priors (`list`)\cr see [`HierarchicalModel()`].
#'
#' @export
HierarchicalDesign <- function(
  ...,
  exchangeable_parameters = list(),
  pool_correlations = list(),
  pool_priors = list()
) {
  arms <- list(...)
  assert_list(arms, types = "DesignArm", any.missing = FALSE, min.len = 2L)
  arm_names <- unname(vapply(arms, function(arm) arm@name, character(1L)))
  assert_character(arm_names, unique = TRUE, any.missing = FALSE)
  names(arms) <- arm_names

  data <- do.call(
    HierarchicalData,
    lapply(arms, function(arm) arm@design@data)
  )
  model <- do.call(
    HierarchicalModel,
    c(
      lapply(arms, function(arm) arm@design@model),
      list(
        exchangeable_parameters = exchangeable_parameters,
        pool_correlations = pool_correlations,
        pool_priors = pool_priors
      )
    )
  )

  .HierarchicalDesign(
    arms = arms,
    data = data,
    model = model
  )
}

## default constructor ----

#' @rdname HierarchicalDesign-class
#' @note Typically, end users will not use the `.DefaultHierarchicalDesign()`
#'   function directly.
#' @export
.DefaultHierarchicalDesign <- function() {
  HierarchicalDesign(
    arm1 = DesignArm(
      name = "arm1",
      design = .DefaultDesign()
    ),
    arm2 = DesignArm(
      name = "arm2",
      design = .DefaultDesign()
    ),
    exchangeable_parameters = list(
      intercept = list(
        arm1 = "alpha0",
        arm2 = "alpha0"
      ),
      slope = list(
        arm1 = "alpha1",
        arm2 = "alpha1"
      )
    )
  )
}
