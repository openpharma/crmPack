#' @include Design-class.R
#' @include HierarchicalDesign-validity.R
NULL


# DesignArm ----

## class ----

#' `DesignArm`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`DesignArm`] is a light-weight wrapper around `Design` object
#' for use in a [`HierarchicalDesign`].
#'
#' @slot name (`string`)\cr the name of the arm.
#' @slot active (`flag`)\cr whether the arm is enrolling or not (historical).
#' @slot borrow (`flag`)\cr whether this arm may borrow information from other
#'   arms when making dose escalation decisions. Only relevant if `active = TRUE`.
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
    design = "ANY"
  ),
  prototype = prototype(
    name = "Arm",
    active = TRUE,
    borrow = TRUE,
    design = .Design()
  ),
  contains = "CrmPackClass",
  validity = v_design_arm
)

## constructor ----

#' @rdname DesignArm-class
#'
#' @param name (`string`)\cr see slot definition.
#' @param active (`flag`)\cr see slot definition.
#' @param design (`Design`)\cr see slot definition.
#' @param borrow (`flag`)\cr see slot definition.
#'
#' @export
#' @example examples/Design-class-DesignArm.R
#'
DesignArm <- function(name, active, design, borrow = TRUE) {
  new(
    "DesignArm",
    name = name,
    active = active,
    borrow = borrow,
    design = design
  )
}

## default constructor ----

#' @rdname DesignArm-class
#' @note Typically, end users will not use the `.DefaultDesignArm()` function.
#' @export
.DefaultDesignArm <- function() {
  DesignArm(
    name = "Arm",
    active = TRUE,
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
#' @param exchangeable_parameters (`list`)\cr see
#'   [`HierarchicalModel()`].
#'
#' @export
HierarchicalDesign <- function(
  ...,
  exchangeable_parameters = list()
) {
  arms <- list(...)
  assert_list(arms, types = "DesignArm", any.missing = FALSE, min.len = 2L)
  arm_names <- unname(vapply(arms, function(arm) arm@name, character(1L)))
  assert_character(arm_names, unique = TRUE, any.missing = FALSE)
  names(arms) <- arm_names

  data <- HierarchicalData(
    arms = lapply(arms, function(arm) arm@design@data)
  )
  model <- do.call(
    HierarchicalModel,
    c(
      lapply(arms, function(arm) arm@design@model),
      list(exchangeable_parameters = exchangeable_parameters)
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
      active = TRUE,
      design = .DefaultDesign()
    ),
    arm2 = DesignArm(
      name = "arm2",
      active = TRUE,
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
