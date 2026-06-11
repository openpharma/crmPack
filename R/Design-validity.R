# Design ----

#' Internal Helper Functions for Validation of [`RuleDesign`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`RuleDesign`] or inherited classes and therefore not exported.
#'
#' @name v_design
#' @param object (`RuleDesign`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_design validates that the [`RuleDesign`] object
#'   contains valid `startingDose`.
v_rule_design <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@startingDose, finite = TRUE),
    "startingDose must be a number"
  )
  v$check(
    test_subset(
      object@startingDose,
      choices = object@data@doseGrid,
      empty.ok = FALSE
    ),
    "startingDose must be included in data@doseGrid"
  )
  v$result()
}

#' Internal Helper Functions for Validation of [`RuleDesignOrdinal`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`RuleDesignOrdinal`] or inherited classes and therefore not exported.
#'
#' @name v_design
#' @param object (`RuleDesignOrdinal`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_design validates that the [`RuleDesignOrdinal`] object
#'   contains valid `starting_dose`.
v_rule_design_ordinal <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@starting_dose, finite = TRUE),
    "starting_dose must be a number"
  )
  v$check(
    test_subset(
      object@starting_dose,
      choices = object@data@doseGrid,
      empty.ok = FALSE
    ),
    "starting_dose must be included in data@doseGrid"
  )
  v$result()
}

#' @describeIn v_design validates that the [`DesignCombo`] object
#'   contains valid `startingDose`.
v_design_combo <- function(object) {
  v <- Validate()
  v$check(
    test_numeric(
      object@startingDose,
      len = 2L,
      any.missing = FALSE,
      finite = TRUE
    ),
    "startingDose must be a numeric vector of length 2"
  )

  if (
    test_numeric(
      object@startingDose,
      len = 2L,
      any.missing = FALSE,
      finite = TRUE
    )
  ) {
    v$check(
      object@startingDose[1] %in% object@data@doseGrid[[1]],
      "startingDose[1] must be included in data@doseGrid[[1]]"
    )
    v$check(
      object@startingDose[2] %in% object@data@doseGrid[[2]],
      "startingDose[2] must be included in data@doseGrid[[2]]"
    )
  }

  v$check(
    test_true(identical(
      names(object@startingDose),
      names(object@data@doseGrid)
    )),
    "startingDose must have the same names as data@doseGrid"
  )

  v$result()
}


#' @describeIn v_design validates that the [`DesignGrouped`] object
#'   contains valid flags.
v_design_grouped <- function(object) {
  v <- Validate()
  v$check(
    test_flag(object@first_cohort_mono_only),
    "first_cohort_mono_only must be a flag"
  )
  v$check(
    test_flag(object@same_dose_for_all),
    "same_dose_for_all must be a flag"
  )
  v$check(
    test_flag(object@same_dose_for_all),
    "same_dose_for_start must be a flag"
  )
  v$result()
}

#' @describeIn v_design validates that the [`DesignArm`] object contains valid slots.
v_design_arm <- function(object) {
  v <- Validate()
  v$check(
    test_multi_class(object@design, classes = c("Design", "DesignCombo")),
    "design must be a Design or DesignCombo object"
  )
  v$check(
    test_string(object@name),
    "name must be a single string"
  )
  v$check(
    test_flag(object@active),
    "active must be a flag"
  )
  v$result()
}

#' @describeIn v_design validates that the [`HierarchicalDesign`] object
#'   contains valid hierarchical arm metadata.
v_hierarchical_design <- function(object) {
  v <- Validate()
  v$check(
    test_list(object@arms, types = "DesignArm", any.missing = FALSE),
    "arms must be a list of DesignArm objects without missings"
  )
  v$check(
    test_names(names(object@arms), type = "unique"),
    "arms must be a named list with unique names"
  )

  if (length(object@arms) > 0) {
    arm_names <- names(object@arms)

    v$check(
      identical(
        arm_names,
        unname(vapply(object@arms, function(arm) arm@name, character(1L)))
      ),
      "the names of arms must match the name slot of each DesignArm"
    )
    v$check(
      identical(arm_names, names(object@data@arms)),
      "data arms must have the same names as the design arms"
    )
    v$check(
      identical(arm_names, names(object@model@models_to_arms)),
      "model arms must have the same names as the design arms"
    )
    v$check(
      identical(
        object@data@arms,
        lapply(object@arms, function(arm) arm@design@data)
      ),
      "data arms must be derived from the data slot of each DesignArm"
    )
    v$check(
      identical(
        object@model@models_to_arms,
        lapply(object@arms, function(arm) arm@design@model)
      ),
      "model arms must be derived from the model slot of each DesignArm"
    )
  }

  v$result()
}
