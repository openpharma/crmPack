# HierarchicalDesign ----

#' Internal Helper Functions for Validation of [`HierarchicalDesign`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`HierarchicalDesign`] or related classes and therefore not exported.
#'
#' @name v_hierarchical_design
#' @param object (`HierarchicalDesign`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_hierarchical_design validates that the [`DesignArm`] object contains valid slots.
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
  v$check(
    test_flag(object@borrow),
    "borrow must be a flag"
  )
  v$result()
}

#' @describeIn v_hierarchical_design validates that the [`HierarchicalDesign`] object
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
