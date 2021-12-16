#' Internal Helper Functions for Validation of [`AllModels`] Objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These functions are only used internally to validate the format of an input
#' [`AllModels`] or inherited classes and therefore not exported.
#'
#' @name validate_model_objects
#' @param object (`AllModels`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn validate_model_objects validates that the names of the
#'   arguments in `init` function are included in `datanames` slot.
validate_model_general <- function(object) {
  o <- Validate()
  o$check(
    all(names(formals(object@init)) %in% object@datanames),
    "Arguments of the init function must be data names"
  )
  o$result()
}
