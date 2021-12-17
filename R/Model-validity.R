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
validate_general_model <- function(object) {
  o <- Validate()
  o$check(
    h_check_fun_formals(object@init, allowed = object@datanames),
    "Arguments of the init function must be data names"
  )
  o$result()
}

#' @describeIn validate_model_objects validates that the names of the
#'   arguments in `dose` and `prob` functions contains `prob` and `dose`
#'   respectively, as well as they match `sample` slot of the `object`.
validate_model <- function(object) {
  o <- Validate()

  o$check(
    h_check_fun_formals(object@dose, mandatory = "prob", allowed = object@sample),
    "Arguments of dose function are incorrect"
  )
  o$check(
    h_check_fun_formals(object@prob, mandatory = "dose", allowed = object@sample),
    "Arguments of prob function are incorrect"
  )
  o$result()
}
