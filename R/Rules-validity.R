#' @describeIn validate_rules_objects validates that the [`StoppingMTDCV`] object contains
#'   valid elements with respect to their types, dependency and length.
validate_stopping_mtd_cv <- function(object) {
  o <- Validate()
  o$check(
    is.probability(object@target, bounds = FALSE),
    "target must be probability > 0 and < 1"
  )
  o$check(
    is.probability(object@threshCV/100, bounds = FALSE),
    "threshCV must be percentage > 0"
  )
  o$result()
}