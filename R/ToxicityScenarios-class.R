#' @include CrmPackClass-class.R
NULL

#' Evaluate a True Toxicity Curve
#'
#' @param fun A vectorized truth function.
#' @param dose_grid Numeric dose grid.
#' @param label Scenario name used in error messages.
#' @return Numeric probabilities, one per dose.
#' @keywords internal
#' @noRd
h_toxicity_prob <- function(fun, dose_grid, label) {
  value <- tryCatch(fun(dose_grid), error = function(e) {
    stop(sprintf("Scenario '%s' failed: %s", label, conditionMessage(e)), call. = FALSE)
  })
  if (!is.numeric(value) || !is.null(dim(value)) ||
      length(value) != length(dose_grid) || anyNA(value) ||
      any(!is.finite(value)) || any(value < 0 | value > 1)) {
    stop(sprintf(
      "Scenario '%s' must return one finite probability in [0, 1] per dose.", label
    ), call. = FALSE)
  }
  as.numeric(value)
}

#' Validate True Toxicity Scenarios
#'
#' @param object A `ToxicityScenarios` object.
#' @return `TRUE` or validation messages.
#' @keywords internal
#' @noRd
v_toxicity_scenarios <- function(object) {
  scenarios <- object@scenarios
  labels <- names(scenarios)
  if (!length(scenarios) || is.null(labels) || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(labels) ||
      !all(vapply(scenarios, is.function, logical(1)))) {
    return("scenarios must be a non-empty list of functions with unique, non-empty names")
  }
  grid <- object@dose_grid
  if (!length(grid) || !is.null(dim(grid)) || anyNA(grid) ||
      any(!is.finite(grid)) || any(grid < 0) || any(diff(grid) <= 0)) {
    return("dose_grid must contain finite, non-negative, strictly increasing doses")
  }
  target <- object@target
  if (length(target) && (length(target) != 2L || !is.null(dim(target)) ||
      anyNA(target) || any(!is.finite(target)) ||
      any(target < 0 | target > 1) || target[1] >= target[2])) {
    return("target must be empty or an increasing pair of probabilities in [0, 1]")
  }
  tryCatch({
    for (i in seq_along(scenarios)) {
      h_toxicity_prob(scenarios[[i]], grid, labels[i])
    }
    TRUE
  }, error = function(e) conditionMessage(e))
}

#' True Toxicity Scenarios
#'
#' A container for assumed true dose-toxicity curves used in simulations.
#' Functions must accept a numeric dose vector and return one finite probability
#' per dose. They should be deterministic; validation evaluates each function.
#' Combination dose grids are not supported.
#'
#' @slot scenarios Named list of vectorized truth functions.
#' @slot dose_grid Finite, non-negative, strictly increasing numeric dose grid.
#' @slot target Optional target interval, stored as an empty numeric vector
#'   when absent.
#' @export
.ToxicityScenarios <- setClass(
  "ToxicityScenarios",
  slots = c(scenarios = "list", dose_grid = "numeric", target = "numeric"),
  contains = "CrmPackClass",
  validity = v_toxicity_scenarios
)

#' @rdname ToxicityScenarios-class
#' @param scenarios Named list of vectorized truth functions.
#' @param dose_grid Finite, non-negative, strictly increasing numeric dose grid.
#' @param target Optional length-two target toxicity interval, or `NULL`.
#' @return A `ToxicityScenarios` object.
#' @examples
#' truths <- ToxicityScenarios(
#'   scenarios = list(Low = function(dose) dose / 100,
#'                    High = function(dose) dose / 50),
#'   dose_grid = c(5, 10, 20), target = c(0.2, 0.35)
#' )
#' tidy(truths)
#' plot(truths)
#' report_table(truths, format = "kable", digits = 2)
#' @export
ToxicityScenarios <- function(scenarios, dose_grid, target = NULL) {
  .ToxicityScenarios(
    scenarios = scenarios, dose_grid = dose_grid,
    target = if (is.null(target)) numeric() else target
  )
}
