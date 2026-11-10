#' @include CrmPackClass-class.R Simulations-class.R
NULL

#' Validate Scenario Names
#' @param x A named list.
#' @return Nothing; errors for empty or ambiguous names.
#' @keywords internal
#' @noRd
h_oc_names <- function(x) {
  if (
    !length(x) ||
      is.null(names(x)) ||
      anyNA(names(x)) ||
      any(!nzchar(trimws(names(x)))) ||
      anyDuplicated(names(x))
  ) {
    stop("Scenarios must have unique, nonempty names.")
  }
}

#' Validate a Simulation Summary Collection
#' @param object A `SimulationsSummaryList` object.
#' @return TRUE or a validation message.
#' @keywords internal
#' @noRd
v_simulations_summary_list <- function(object) {
  tryCatch(
    {
      h_oc_names(object@simulations)
      if (
        !identical(names(object@simulations), names(object@summaries)) ||
          !identical(names(object@simulations), names(object@stop_categories))
      ) {
        stop("Simulation, summary, and category scenario names must agree.")
      }
      if (
        !all(vapply(
          object@summaries,
          is,
          logical(1),
          "GeneralSimulationsSummary"
        ))
      ) {
        stop("summaries must contain GeneralSimulationsSummary objects.")
      }
      assert_numeric(
        object@target,
        len = 2L,
        lower = 0,
        upper = 1,
        any.missing = FALSE
      )
      if (object@target[1L] >= object@target[2L]) {
        stop("target must be strictly increasing.")
      }
      for (sims in object@simulations) {
        h_report_dose_grid(sims)
      }
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
}

#' Cached Summaries Across Simulation Scenarios
#'
#' Collect single-agent binary-toxicity simulations and compute each summary
#' once, using the truth stored on that simulation. Combination, hierarchical,
#' pseudo and dual-endpoint simulations are not supported. Scenarios may use
#' different dose grids. Counts include placebo observations.
#'
#' @slot simulations Named list of original simulation objects.
#' @slot summaries Named list of cached per-scenario summary objects.
#' @slot target Shared increasing length-two target toxicity interval.
#' @slot stop_categories Named list of resolved stopping-category mappings, one
#'   per scenario. Rules without class metadata remain individual labels.
#' @export
.SimulationsSummaryList <- setClass(
  "SimulationsSummaryList",
  slots = c(
    simulations = "list",
    summaries = "list",
    target = "numeric",
    stop_categories = "list"
  ),
  contains = "CrmPackClass",
  validity = v_simulations_summary_list
)

#' @rdname SimulationsSummaryList-class
#' @param ... Uniquely named simulation objects, or one named list of objects.
#' @param target Shared increasing length-two target toxicity interval.
#' @param stop_categories Named list mapping categories to report labels, shared
#'   across scenarios. `NULL` uses class-based defaults; `list()` keeps individual
#'   labels. Explicit mappings must reference labels present in every scenario.
#' @return A `SimulationsSummaryList` object.
#' @examples
#' sims <- GeneralSimulations(
#'   data = list(Data(x = c(1, 1), y = c(0, 1), doseGrid = 1:3),
#'               Data(x = c(1, 2), y = c(0, 0), doseGrid = 1:3)),
#'   doses = c(2, 3), seed = 1L, truth = function(dose) dose / 10
#' )
#' scenarios <- SimulationsSummaryList(Low = sims, target = c(0.2, 0.35))
#' scenarios@summaries$Low@n_obs
#' registry <- oc_stat_registry(scenarios)
#' registry$n_pat_mean$fun(sims, scenarios@summaries$Low, "Low")
#' @export
SimulationsSummaryList <- function(
  ...,
  target = c(0.2, 0.35),
  stop_categories = NULL
) {
  simulations <- list(...)
  if (length(simulations) == 1L && is.list(simulations[[1L]])) {
    simulations <- simulations[[1L]]
  }
  h_oc_names(simulations)
  assert_numeric(target, len = 2L, lower = 0, upper = 1, any.missing = FALSE)
  if (target[1L] >= target[2L]) {
    stop("target must be strictly increasing.")
  }
  categories <- lapply(simulations, function(sims) {
    if (
      is(sims, "PseudoSimulations") ||
        is(sims, "DualSimulations") ||
        is(sims, "PseudoDualSimulations")
    ) {
      stop("Pseudo and dual-endpoint simulations are not supported.")
    }
    h_report_dose_grid(sims)
    h_toxicity_prob(
      get_simulation_truth(sims),
      sims@data[[1L]]@doseGrid,
      "truth"
    )
    if (!methods::.hasSlot(sims, "stop_report")) {
      if (length(stop_categories)) {
        stop("These simulations have no stop_report labels.")
      }
      return(list())
    }
    mapping <- if (is.null(stop_categories)) {
      h_default_stop_categories(sims@stop_report)
    } else {
      stop_categories
    }
    h_report_stop_reasons(sims@stop_report, mapping)
    mapping
  })
  summaries <- lapply(simulations, summary, target = target)
  .SimulationsSummaryList(
    simulations = simulations,
    summaries = summaries,
    target = target,
    stop_categories = categories
  )
}
