#' Simulation Reporting Accessors
#'
#' Extract unformatted operating characteristics without rerunning simulations.
#' Per-dose summaries include unvisited grid doses, counting zero patients and
#' DLTs in trials that did not visit a dose. Placebo observations are included.
#' These accessors support single-agent binary-toxicity simulations; combination,
#' hierarchical, and ordinal outcomes are not supported.
#'
#' @param sims A nonempty [`GeneralSimulations`] object with binary [`Data`]
#'   observations. `report_stop_reasons()` also accepts a simulation summary
#'   with a matrix `stop_report` slot.
#' @param quantiles Numeric probabilities at which to summarize sample size.
#' @param target Increasing length-two toxicity interval, including both bounds.
#' @param stop_categories Optional named list mapping category names to stopping
#'   rule report labels. Categories count trials where any mapped rule fired,
#'   rather than adding percentages. Unmapped labels remain as individual rows.
#'   With `NULL`, use class-based PMDA-style defaults: `Toxicity` for
#'   `StoppingLowestDoseHSRBeta`, `Max patients` for `StoppingMinPatients` and
#'   `StoppingMinCohorts`, and `Other` for other bundled rules. Missing-dose,
#'   composite and wrapper rules are `Other`: their class alone does not prove
#'   toxicity. Unknown classes and legacy reports without metadata retain their
#'   labels. `list()` requests individual rules. These are rule activations,
#'   not mutually exclusive causes of termination; minimum-size rules may also
#'   be gates inside composite rules.
#'   Set stopping rules' `report_label`s to obtain readable output.
#'
#' @return A tibble, depending on the accessor called:
#'
#'   - Dose selection has `dose` (`NA` for no recommendation), `n`, and `percent`;
#'       its denominator includes all trials.
#'   - Per-dose patient summaries have `dose`, `mean`, `sd`, `min`, `max`; DLT summaries have
#'     `dose`, `mean`, `sd`. Sample size has `mean`, `sd`, `min`, `max` and
#'     quantile columns named `q<percent>`. SD is `NA` for one trial.
#'   - Stop reasons have `label`, `rule_class`, `n`, `percent`; overlapping rules
#'     or categories need not sum to 100. Missing stop outcomes propagate.
#'   - MTD classification has `classification`, `n`, `percent`, with categories
#'     `correct`, `overestimated`, `underestimated`, `none`. Only missing final
#'     recommendations count as `none`; truth is evaluated only at selected doses.
#'
#' @examples
#' sims <- GeneralSimulations(
#'   data = list(Data(x = c(1, 1), y = c(0, 1), doseGrid = 1:3)),
#'   doses = 2, seed = 1L, truth = function(dose) dose / 10
#' )
#' report_dose_selection(sims)
#' report_patients_per_dose(sims)
#' report_dlts_per_dose(sims)
#' report_sample_size(sims)
#' report_mtd_classification(sims, target = c(0.2, 0.35))
#' @name report_simulations
NULL

#' Validate Single-Agent Reporting Inputs
#' @param sims Simulation object.
#' @return The common dose grid, invisibly.
#' @keywords internal
h_report_dose_grid <- function(sims) {
  assert_class(sims, "GeneralSimulations")
  if (!length(sims@data)) {
    stop("Reporting requires at least one simulated trial.")
  }
  grid <- sims@data[[1L]]@doseGrid
  for (data in sims@data) {
    if (
      !is(data, "Data") || is(data, "DataOrdinal") || !all(data@y %in% c(0, 1))
    ) {
      stop("Reporting requires single-agent binary-toxicity Data objects.")
    }
    if (!identical(data@doseGrid, grid)) {
      stop("All simulated trials must use the same dose grid.")
    }
  }
  invisible(grid)
}

#' Count Observations at Each Dose in Each Trial
#' @param doses List of numeric dose vectors.
#' @param grid Common dose grid.
#' @return A dose-by-trial matrix of counts.
#' @keywords internal
h_report_dose_counts <- function(doses, grid) {
  counts <- vapply(
    doses,
    function(x) {
      index <- match_within_tolerance(x, grid)
      if (anyNA(index)) {
        stop("Observed doses must match the dose grid.")
      }
      tabulate(index, nbins = length(grid))
    },
    integer(length(grid))
  )
  matrix(counts, nrow = length(grid), ncol = length(doses))
}

#' Calculate Empirical Percentages
#' @param x Numeric observations; missing values remain in the denominator.
#' @param levels Numeric values to include, including missing values if needed.
#' @return A tibble with dose, count, and percent columns.
#' @keywords internal
h_report_percentages <- function(x, levels = sort(unique(x))) {
  counts <- tabulate(match(x, levels), nbins = length(levels))
  tibble::tibble(dose = levels, n = counts, percent = counts / length(x) * 100)
}

#' @rdname report_simulations
#' @export
report_dose_selection <- function(sims) {
  grid <- h_report_dose_grid(sims)
  selected <- sims@doses
  index <- match_within_tolerance(selected, grid)
  if (any(is.na(index) & !is.na(selected))) {
    stop(
      "Selected doses must match the dose grid or be NA (no recommendation)."
    )
  }
  h_report_percentages(grid[index], c(grid, NA_real_))
}

#' Summarize Per-Trial Counts
#' @param counts Dose-by-trial matrix.
#' @param grid Dose grid.
#' @return A tibble of dose-wise count summaries.
#' @keywords internal
h_report_count_summary <- function(counts, grid) {
  tibble::tibble(
    dose = grid,
    mean = rowMeans(counts),
    sd = apply(counts, 1L, stats::sd),
    min = apply(counts, 1L, min),
    max = apply(counts, 1L, max)
  )
}

#' @rdname report_simulations
#' @export
report_patients_per_dose <- function(sims) {
  grid <- h_report_dose_grid(sims)
  h_report_count_summary(
    h_report_dose_counts(lapply(sims@data, slot, "x"), grid),
    grid
  )
}

#' @rdname report_simulations
#' @export
report_dlts_per_dose <- function(sims) {
  grid <- h_report_dose_grid(sims)
  doses <- lapply(sims@data, function(data) data@x[data@y == 1])
  result <- h_report_count_summary(h_report_dose_counts(doses, grid), grid)
  result[c("dose", "mean", "sd")]
}

#' @rdname report_simulations
#' @export
report_sample_size <- function(sims, quantiles = c(0.025, 0.5, 0.975)) {
  h_report_dose_grid(sims)
  assert_numeric(
    quantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    unique = TRUE
  )
  n <- vapply(sims@data, function(data) length(data@x), integer(1))
  values <- c(mean = mean(n), sd = stats::sd(n), min = min(n), max = max(n))
  qs <- stats::quantile(n, quantiles, names = FALSE)
  names(qs) <- if (length(quantiles)) {
    paste0("q", 100 * quantiles)
  } else {
    character()
  }
  tibble::as_tibble_row(as.list(c(values, qs)))
}

#' @rdname report_simulations
#' @export
report_mtd_classification <- function(sims, target) {
  h_report_dose_grid(sims)
  assert_numeric(target, len = 2L, lower = 0, upper = 1, any.missing = FALSE)
  if (target[1L] >= target[2L]) {
    stop("target must be strictly increasing.")
  }
  truth <- get_simulation_truth(sims)
  assert_function(truth)
  selected <- !is.na(sims@doses)
  probs <- if (any(selected)) truth(sims@doses[selected]) else numeric()
  assert_numeric(
    probs,
    len = sum(selected),
    lower = 0,
    upper = 1,
    any.missing = FALSE
  )
  category <- rep("none", length(selected))
  category[selected] <- ifelse(
    probs < target[1L],
    "underestimated",
    ifelse(probs > target[2L], "overestimated", "correct")
  )
  levels <- c("correct", "overestimated", "underestimated", "none")
  n <- tabulate(match(category, levels), nbins = length(levels))
  tibble::tibble(
    classification = levels,
    n = n,
    percent = n / length(selected) * 100
  )
}

#' @rdname report_simulations
#' @export
report_stop_reasons <- function(sims, stop_categories = NULL) {
  if (
    !isS4(sims) ||
      !methods::.hasSlot(sims, "stop_report") ||
      !is.matrix(sims@stop_report)
  ) {
    stop("sims must have a matrix stop_report slot.")
  }
  h_report_stop_reasons(
    sims@stop_report,
    if (is.null(stop_categories)) {
      h_default_stop_categories(sims@stop_report)
    } else {
      stop_categories
    }
  )
}

#' Summarize Stopping Outcomes
#' @param report Matrix of stopping outcomes.
#' @param categories Optional named list mapping categories to labels.
#' @return A tibble of rule or category activation percentages.
#' @keywords internal
h_report_stop_reasons <- function(report, categories = NULL) {
  labels <- colnames(report)
  keep <- which(!is.na(labels))
  labels <- labels[keep]
  classes <- attr(report, "stopping_classes")
  groups <- as.list(keep)
  names(groups) <- labels
  if (!is.null(categories)) {
    assert_list(categories, names = "unique")
    if (any(!nzchar(names(categories)))) {
      stop("Category names must be nonempty.")
    }
    for (category in categories) {
      assert_character(category, min.len = 1L, any.missing = FALSE)
      if (!all(category %in% labels)) {
        stop("Unknown stop report label in stop_categories.")
      }
    }
    mapped <- unique(unlist(categories, use.names = FALSE))
    groups <- c(
      lapply(categories, function(x) keep[labels %in% x]),
      groups[!labels %in% mapped]
    )
  }
  n <- vapply(
    groups,
    function(index) {
      sum(apply(report[, index, drop = FALSE], 1L, any))
    },
    numeric(1)
  )
  rule_class <- vapply(
    groups,
    function(index) {
      if (length(index) == 1L && !is.null(classes)) {
        unname(classes[colnames(report)[index]])
      } else {
        NA_character_
      }
    },
    character(1)
  )
  tibble::tibble(
    label = as.character(names(groups)),
    rule_class = unname(rule_class),
    n = unname(n),
    percent = unname(n) / nrow(report) * 100
  )
}

#' Resolve Default Stopping Categories from Rule Classes
#' @param report Logical stop report matrix with optional class metadata.
#' @return Named list mapping default categories to report labels.
#' @keywords internal
#' @noRd
h_default_stop_categories <- function(report) {
  classes <- attr(report, "stopping_classes")
  labels <- colnames(report)
  classes <- classes[labels]
  # Explicitly enumerate bundled classes; unknown/custom classes stay unmapped.
  other <- c(
    "StoppingMissingDose",
    "StoppingCohortsNearDose",
    "StoppingDoseStabilized",
    "StoppingPatientsNearDose",
    "StoppingTargetProb",
    "StoppingMTDdistribution",
    "StoppingMTDCV",
    "StoppingTargetBiomarker",
    "StoppingSpecificDose",
    "StoppingHighestDose",
    "StoppingTDCIRatio",
    "StoppingMaxGainCIRatio",
    "StoppingList",
    "StoppingAll",
    "StoppingAny",
    "StoppingOrdinal",
    "StoppingExternal"
  )
  groups <- list(
    Toxicity = labels[!is.na(classes) & classes == "StoppingLowestDoseHSRBeta"],
    `Max patients` = labels[
      !is.na(classes) &
        classes %in% c("StoppingMinPatients", "StoppingMinCohorts")
    ],
    Other = labels[!is.na(classes) & classes %in% other]
  )
  groups <- lapply(groups, function(x) x[!is.na(x)])
  groups[lengths(groups) > 0L]
}
