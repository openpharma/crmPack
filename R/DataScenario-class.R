#' @include Data-class.R Samples-class.R
NULL

#' Validate an Evaluated Data Scenario
#' @param object Evaluated scenario.
#' @return TRUE or validation messages.
#' @keywords internal
#' @noRd
v_data_scenario <- function(object) {
  if (
    !is(object@data, "Data") &&
      !is(object@data, "DataCombo") &&
      !is(object@data, "HierarchicalData")
  ) {
    return("data must be Data, DataCombo, DataDA, or HierarchicalData")
  }
  hierarchical <- is(object@data, "HierarchicalData")
  arms <- if (hierarchical) names(object@data@arms) else "overall"
  if (hierarchical != is(object@samples, "HierarchicalSamples")) {
    return("data and samples must have compatible hierarchical structure")
  }
  if (hierarchical && !identical(names(object@samples@arm_samples), arms)) {
    return("sample arms must match data arms in order")
  }
  for (field in setdiff(slotNames(object), c("data", "samples"))) {
    if (!identical(names(slot(object, field)), arms)) {
      return(paste(field, "must have identical arm names and ordering"))
    }
  }
  TRUE
}

#' Evaluated Hypothetical Data Scenario
#'
#' Returned by [scenario()]. Decision slots are lists indexed by arm, using
#' `"overall"` for a non-hierarchical result. Rule-specific contents are retained.
#' @slot data Evaluated data.
#' @slot samples Posterior samples compatible with the data.
#' @slot fit Per-arm posterior fits.
#' @slot dose_limit Per-arm increment limits.
#' @slot next_best Per-arm complete rule results, or NULL for unevaluated arms.
#' @slot next_dose Per-arm proposed doses.
#' @slot cohort_size Per-arm active cohort sizes.
#' @slot placebo_cohort_size Per-arm placebo cohort sizes, or NULL.
#' @slot stop Per-arm stop decisions.
#' @slot stop_report Per-arm stopping rule results.
#' @slot stop_reason Per-arm stopping messages.
#' @export
.DataScenario <- setClass(
  "DataScenario",
  contains = "CrmPackClass",
  slots = c(
    data = "CrmPackClass",
    samples = "Samples",
    fit = "list",
    dose_limit = "list",
    next_best = "list",
    next_dose = "list",
    cohort_size = "list",
    placebo_cohort_size = "list",
    stop = "list",
    stop_report = "list",
    stop_reason = "list"
  ),
  validity = v_data_scenario
)

#' Normalize Scenario Decision Slots
#' @param data Evaluated data.
#' @param samples Posterior samples.
#' @param ... Decision slots.
#' @return A DataScenario.
#' @keywords internal
#' @noRd
h_data_scenario <- function(data, samples, ...) {
  fields <- list(...)
  if (!is(data, "HierarchicalData")) {
    fields <- lapply(fields, function(value) list(overall = value))
  }
  do.call(.DataScenario, c(list(data = data, samples = samples), fields))
}

#' Normalize a Scenario Dose Recommendation
#' @param next_dose Rule recommendation.
#' @param arm_data Arm data.
#' @return A numeric dose vector.
#' @keywords internal
#' @noRd
h_scenario_next_dose <- function(next_dose, arm_data) {
  if (is(arm_data, "DataCombo")) {
    if (is.matrix(next_dose)) {
      dose_value <- as.numeric(next_dose[1L, ])
      names(dose_value) <- colnames(next_dose)
      return(dose_value)
    }
    if (length(next_dose) == 1L && is.na(next_dose)) {
      dose_value <- rep(NA_real_, length(arm_data@drugNames))
      names(dose_value) <- arm_data@drugNames
      return(dose_value)
    }
    dose_value <- as.numeric(next_dose)
    if (length(dose_value) == length(arm_data@drugNames)) {
      names(dose_value) <- arm_data@drugNames
    }
    return(dose_value)
  }

  next_dose
}
