#' @include SimulationsSummaryList-class.R
NULL

#' Plot Operating Characteristics Across Scenarios
#'
#' Plot per-dose selection percentages or mean patient counts, with a panel for
#' each scenario. Counts include placebo and unvisited doses (zero patients).
#' Selection percentages use all trials as their denominator, including trials
#' without a recommendation, which appear as `None`. No summaries or MCMC are
#' recomputed. Existing single-simulation plotting methods are unaffected.
#'
#' @param x A `SimulationsSummaryList`.
#' @param y Unused.
#' @param type `"doseSelected"` (default) or `"nPatients"`.
#' @param scenario_labels Optional named scenario display labels.
#' @param scales `"fixed"` for shared dose axes, or `"free_x"` for each scenario's
#'   own dose grid. The y-axis is always shared for comparison.
#' @param ncol Optional positive integer number of facet columns.
#' @param axis_text_angle Finite angle for dose-axis labels.
#' @param ... Unused.
#' @return A `ggplot` with discrete dose columns faceted by scenario. Its data
#'   retain original scenario names, numeric doses (`NA` for None), and `value`.
#' @export
setMethod(
  "plot",
  signature(x = "SimulationsSummaryList", y = "missing"),
  function(
    x,
    y,
    type = c("doseSelected", "nPatients"),
    scenario_labels = NULL,
    scales = c("fixed", "free_x"),
    ncol = NULL,
    axis_text_angle = 45,
    ...
  ) {
    type <- match.arg(type)
    scales <- match.arg(scales)
    if (!is.null(ncol)) {
      assert_count(ncol, positive = TRUE)
    }
    assert_number(axis_text_angle, finite = TRUE)
    scenarios <- names(x@simulations)
    headers <- h_oc_headers(scenarios, overrides = scenario_labels)
    values <- dplyr::bind_rows(lapply(scenarios, function(scenario) {
      sims <- x@simulations[[scenario]]
      if (type == "doseSelected") {
        result <- report_dose_selection(sims)
        value <- result$percent
      } else {
        result <- report_patients_per_dose(sims)
        value <- result$mean
      }
      tibble::tibble(scenario = scenario, dose = result$dose, value = value)
    }))
    levels <- as.character(sort(unique(values$dose[!is.na(values$dose)])))
    if (type == "doseSelected") {
      levels <- c(levels, "None")
    }
    values$dose_label <- factor(
      ifelse(is.na(values$dose), "None", as.character(values$dose)),
      levels = levels
    )
    values$scenario <- factor(values$scenario, levels = scenarios)
    ggplot(values, aes(x = .data$dose_label, y = .data$value)) +
      geom_col(width = 0.8) +
      facet_wrap(
        vars(.data$scenario),
        scales = scales,
        ncol = ncol,
        labeller = labeller(scenario = as_labeller(headers))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = "Dose",
        y = if (type == "doseSelected") {
          "Selected dose [%]"
        } else {
          "Mean number of patients"
        }
      ) +
      theme(
        axis.text.x = element_text(
          angle = axis_text_angle,
          hjust = if (axis_text_angle == 0) 0.5 else 1
        )
      )
  }
)
