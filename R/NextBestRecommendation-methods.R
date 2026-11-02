# plot ----

## NextBestRecommendation-NextBestMTD ----

#' Plot Method for the [`NextBestRecommendation`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that creates a combined plot for a [`NextBestRecommendation`] object
#' using a NextBestMTD rule
#'
#' @inheritParams nextBest
#' @param x (`NextBestRecommendation`)\cr object we want to plot.
#' @param y (`NextBest`)\cr missing object, for compatibility with the generic
#'   function.
#' @param samples (`Samples`)\cr posterior samples from `model` parameters given
#'   `data`.
#' @param ... passed to subclass `plot` methods.
#'
#' @return A `gg` object combining the arm-specific plots, or `NULL` if no
#'   arm plot is available.
#'
#' @aliases plot-NextBestSolution-NextBestMTD
#' @export
setMethod(
  f = "plot",
  signature = signature(
    x = "NextBestRecommendation",
    y = "NextBestMTD"
  ),
  definition = function(x, y, doselimit, data, model, samples, ...) {
    dose_target_samples <- dose(x = y@target, model, samples, ...)
    dose_target <- y@derive(dose_target_samples)
    range <- range(data@doseGrid)

    colours <- c("Estimate" = "black", "Next" = "blue",     "Max" = "red")
    refData <- tibble(
      X =      c(dose_target, x@value, doselimit),
      Label =  c( "Estimate",  "Next",     "Max"),
      Nudge =  c(          0, -0.005,    0.005) * (range[2] - range[1])
    )
    p <- ggplot(
      data = data.frame(x = dose_target_samples),
      aes(.data$x)
    ) +
      geom_density(colour = "grey50") +
      coord_cartesian(xlim = range) +
      geom_vline(
        data = refData,
        aes(
          xintercept = X + Nudge,
          colour = Label
        )
      ) +
      scale_colour_manual(name = " ", values = colours) +
      # Temporary
      labs(
        caption = paste0("Target: ", round(dose_target, 1), "; Next: ", x@value, "; Max: ", doselimit)
      )

    p
  }
)

## NextBestRecommendation-NextBestEWOC ----

#' Plot Method for the [`NextBestRecommendation`] Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A method that creates a plot for a [`NextBestRecommendation`] object
#' using a NextBestEWOC rule
#'
#' @inheritParams nextBest
#' @param x (`NextBestSolution`)\cr object we want to plot.
#' @param y (`missing`)\cr missing object, for compatibility with the generic
#'   function.
#' @param samples (`Samples`)\cr posterior samples from `model` parameters given
#'   `data`.
#' @param ... passed to subclass `plot` methods.
#'
#' @return A `gg` object combining the arm-specific plots, or `NULL` if no
#'   arm plot is available.
#'
#' @aliases plot-NextBestSolution
#' @export
setMethod(
  f = "plot",
  signature = signature(
    x = "NextBestRecommendation",
    y = "NextBestEWOC"
  ),
  definition = function(x, y, doselimit, data, model, samples, ...) {
    # prob_plot_type <- match.arg(prob_plot_type)
    # dose_scale <- match.arg(dose_scale)
    # axis_ticks <- match.arg(axis_ticks)
    # safe_dose_marker <- match.arg(safe_dose_marker)
    # assert_number(axis_text_angle, finite = TRUE)
    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )

    # Estimates of posterior probabilities that are based on the prob. samples
    # which are within overdose/target interval.
    prob_overdose <- colMeans(h_in_range(
      prob_samples,
      y@overdose,
      bounds_closed = c(FALSE, TRUE)
    ))

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    ) &
      (prob_overdose <= y@max_overdose_prob)

    next_dose <- if (any(is_dose_eligible)) {
      # Take the highest eligible dose.
      next_best_level <- sum(is_dose_eligible)
      data@doseGrid[is_dose_eligible][next_best_level]
    } else {
      NA_real_
    }

    safe_dose <- if (any(is_dose_eligible)) {
      data@doseGrid[sum(is_dose_eligible)]
    } else {
      NA_real_
    }
    # overdose_base <- h_next_best_reference_lines(
    #   ggplot(),
    #   data@doseGrid,
    #   dose_scale,
    #   safe_dose = if (identical(safe_dose_marker, "line")) safe_dose else NA_real_,
    #   overdose_threshold = nextBest@max_overdose_prob * 100
    # )

    # Build plot for the overdosing probability.
    # p <- h_next_best_probability_plot(
    #   dose_grid = data@doseGrid,
    #   probability = prob_overdose,
    #   description = "Overdose probability [%]",
    #   colour = "red",
    #   prob_plot_type = prob_plot_type,
    #   dose_scale = dose_scale,
    #   axis_ticks = axis_ticks,
    #   axis_text_angle = axis_text_angle,
    #   base_plot = overdose_base
    # )
    p <- ggplot() +
           geom_linerange(
             aes(x = data@doseGrid, ymin = 0, ymax = prob_overdose),
             colour = "blue"
          )
    # if (identical(safe_dose_marker, "triangle")) {
    #   p <- h_next_best_safety_marker(
    #     p, safe_dose, prob_overdose, data@doseGrid, dose_scale
    #   )
    # }
    p
  }
)
