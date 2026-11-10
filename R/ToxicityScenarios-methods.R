#' @include ToxicityScenarios-class.R
#' @include CrmPackClass-methods.R
NULL

#' Tidy True Toxicity Scenarios
#'
#' @param x A `ToxicityScenarios` object.
#' @param ... Unused.
#' @return A `tbl_ToxicityScenarios` tibble with `scenario`, `dose`, and
#'   `prob` columns in scenario and dose-grid order. Probabilities are unrounded.
#' @rdname tidy-ToxicityScenarios
#' @export
setMethod("tidy", signature(x = "ToxicityScenarios"), function(x, ...) {
  # Validate structure and probabilities before assembling the rows.
  validObject(x)
  n <- length(x@dose_grid)
  values <- lapply(seq_along(x@scenarios), function(i) {
    h_toxicity_prob(x@scenarios[[i]], x@dose_grid, names(x@scenarios)[i])
  })
  h_tidy_class(tibble::tibble(
    scenario = rep(names(x@scenarios), each = n),
    dose = rep(x@dose_grid, times = length(x@scenarios)),
    prob = unlist(values, use.names = FALSE)
  ), x)
})

#' Plot True Toxicity Scenarios
#'
#' @param x A `ToxicityScenarios` object.
#' @param y Unused.
#' @param ... Unused.
#' @return A `ggplot` with one curve per scenario and an optional shaded target
#'   interval. Points identify the supplied dose grid; lines join those points.
#' @export
setMethod("plot", signature(x = "ToxicityScenarios", y = "missing"),
  function(x, y, ...) {
    data <- tidy(x)
    data$scenario <- factor(data$scenario, levels = names(x@scenarios))
    p <- ggplot(data, aes(x = .data$dose, y = .data$prob,
                         colour = .data$scenario, group = .data$scenario))
    if (length(x@target)) {
      p <- p + annotate("rect", xmin = -Inf, xmax = Inf,
                        ymin = x@target[1], ymax = x@target[2],
                        fill = "grey60", alpha = 0.2)
    }
    p + geom_line() + geom_point() +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Dose", y = "DLT probability", colour = "Scenario")
  }
)
