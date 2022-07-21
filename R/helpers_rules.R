#' Calculating the Information Theoretic Distance
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which provides the value of the
#' divergence as given by equation in (7) in the reference at
#' https://doi.org/10.1002/sim.8450.
#'
#' @param prob (`numeric`)\cr vector or matrix with probabilities of a DLT occurring.
#' @param target (`number `)\cr single target probability of a DLT.
#' @param asymmetry (`number`)\cr describes the rate of penalization
#'   for overly toxic does, range 0 to 2.
#'
#' @export
#' @examples
#' h_info_theory_dist(c(0.5, 0.2), 0.4, 1.2)
h_info_theory_dist <- function(prob, target, asymmetry) {
  assert_probabilities(prob)
  assert_true(test_vector(prob) || test_matrix(prob))
  assert_number(target, finite = TRUE)
  assert_number(asymmetry, lower = 0, upper = 2)

  ((prob - target)^2) / (((prob^asymmetry) * (1 - prob)^(2 - asymmetry)))
}

#' Building the Plot for nextBest-NextBestTDsamples Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for nextBest-NextBestTDsamples Method.
#'
#' @param target_in_trial_samples (`numeric`)\cr vector of in-trial samples.
#' @param target_trial_end_samples (`numeric`)\cr vector of end-of-trial samples.
#' @param target_in_trial_est (`number`)\cr target in-trial estimate.
#' @param target_trial_end_est (`number`)\cr target end-of-trial estimate.
#' @param dose_grid_range (`numeric`)\cr range of dose grid.
#' @param nextBest (`NextBestTDsamples`)\cr the rule for the next best dose.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param next_best_dose (`number`)\cr next best dose.
#'
#' @export
#'
h_next_best_tdsamples_plot <- function(target_in_trial_samples,
                                       target_trial_end_samples,
                                       target_in_trial_est,
                                       target_trial_end_est,
                                       dose_grid_range,
                                       nextBest,
                                       doselimit,
                                       next_best_dose) {
  assert_numeric(target_in_trial_samples, any.missing = FALSE)
  assert_numeric(target_trial_end_samples, any.missing = FALSE)
  assert_number(target_in_trial_est)
  assert_number(target_trial_end_est)
  assert_numeric(dose_grid_range, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE)
  assert_class(nextBest, "NextBestTDsamples")
  assert_number(doselimit)
  assert_number(next_best_dose, na.ok = TRUE)

  p <- ggplot(
    data = rbind(
      data.frame(period = "during", TD = target_in_trial_samples),
      data.frame(period = "end", TD = target_trial_end_samples)
    ),
    aes(x = .data$TD, colour = .data$period),
  ) +
    geom_density(fill = "grey50") +
    coord_cartesian(xlim = dose_grid_range) +
    scale_color_manual(values = c(during = "grey50", end = "violet")) +
    theme(legend.position = "none") +
    ylab("Posterior density") +
    geom_vline(xintercept = target_in_trial_est, colour = "orange", lwd = 1.1) +
    annotate(
      geom = "text",
      label = paste("TD", nextBest@targetDuringTrial * 100, "Estimate"),
      x = target_in_trial_est,
      y = 0,
      hjust = -0.1,
      vjust = -20,
      size = 5,
      colour = "orange"
    ) +
    geom_vline(xintercept = target_trial_end_est, colour = "violet", lwd = 1.1) +
    annotate(
      geom = "text",
      label = paste("TD", nextBest@targetEndOfTrial * 100, "Estimate"),
      x = target_trial_end_est,
      y = 0,
      hjust = -0.1,
      vjust = -25,
      size = 5,
      colour = "violet"
    )

  maxdoselimit <- min(doselimit, dose_grid_range[2])

  p +
    geom_vline(xintercept = maxdoselimit, colour = "red", lwd = 1.1) +
    geom_text(
      data = data.frame(x = maxdoselimit),
      aes(x, 0, label = "Max", hjust = +1, vjust = -35),
      angle = 90,
      vjust = 1.5,
      hjust = 0.5,
      colour = "red"
    ) +
    geom_vline(xintercept = next_best_dose, colour = "blue", lwd = 1.1) +
    geom_text(
      data = data.frame(x = next_best_dose),
      aes(x, 0, label = "Next", hjust = 0.1, vjust = -30),
      angle = 90,
      vjust = -0.5,
      hjust = 0.5,
      colour = "blue"
    )
}

#' Building the Plot for nextBest-NextBestTD Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for nextBest-NextBestTD Method.
#'
#' @param prob_target_drt (`proportion`)\cr target DLT probability during the trial.
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param prob_target_eot (`proportion`)\cr target DLT probability at the end of the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param data (`Data`)\cr the data object from which the dose grid will be fetched.
#' @param prob_dlt (`numeric`)\cr
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param next_dose (`number`)\cr next best dose.
#'
#' @export
#'
h_next_best_td_plot <- function(prob_target_drt,
                                dose_target_drt,
                                prob_target_eot,
                                dose_target_eot,
                                data,
                                prob_dlt,
                                doselimit,
                                next_dose) {
  assert_probability(prob_target_drt)
  assert_number(dose_target_drt)
  assert_probability(prob_target_eot)
  assert_number(dose_target_eot)
  assert_class(data, "Data")
  assert_probabilities(prob_dlt, len = data@nGrid)
  assert_number(doselimit)
  assert_number(next_dose, na.ok = TRUE)

  dose_grid_range <- c(
    data@doseGrid[ifelse(data@placebo && data@nGrid >= 2, 2, 1)],
    data@doseGrid[data@nGrid]
  )

  p <- ggplot(
    data = data.frame(x = data@doseGrid, y = prob_dlt),
    aes(x = .data$x, y = .data$y)
  ) +
    geom_line(colour = "red", size = 1.5) +
    coord_cartesian(xlim = c(0, dose_grid_range[2])) +
    ylim(c(0, 1)) +
    xlab("Dose Levels") +
    ylab("Probability of DLT")

  if (h_in_range(dose_target_drt, range = dose_grid_range, bounds_closed = TRUE)) {
    p <- p +
      geom_point(
        data = data.frame(x = dose_target_drt, y = prob_target_drt),
        aes(x = .data$x, y = .data$y),
        colour = "orange",
        shape = 15,
        size = 8
      ) +
      annotate(
        geom = "text",
        label = paste("TD", prob_target_drt * 100, "Estimate"),
        x = dose_target_drt + 1,
        y = prob_target_drt - 0.2,
        size = 5,
        colour = "orange"
      )
  }

  if (h_in_range(dose_target_eot, range = dose_grid_range, bounds_closed = TRUE)) {
    p <- p +
      geom_point(
        data = data.frame(x = dose_target_eot, y = prob_target_eot),
        aes(x = .data$x, y = .data$y),
        colour = "violet",
        shape = 16,
        size = 8
      ) +
      annotate(
        geom = "text",
        label = paste("TD", prob_target_eot * 100, "Estimate"),
        x = dose_target_eot + 1,
        y = prob_target_eot - 0.1,
        size = 5,
        colour = "violet"
      )
  }

  maxdoselimit <- min(doselimit, dose_grid_range[2])

  p +
    geom_vline(xintercept = maxdoselimit, colour = "brown", lwd = 1.1) +
    geom_text(
      data = data.frame(x = maxdoselimit, y = 0),
      aes(x = .data$x, y = .data$y, label = "Max", hjust = +1, vjust = -30),
      angle = 90,
      vjust = 1.5,
      hjust = 0.5,
      colour = "brown",
    ) +
    geom_vline(xintercept = next_dose, colour = "purple", lwd = 1.1) +
    geom_text(
      data = data.frame(x = next_dose, y = 0),
      aes(x = .data$x, y = .data$y, label = "Next", hjust = 0, vjust = -30),
      angle = 90,
      vjust = -0.5,
      hjust = 0.5,
      colour = "purple"
    )
}
