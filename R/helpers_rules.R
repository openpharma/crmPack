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
  assert_numeric(target_in_trial_samples, finite = TRUE, any.missing = FALSE)
  assert_numeric(target_trial_end_samples, finite = TRUE, any.missing = FALSE)
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

  p <- p +
    geom_vline(xintercept = maxdoselimit, colour = "red", lwd = 1.1) +
    geom_text(
      data = data.frame(x = maxdoselimit),
      aes(x, 0, label = "Max", hjust = +1, vjust = -35),
      colour = "red"
    ) +
    geom_vline(xintercept = next_best_dose, colour = "blue", lwd = 1.1) +
    geom_text(
      data = data.frame(x = next_best_dose),
      aes(x, 0, label = "Next", hjust = 0.1, vjust = -30),
      colour = "blue"
    )
}
