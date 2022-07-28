# for nextBest methods ----

## some specific helpers ----

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

#' Find a Matrix for Variance According to Yeung et. al (2015)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which computes the matrix required for calculation of the
#' variance of the logarithm of the maximum gain dose, based on Yeung et. al (2015)
#' paper. This helper function is used by [`nextBest-NextBestMaxGain()`] method.
#'
#' @param log_dose_mg (`number`)\cr the log of dose corresponding to the maximum gain.
#' @param model (`ModelTox`)\cr the DLT model.
#' @param model_eff (`Effloglog`)\cr the efficacy model.
#'
#' @references
#'   Yeung, W.Y., Whitehead, J., Reigner, B., Beyer, U., Diack, Ch., Jaki, T. (2015),
#'   Bayesian adaptive dose-escalation procedures for binary and continuous responses utilizing a gain function,
#'   *Pharmaceutical Statistics*,
#'   \doi{10.1002/pst.1706} \cr
#'
#' @export
#'
h_delta_g_yeung <- function(log_dose_mg, model, model_eff) {
  assert_number(log_dose_mg, na.ok = TRUE)
  assert_class(model, "ModelTox")
  assert_class(model_eff, "Effloglog")

  mean_eff_mg <- model_eff@theta1 + model_eff@theta2 * log(log_dose_mg)
  denom <- model@phi2 * mean_eff_mg * (1 + model@phi2 * log_dose_mg)
  dgphi1 <- -(mean_eff_mg * log_dose_mg * model@phi2 - model_eff@theta2) / denom
  dgphi2 <- -(log_dose_mg * (mean_eff_mg * (1 + log_dose_mg * model@phi2) - model_eff@theta2)) / denom
  dgtheta1 <- -(log_dose_mg * model@phi2) / denom
  dgtheta2 <- -(exp(model@phi1 + model@phi2 * log_dose_mg) * (model@phi2 * log_dose_mg * log(log_dose_mg) - 1) - 1) / denom
  matrix(c(dgphi1, dgphi2, dgtheta1, dgtheta2), 4, 1)
}

## next best at grid ----

#' Get Closest Grid Doses for a Given Target Doses for nextBest-NextBestMaxGain Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function that for a given target doses finds the dose in grid that is
#' closest and below the target. There are four different targets in the context
#' of [`nextBest-NextBestMaxGain()`] method: min(`dose_mg`, `dose_target_drt`),
#' `dose_mg`, `dose_target_drt` or `dose_target_eot`.
#'
#' @note For placebo design, if safety allows, exclude placebo from the
#'   recommended next doses.
#'
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param dose_mg (`number`)\cr the dose corresponding to the maximum gain.
#' @param prob_target_eot (`proportion`)\cr target DLT probability at the end of the trial.
#' @param doses_eligible (`numeric`)\cr eligible doses from the grid.
#' @param placebo (`flag`)\cr if `TRUE` the first dose level in the dose grid used
#'   is considered as placebo. This is needed to adjust the max gain dose using
#'   efficacy constant value. If the `placebo` was used, then the `model_eff@const`
#'   is added to `dose_mg`.
#' @param model (`ModelTox`)\cr the DLT model.
#' @param model_eff (`Effloglog`)\cr the efficacy model.
#'
#' @export
#'
h_next_best_mg_doses_at_grid <- function(dose_target_drt,
                                         dose_target_eot,
                                         dose_mg,
                                         prob_target_eot,
                                         doses_eligible,
                                         placebo,
                                         model,
                                         model_eff) {
  assert_number(dose_target_drt, na.ok = TRUE)
  assert_number(dose_target_eot, na.ok = TRUE)
  assert_number(dose_mg, na.ok = TRUE)
  assert_probability(prob_target_eot)
  assert_numeric(doses_eligible, finite = TRUE, any.missing = FALSE)
  assert_flag(placebo)
  assert_class(model, "ModelTox")
  assert_class(model_eff, "Effloglog")

  # h_find_interval assumes that elements in doses_eligible are strictly increasing.
  next_dose_lev <- h_find_interval(min(dose_mg, dose_target_drt), doses_eligible)
  next_dose <- doses_eligible[next_dose_lev]

  next_dose_mg_lev <- h_find_interval(dose_mg, doses_eligible)
  next_dose_mg <- doses_eligible[next_dose_mg_lev]

  next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
  next_dose_drt <- doses_eligible[next_dose_lev_drt]

  next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
  next_dose_eot <- doses_eligible[next_dose_lev_eot]

  # Find the variance of the log of dose_mg.
  # First, find the covariance matrix of all the parameters, phi1, phi2, theta1 and theta2
  # given that phi1 and phi2 are independent of theta1 and theta2.
  log_dose_mg <- log(dose_mg + ifelse(placebo, model_eff@const, 0))
  delta_g <- h_delta_g_yeung(log_dose_mg, model, model_eff)
  zero_matrix <- matrix(0, 2, 2)
  cov_beta <- cbind(rbind(model@Pcov, zero_matrix), rbind(zero_matrix, model_eff@Pcov))
  var_log_dose_mg <- as.vector(t(delta_g) %*% cov_beta %*% delta_g)

  # 95% credibility interval.
  ci_dose_mg <- exp(log_dose_mg + c(-1, 1) * 1.96 * sqrt(var_log_dose_mg))
  ci_ratio_dose_mg <- as.numeric(ci_dose_mg[2] / ci_dose_mg[1])

  # Find the variance of the log of the dose_target_eot.
  mat <- matrix(
    c(
      -1 / (model@phi2),
      -(log(prob_target_eot / (1 - prob_target_eot)) - model@phi1) / (model@phi2)^2
    ),
    nrow = 1
  )
  var_dose_target_eot <- as.vector(mat %*% model@Pcov %*% t(mat))

  # 95% credibility interval.
  ci_td_eot <- exp(log(dose_target_eot) + c(-1, 1) * 1.96 * sqrt(var_dose_target_eot))
  ci_ratio_td_eot <- ci_td_eot[2] / ci_td_eot[1]

  list(
    next_dose = next_dose,
    next_dose_drt = next_dose_drt,
    next_dose_eot = next_dose_eot,
    next_dose_mg = next_dose_mg,
    ci_td_eot = ci_td_eot,
    ci_ratio_td_eot = ci_ratio_td_eot,
    ci_dose_mg = ci_dose_mg,
    ci_ratio_dose_mg = ci_ratio_dose_mg
  )
}

## plot ----

#' Building the Plot for nextBest-NextBestTDsamples Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for [`nextBest-NextBestTDsamples()`]
#' method.
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
#' Helper function which creates the plot for [`nextBest-NextBestTD()`] method.
#'
#' @param prob_target_drt (`proportion`)\cr target DLT probability during the trial.
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param prob_target_eot (`proportion`)\cr target DLT probability at the end of the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param data (`Data`)\cr the data object from which the dose grid will be fetched.
#' @param prob_dlt (`numeric`)\cr DLT probabilities for doses in grid.
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

  dose_grid_range <- h_dose_grid_range(data)

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

#' Building the Plot for nextBest-NextBestMaxGain Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for [`nextBest-NextBestMaxGain()`] method.
#'
#' @param prob_target_drt (`proportion`)\cr target DLT probability during the trial.
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param prob_target_eot (`proportion`)\cr target DLT probability at the end of the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param dose_mg (`number`)\cr the dose corresponding to the maximum gain.
#' @param max_gain (`number`)\cr the dose corresponding to the maximum gain.
#' @param next_dose (`number`)\cr next best dose.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param data (`DataDual`)\cr the data object from which the dose grid will be fetched.
#' @param model (`ModelTox`)\cr the DLT model.
#' @param model_eff (`Effloglog`)\cr the efficacy model.
#'
#' @export
#'
h_next_best_mg_plot <- function(prob_target_drt,
                                dose_target_drt,
                                prob_target_eot,
                                dose_target_eot,
                                dose_mg,
                                max_gain,
                                next_dose,
                                doselimit,
                                data,
                                model,
                                model_eff) {
  assert_probability(prob_target_drt)
  assert_number(dose_target_drt)
  assert_probability(prob_target_eot)
  assert_number(dose_target_eot)
  assert_number(dose_mg, na.ok = TRUE)
  assert_number(max_gain, na.ok = TRUE)
  assert_number(next_dose, na.ok = TRUE)
  assert_number(doselimit)
  assert_class(data, "Data")
  assert_class(model, "ModelTox")
  assert_class(model_eff, "Effloglog")

  dose_grid_range <- h_dose_grid_range(data)

  data_plot <- data.frame(
    dose = rep(data@doseGrid, 3),
    y = c(
      prob(dose = data@doseGrid, model = model),
      efficacy(dose = data@doseGrid, model = model_eff),
      gain(dose = data@doseGrid, model_dle = model, model_eff = model_eff)
    ),
    group = c(
      rep("p(DLE)", data@nGrid),
      rep("Expected Efficacy", data@nGrid),
      rep("Gain", data@nGrid)
    )
  )

  p <- ggplot(data = data_plot, aes(x = .data$dose, y = .data$y)) +
    geom_line(aes(group = group, color = group), size = 1.5) +
    ggplot2::scale_colour_manual(name = "curves", values = c("blue", "green3", "red")) +
    coord_cartesian(xlim = c(0, dose_grid_range[2])) +
    ylim(range(data_plot$y)) +
    xlab("Dose Level") +
    ylab("Values")

  if (h_in_range(dose_target_eot, range = dose_grid_range, bounds_closed = FALSE)) {
    lab <- paste("TD", prob_target_eot * 100, "Estimate")
    p <- p +
      geom_point(
        data = data.frame(x = dose_target_eot, y = prob_target_eot),
        aes(x = .data$x, y = .data$y),
        colour = "violet",
        shape = 16,
        size = 8
      ) +
      annotate(
        geom = "text", label = lab, x = dose_target_eot - 1, y = 0.2, size = 5, colour = "violet"
      )
  }

  if (h_in_range(dose_mg, range = dose_grid_range, bounds_closed = FALSE)) {
    p <- p +
      geom_point(
        data = data.frame(x = dose_mg, y = max_gain), aes(x = .data$x, y = .data$y), colour = "green3", shape = 17, size = 8
      ) +
      annotate(
        "text",
        label = "Max Gain Estimate", x = dose_mg, y = max_gain - 0.1, size = 5, colour = "green3"
      )
  }

  if (h_in_range(dose_target_drt, range = dose_grid_range, bounds_closed = FALSE)) {
    lab <- paste("TD", prob_target_drt * 100, "Estimate")
    p <- p +
      geom_point(
        data = data.frame(x = dose_target_drt, y = prob_target_drt),
        aes(x = .data$x, y = .data$y),
        colour = "orange",
        shape = 15,
        size = 8
      ) +
      annotate(
        geom = "text", label = lab, x = dose_target_drt + 25, y = prob_target_drt + 0.01, size = 5, colour = "orange"
      )
  }

  maxdoselimit <- min(doselimit, dose_grid_range[2])

  p +
    geom_vline(xintercept = maxdoselimit, colour = "brown", lwd = 1.1) +
    annotate(
      geom = "text", label = "Max", x = maxdoselimit - 2, y = max(data_plot$y), size = 5, angle = 90, vjust = -0.5, hjust = 0.5, colour = "brown"
    ) +
    geom_vline(xintercept = next_dose, colour = "purple", lwd = 1.1) +
    annotate(
      geom = "text", label = "Next", x = next_dose + 1, y = max(data_plot$y) - 0.05, size = 5, angle = 90, vjust = 1.5, hjust = 0.5, color = "purple"
    )
}
