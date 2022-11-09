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

#' Credibility Intervals for Max Gain and Target Doses at `nextBest-NextBestMaxGain` Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function for [`nextBest-NextBestMaxGain()`] method. It computes a
#' 95% credibility intervals for given target dose and max gain dose.
#' It also returns a ratio of upper and lower bounds of the interval.
#'
#' @param dose_target (`number`)\cr target dose estimate.
#' @param dose_mg (`number`)\cr the dose corresponding to the maximum gain.
#' @param prob_target (`proportion`)\cr target DLT probability.
#' @param placebo (`flag`)\cr if `TRUE` the first dose level in the dose grid used
#'   is considered as placebo. This is needed to adjust the max gain dose using
#'   efficacy constant value. If the `placebo` was used, then the `model_eff@const`
#'   is added to `dose_mg`.
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
h_next_best_mg_ci <- function(dose_target,
                              dose_mg,
                              prob_target,
                              placebo,
                              model,
                              model_eff) {
  assert_number(dose_target, na.ok = TRUE)
  assert_number(dose_mg, na.ok = TRUE)
  assert_probability(prob_target)
  assert_flag(placebo)
  assert_class(model, "ModelTox")
  assert_class(model_eff, "Effloglog")

  # Find the variance of the log of target dose.
  mat <- matrix(
    c(
      -1 / (model@phi2),
      -(log(prob_target / (1 - prob_target)) - model@phi1) / (model@phi2)^2
    ),
    nrow = 1
  )
  var_dose_target <- as.vector(mat %*% model@Pcov %*% t(mat))

  # 95% credibility interval for target dose.
  ci_dose_target <- exp(log(dose_target) + c(-1, 1) * 1.96 * sqrt(var_dose_target))
  cir_dose_target <- ci_dose_target[2] / ci_dose_target[1]

  # Find the variance of the log of dose_mg.
  # First, find the covariance matrix of all the parameters, phi1, phi2, theta1 and theta2
  # given that phi1 and phi2 are independent of theta1 and theta2.
  log_dose_mg <- log(dose_mg + ifelse(placebo, model_eff@const, 0))

  # Find a delta_g matrix for a variance according to Yeung et. al (2015).
  mean_eff_mg <- model_eff@theta1 + model_eff@theta2 * log(log_dose_mg)
  denom <- model@phi2 * mean_eff_mg * (1 + model@phi2 * log_dose_mg)
  dgphi1 <- -(mean_eff_mg * log_dose_mg * model@phi2 - model_eff@theta2) / denom
  dgphi2 <- -(log_dose_mg * (mean_eff_mg * (1 + log_dose_mg * model@phi2) - model_eff@theta2)) / denom
  dgtheta1 <- -(log_dose_mg * model@phi2) / denom
  dgtheta2_num <- -(exp(model@phi1 + model@phi2 * log_dose_mg) * (model@phi2 * log_dose_mg * log(log_dose_mg) - 1) - 1)
  dgtheta2 <- dgtheta2_num / denom
  delta_g <- matrix(c(dgphi1, dgphi2, dgtheta1, dgtheta2), 4, 1)

  zero_matrix <- matrix(0, 2, 2)
  cov_beta <- cbind(rbind(model@Pcov, zero_matrix), rbind(zero_matrix, model_eff@Pcov))
  var_log_dose_mg <- as.vector(t(delta_g) %*% cov_beta %*% delta_g)

  # 95% credibility interval for max gain dose.
  ci_mg <- exp(log_dose_mg + c(-1, 1) * 1.96 * sqrt(var_log_dose_mg))
  ci_ratio_mg <- ci_mg[2] / ci_mg[1]

  list(
    ci_dose_target = ci_dose_target,
    ci_ratio_dose_target = cir_dose_target,
    ci_dose_mg = ci_mg,
    ci_ratio_dose_mg = ci_ratio_mg
  )
}

## next best at grid ----

#' Get Closest Grid Doses for a Given Target Doses for `nextBest-NextBestMaxGain` Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function that for a given target doses finds the dose in grid that is
#' closest and below the target. There are four different targets in the context
#' of [`nextBest-NextBestMaxGain()`] method: \eqn{min(`dose_mg`, `dose_target_drt`)},
#' `dose_mg`, `dose_target_drt` or `dose_target_eot`.
#'
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param dose_mg (`number`)\cr the dose corresponding to the maximum gain.
#' @param dose_grid (`numeric`)\cr all possible doses.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param placebo (`flag`)\cr if `TRUE` the first dose level in the `dose_grid`
#'   is considered as placebo.
#'
#' @export
#'
h_next_best_mg_doses_at_grid <- function(dose_target_drt,
                                         dose_target_eot,
                                         dose_mg,
                                         dose_grid,
                                         doselimit,
                                         placebo) {
  assert_number(dose_target_drt, na.ok = TRUE)
  assert_number(dose_target_eot, na.ok = TRUE)
  assert_number(dose_mg, na.ok = TRUE)

  doses_eligible <- h_next_best_eligible_doses(dose_grid, doselimit, placebo)

  # h_find_interval assumes that elements in doses_eligible are strictly increasing.
  next_dose_lev <- h_find_interval(min(dose_mg, dose_target_drt), doses_eligible)
  next_dose <- doses_eligible[next_dose_lev]

  next_dose_mg_lev <- h_find_interval(dose_mg, doses_eligible)
  next_dose_mg <- doses_eligible[next_dose_mg_lev]

  next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
  next_dose_drt <- doses_eligible[next_dose_lev_drt]

  next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
  next_dose_eot <- doses_eligible[next_dose_lev_eot]

  next_dose_list <- list(
    next_dose = next_dose,
    next_dose_drt = next_dose_drt,
    next_dose_eot = next_dose_eot,
    next_dose_mg = next_dose_mg
  )
}

## eligible doses ----

#' Get Eligible Doses from the Dose Grid.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function that gets the eligible doses from the dose grid.
#' The eligible doses are the doses which do not exceed a given
#' `doselimit`. For placebo design, if safety allows (i.e. if there is at least
#' one non-placebo dose which does not exceed the dose limit), the placebo dose
#' it then excluded from the eligible doses.
#'
#' @param dose_grid (`numeric`)\cr all possible doses.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param placebo (`flag`)\cr if `TRUE` the first dose level in the `dose_grid`
#'   is considered as placebo.
#' @param levels (`flag`)\cr if `TRUE` the levels of eligible doses are returned,
#'   otherwise, the doses (default).
#'
#' @return A numeric vector with eligible doses or eligible dose levels if `levels`
#'   flag is `TRUE`.
#'
#' @export
#' @examples
#' dose_grid <- c(0.001, seq(25, 200, 25))
#' h_next_best_eligible_doses(dose_grid, 79, TRUE)
#' h_next_best_eligible_doses(dose_grid, 24, TRUE)
h_next_best_eligible_doses <- function(dose_grid,
                                       doselimit,
                                       placebo,
                                       levels = FALSE) {
  assert_numeric(dose_grid, finite = TRUE, any.missing = FALSE, min.len = 1L, sorted = TRUE)
  assert_number(doselimit)
  assert_flag(placebo)
  assert_flag(levels)

  is_dose_eligible <- dose_grid <= doselimit
  if (placebo && sum(is_dose_eligible) > 1L) {
    is_dose_eligible[1] <- FALSE
  }

  if (levels) {
    is_dose_eligible
  } else {
    dose_grid[is_dose_eligible]
  }
}

## plot ----

#' Building the Plot for `nextBest-NextBestNCRMLoss` Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for [`nextBest-NextBestNCRMLoss()`]
#' method.
#'
#' @param prob_mat (`numeric`)\cr matrix with probabilities of a grid doses
#'   to be in a given interval. If `is_unacceptable_specified` is `TRUE`, there
#'   must be 4 intervals (columns) in `prob_mat`: `underdosing`, `target`,
#'   `excessive`, `unacceptable`. Otherwise, there must be 3 intervals (columns):
#'   `underdosing`, `target`, `overdose`. Number of rows must be equal to number
#'   of doses in a grid.
#' @param posterior_loss (`numeric`)\cr posterior losses.
#' @param max_overdose_prob (`number`)\cr maximum overdose posterior
#'   probability that is allowed.
#' @param dose_grid (`numeric`)\cr dose grid.
#' @param max_eligible_dose_level (`number`)\cr maximum eligible dose level in
#'   the `dose_grid`.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param next_dose (`number`)\cr next best dose.
#' @param is_unacceptable_specified (`flag`)\cr is unacceptable interval specified?
#'
#' @export
h_next_best_ncrm_loss_plot <- function(prob_mat,
                                       posterior_loss,
                                       max_overdose_prob,
                                       dose_grid,
                                       max_eligible_dose_level,
                                       doselimit,
                                       next_dose,
                                       is_unacceptable_specified) {
  assert_numeric(dose_grid, finite = TRUE, any.missing = FALSE, sorted = TRUE)
  n_grid <- length(dose_grid)
  assert_flag(is_unacceptable_specified)
  assert_probabilities(prob_mat)
  assert_matrix(prob_mat, min.cols = 3, max.cols = 4, nrows = n_grid, col.names = "named")
  if (!is_unacceptable_specified) {
    assert_names(colnames(prob_mat), permutation.of = c("underdosing", "target", "overdose"))
  } else {
    assert_names(colnames(prob_mat), permutation.of = c("underdosing", "target", "excessive", "unacceptable"))
  }
  assert_numeric(posterior_loss, finite = TRUE, any.missing = FALSE, len = n_grid)
  assert_probability(max_overdose_prob)
  assert_number(max_eligible_dose_level, lower = 0, upper = n_grid)
  assert_number(doselimit)
  assert_number(next_dose, na.ok = TRUE)

  # Build plots, first for the target probability.
  p1 <- ggplot() +
    geom_bar(
      data = data.frame(Dose = dose_grid, y = prob_mat[, "target"] * 100),
      aes(x = .data$Dose, y = .data$y),
      stat = "identity",
      position = "identity",
      width = min(diff(dose_grid)) / 2,
      colour = "darkgreen",
      fill = "darkgreen"
    ) +
    ylim(c(0, 100)) +
    ylab(paste("Target probability [%]"))

  if (is.finite(doselimit)) {
    p1 <- p1 + geom_vline(xintercept = doselimit, lwd = 1.1, lty = 2, colour = "black")
  }

  if (max_eligible_dose_level > 0) {
    p1 <- p1 +
      geom_vline(xintercept = dose_grid[max_eligible_dose_level], lwd = 1.1, lty = 2, colour = "red")
  }

  p_loss <- ggplot() +
    # For the loss function.
    geom_bar(
      data = data.frame(Dose = dose_grid, y = posterior_loss),
      aes(x = .data$Dose, y = .data$y),
      stat = "identity",
      position = "identity",
      width = min(diff(dose_grid)) / 2,
      colour = "darkgreen",
      fill = "darkgreen"
    ) +
    geom_point(
      aes(x = next_dose, y = max(posterior_loss) + 0.2),
      size = 3,
      pch = 25,
      col = "red",
      bg = "red"
    ) +
    ylab(paste("Loss function"))

  if (!is_unacceptable_specified) {
    # Second, for the overdosing probability.
    p2 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = dose_grid, y = prob_mat[, "overdose"] * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(dose_grid)) / 2,
        colour = "red",
        fill = "red"
      ) +
      geom_hline(
        yintercept = max_overdose_prob * 100, lwd = 1.1, lty = 2,
        colour = "black"
      ) +
      ylim(c(0, 100)) +
      ylab("Overdose probability [%]")

    # Combine it all together.
    plots_single <- list(plot1 = p1, plot2 = p2, plot_loss = p_loss)
    plot_joint <- gridExtra::arrangeGrob(p1, p2, p_loss, nrow = 3)
  } else {
    # Plot in case of 4 toxicity intervals. Second, for the overdosing probability.
    p2 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = dose_grid, y = prob_mat[, "excessive"] * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(dose_grid)) / 2,
        colour = "red",
        fill = "red"
      ) +
      ylim(c(0, 100)) +
      ylab("Excessive probability [%]")

    p3 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = dose_grid, y = prob_mat[, "unacceptable"] * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(dose_grid)) / 2,
        colour = "red",
        fill = "red"
      ) +
      ylim(c(0, 100)) +
      ylab("Unacceptable probability [%]")

    # Combine it all together.
    plots_single <- list(plot1 = p1, plot2 = p2, plot3 = p3, plot_loss = p_loss)
    plot_joint <- gridExtra::arrangeGrob(p1, p2, p3, p_loss, nrow = 4)
  }

  list(plots_single = plots_single, plot_joint = plot_joint)
}

#' Building the Plot for `nextBest-NextBestTDsamples` Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for [`nextBest-NextBestTDsamples()`]
#' method.
#'
#' @param dose_target_drt_samples (`numeric`)\cr vector of in-trial samples.
#' @param dose_target_eot_samples (`numeric`)\cr vector of end-of-trial samples.
#' @param dose_target_drt (`number`)\cr target in-trial estimate.
#' @param dose_target_eot (`number`)\cr target end-of-trial estimate.
#' @param dose_grid_range (`numeric`)\cr range of dose grid.
#' @param nextBest (`NextBestTDsamples`)\cr the rule for the next best dose.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param next_dose (`number`)\cr next best dose.
#'
#' @export
#'
h_next_best_tdsamples_plot <- function(dose_target_drt_samples,
                                       dose_target_eot_samples,
                                       dose_target_drt,
                                       dose_target_eot,
                                       dose_grid_range,
                                       nextBest,
                                       doselimit,
                                       next_dose) {
  assert_numeric(dose_target_drt_samples, any.missing = FALSE)
  assert_numeric(dose_target_eot_samples, any.missing = FALSE)
  assert_number(dose_target_drt)
  assert_number(dose_target_eot)
  assert_numeric(dose_grid_range, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE)
  assert_class(nextBest, "NextBestTDsamples")
  assert_number(doselimit)
  assert_number(next_dose, na.ok = TRUE)

  p <- ggplot(
    data = rbind(
      data.frame(period = "during", TD = dose_target_drt_samples),
      data.frame(period = "end", TD = dose_target_eot_samples)
    ),
    aes(x = .data$TD, colour = .data$period),
  ) +
    geom_density(fill = "grey50") +
    coord_cartesian(xlim = dose_grid_range) +
    scale_color_manual(values = c(during = "grey50", end = "violet")) +
    theme(legend.position = "none") +
    ylab("Posterior density") +
    geom_vline(xintercept = dose_target_drt, colour = "orange", lwd = 1.1) +
    annotate(
      geom = "text",
      label = paste("TD", nextBest@prob_target_drt * 100, "Estimate"),
      x = dose_target_drt,
      y = 0,
      hjust = -0.1,
      vjust = -20,
      size = 5,
      colour = "orange"
    ) +
    geom_vline(xintercept = dose_target_eot, colour = "violet", lwd = 1.1) +
    annotate(
      geom = "text",
      label = paste("TD", nextBest@prob_target_eot * 100, "Estimate"),
      x = dose_target_eot,
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
    geom_vline(xintercept = next_dose, colour = "blue", lwd = 1.1) +
    geom_text(
      data = data.frame(x = next_dose),
      aes(x, 0, label = "Next", hjust = 0.1, vjust = -30),
      angle = 90,
      vjust = -0.5,
      hjust = 0.5,
      colour = "blue"
    )
}

#' Building the Plot for `nextBest-NextBestTD` Method.
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

#' Building the Plot for `nextBest-NextBestMaxGain` Method.
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
#' @param max_gain (`number`)\cr the maximum gain estimate.
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
        data = data.frame(x = dose_mg, y = max_gain),
        aes(x = .data$x, y = .data$y),
        colour = "green3",
        shape = 17,
        size = 8
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
      geom = "text",
      label = "Max",
      x = maxdoselimit - 2,
      y = max(data_plot$y),
      size = 5,
      angle = 90,
      vjust = -0.5,
      hjust = 0.5,
      colour = "brown"
    ) +
    geom_vline(xintercept = next_dose, colour = "purple", lwd = 1.1) +
    annotate(
      geom = "text",
      label = "Next",
      x = next_dose + 1,
      y = max(data_plot$y) - 0.05,
      size = 5,
      angle = 90,
      vjust = 1.5,
      hjust = 0.5,
      color = "purple"
    )
}

#' Building the Plot for `nextBest-NextBestMaxGainSamples` Method.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function which creates the plot for [`nextBest-NextBestMaxGainSamples()`] method.
#'
#' @param prob_target_drt (`proportion`)\cr target DLT probability during the trial.
#' @param dose_target_drt (`number`)\cr target dose estimate during the trial.
#' @param prob_target_eot (`proportion`)\cr target DLT probability at the end of the trial.
#' @param dose_target_eot (`number`)\cr target dose estimate at the end of the trial.
#' @param dose_mg (`number`)\cr the dose corresponding to the maximum gain.
#' @param dose_mg_samples (`numeric`)\cr for every sample, the dose (from the dose grid)
#'   that gives the maximum gain value.
#' @param next_dose (`number`)\cr next best dose.
#' @param doselimit (`number`)\cr the maximum allowed next dose.
#' @param dose_grid_range (`numeric`)\cr dose grid range.
#'
#' @export
#'
h_next_best_mgsamples_plot <- function(prob_target_drt,
                                       dose_target_drt,
                                       prob_target_eot,
                                       dose_target_eot,
                                       dose_mg,
                                       dose_mg_samples,
                                       next_dose,
                                       doselimit,
                                       dose_grid_range) {
  assert_numeric(dose_grid_range, len = 2, sorted = TRUE)
  assert_probability(prob_target_drt)
  assert_number(dose_target_drt)
  assert_probability(prob_target_eot)
  assert_number(dose_target_eot)
  assert_number(dose_mg, na.ok = TRUE)
  assert_numeric(
    dose_mg_samples, lower = dose_grid_range[1], upper = dose_grid_range[2], finite = TRUE, any.missing = FALSE
  )
  assert_number(next_dose, na.ok = TRUE)
  assert_number(doselimit)

  p <- ggplot() +
    geom_histogram(
      data = data.frame(Gstar = dose_mg_samples),
      aes(x = .data$Gstar),
      fill = "darkgreen",
      colour = "green3",
      binwidth = 25
    ) +
    coord_cartesian(xlim = c(0, dose_grid_range[2])) +
    ylab("Posterior density")

  if (h_in_range(dose_target_drt, range = dose_grid_range, bounds_closed = FALSE)) {
    lab <- paste("TD", prob_target_drt * 100, "Estimate")
    p <- p +
      geom_vline(xintercept = dose_target_drt, colour = "orange", lwd = 1.1) +
      annotate(
        geom = "text", label = lab, x = dose_target_drt, y = 0, hjust = -0.1, vjust = -20, size = 5, colour = "orange"
      )
  }

  if (h_in_range(dose_target_eot, range = dose_grid_range, bounds_closed = FALSE)) {
    lab <- paste("TD", prob_target_eot * 100, "Estimate")
    p <- p +
      geom_vline(xintercept = dose_target_eot, colour = "violet", lwd = 1.1) +
      annotate(
        geom = "text", label = lab, x = dose_target_eot, y = 0, hjust = -0.1, vjust = -25, size = 5, colour = "violet"
      )
  }

  if (h_in_range(dose_mg, range = dose_grid_range, bounds_closed = FALSE)) {
    lab <- "Gstar Estimate"
    p <- p +
      geom_vline(xintercept = dose_mg, colour = "green", lwd = 1.1) +
      annotate(
        geom = "text", label = lab, x = dose_mg, y = 0, hjust = -0.1, vjust = -25, size = 5, colour = "green"
      )
  }

  maxdoselimit <- min(doselimit, dose_grid_range[2])

  p +
    geom_vline(xintercept = maxdoselimit, colour = "red", lwd = 1.1) +
    annotate(
      geom = "text", label = "Max", x = maxdoselimit, y = 0, hjust = +1, vjust = -35, colour = "red"
    ) +
    geom_vline(xintercept = next_dose, colour = "blue", lwd = 1.1) +
    annotate(
      geom = "text", label = "Next", x = next_dose, y = 0, hjust = 0.1, vjust = -30, colour = "blue"
    )
}
