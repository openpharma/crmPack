#' @include Model-methods.R
#' @include Samples-class.R
#' @include Rules-class.R
#' @include helpers_rules.R
NULL

# nextBest ----

## generic ----

#' Finding the Next Best Dose
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A function that computes the recommended next best dose based on the
#' corresponding rule `nextBest`, the posterior `samples` from the `model` and
#' the underlying `data`.
#'
#' @param nextBest (`NextBest`)\cr the rule for the next best dose.
#' @param doselimit (`number`)\cr the maximum allowed next dose. If it is an
#'   infinity (default), then essentially no dose limit will be applied in the
#'   course of dose recommendation calculation.
#' @param samples (`Samples`)\cr posterior samples from `model` parameters given
#'   `data`.
#' @param model (`GeneralModel`)\cr model that was used to generate the samples.
#' @param data (`Data`)\cr data that was used to generate the samples.
#' @param ... additional arguments without method dispatch.
#'
#' @return A list with the next best dose recommendation  (element named `value`)
#'   from the grid defined in `data`, and a plot depicting this recommendation
#'   (element named `plot`). In case of multiple plots also an element
#'   named `singlePlots` is included. The `singlePlots` is itself a list with
#'   single plots. An additional list with elements describing the outcome
#'   of the rule can be contained too.
#'
#' @export
#'
setGeneric(
  name = "nextBest",
  def = function(nextBest, doselimit, samples, model, data, ...) {
    if (!missing(doselimit)) {
      assert_number(doselimit, lower = 0, finite = FALSE)
    }
    standardGeneric("nextBest")
  },
  valueClass = "list"
)

## NextBestMTD ----

#' @describeIn nextBest find the next best dose based on the MTD rule.
#'
#' @aliases nextBest-NextBestMTD
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestMTD.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestMTD",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Generate the MTD samples and derive the next best dose.
    dose_target_samples <- dose(x = nextBest@target, model, samples)
    dose_target <- nextBest@derive(dose_target_samples)

    # Round to the next possible grid point.
    doses_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo)
    next_dose_level <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level]

    # Create a plot.
    p <- ggplot(
      data = data.frame(x = dose_target_samples),
      aes(.data$x),
      fill = "grey50",
      colour = "grey50"
    ) +
      geom_density() +
      coord_cartesian(xlim = range(data@doseGrid)) +
      geom_vline(xintercept = dose_target, colour = "black", lwd = 1.1) +
      geom_text(
        data = data.frame(x = dose_target),
        aes(.data$x, 0),
        label = "Est",
        vjust = -0.5,
        hjust = 0.5,
        colour = "black",
        angle = 90
      ) +
      xlab("MTD") +
      ylab("Posterior density")

    if (is.finite(doselimit)) {
      p <- p +
        geom_vline(xintercept = doselimit, colour = "red", lwd = 1.1) +
        geom_text(
          data = data.frame(x = doselimit),
          aes(.data$x, 0),
          label = "Max",
          vjust = -0.5,
          hjust = -0.5,
          colour = "red",
          angle = 90
        )
    }

    p <- p +
      geom_vline(xintercept = next_dose, colour = "blue", lwd = 1.1) +
      geom_text(
        data = data.frame(x = next_dose),
        aes(.data$x, 0),
        label = "Next",
        vjust = -0.5,
        hjust = -1.5,
        colour = "blue",
        angle = 90
      )

    list(value = next_dose, plot = p)
  }
)

## NextBestNCRM ----

#' @describeIn nextBest find the next best dose based on the NCRM method. The
#'   additional element `probs` in the output's list contains the target and
#'   overdosing probabilities (across all doses in the dose grid) used in the
#'   derivation of the next best dose.
#'
#' @aliases nextBest-NextBestNCRM
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestNCRM.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestNCRM",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)

    # Estimates of posterior probabilities that are based on the prob. samples
    # which are within overdose/target interval.
    prob_overdose <- colMeans(h_in_range(prob_samples, nextBest@overdose, bounds_closed = c(FALSE, TRUE)))
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo, levels = TRUE) &
      (prob_overdose <= nextBest@max_overdose_prob)

    next_dose <- if (any(is_dose_eligible)) {
      # If maximum target probability is higher than some numerical threshold,
      # then take that level, otherwise stick to the maximum level that is OK.
      # next_best_level is relative to eligible doses.
      next_best_level <- ifelse(
        test = any(prob_target[is_dose_eligible] > 0.05),
        yes = which.max(prob_target[is_dose_eligible]),
        no = sum(is_dose_eligible)
      )
      data@doseGrid[is_dose_eligible][next_best_level]
    } else {
      NA
    }

    # Build plots, first for the target probability.
    p1 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = data@doseGrid, y = prob_target * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(data@doseGrid)) / 2,
        colour = "darkgreen",
        fill = "darkgreen"
      ) +
      ylim(c(0, 100)) +
      ylab(paste("Target probability [%]"))

    if (is.finite(doselimit)) {
      p1 <- p1 + geom_vline(xintercept = doselimit, lwd = 1.1, lty = 2, colour = "black")
    }

    if (any(is_dose_eligible)) {
      p1 <- p1 +
        geom_vline(xintercept = data@doseGrid[sum(is_dose_eligible)], lwd = 1.1, lty = 2, colour = "red") +
        geom_point(
          aes(x = next_dose, y = prob_target[is_dose_eligible][next_best_level] * 100 + 0.03),
          size = 3,
          pch = 25,
          col = "red",
          bg = "red"
        )
    }

    # Second, for the overdosing probability.
    p2 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = data@doseGrid, y = prob_overdose * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(data@doseGrid)) / 2,
        colour = "red",
        fill = "red"
      ) +
      geom_hline(yintercept = nextBest@max_overdose_prob * 100, lwd = 1.1, lty = 2, colour = "black") +
      ylim(c(0, 100)) +
      ylab("Overdose probability [%]")

    # Place them below each other.
    plot_joint <- gridExtra::arrangeGrob(p1, p2, nrow = 2)

    list(
      value = next_dose,
      plot = plot_joint,
      singlePlots = list(plot1 = p1, plot2 = p2),
      probs = cbind(
        dose = data@doseGrid,
        target = prob_target,
        overdose = prob_overdose
      )
    )
  }
)

## NextBestNCRM-DataParts ----

#' @describeIn nextBest find the next best dose based on the NCRM method when
#'   two parts trial is used.
#'
#' @aliases nextBest-NextBestNCRM-DataParts
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestNCRM-DataParts.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestNCRM",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "DataParts"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
    # Exception when we are in part I or about to start part II!
    if (all(data@part == 1L)) {
      # Propose the highest possible dose (assuming that the dose limit came
      # from reasonable increments rule, i.e. incrementsRelativeParts).
      if (is.infinite(doselimit)) {
        stop("A finite doselimit needs to be specified for Part I.")
      }
      list(value = doselimit, plot = NULL)
    } else {
      # Otherwise we will just do the standard thing.
      callNextMethod(nextBest, doselimit, samples, model, data, ...)
    }
  }
)

## NextBestNCRMLoss ----

#' @describeIn nextBest find the next best dose based on the NCRM method and
#'   loss function.
#'
#' @aliases nextBest-NextBestNCRMLoss
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestNCRMLoss.R
#'
setMethod("nextBest",
  signature = signature(
    nextBest = "NextBestNCRMLoss",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)
    # Compute probabilities to be in target and overdose tox interval.
    prob_underdosing <- colMeans(prob_samples < nextBest@target[1])
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))
    prob_overdose <- colMeans(h_in_range(prob_samples, nextBest@overdose, bounds_closed = c(FALSE, TRUE)))
    prob_mean <- colMeans(prob_samples)
    prob_sd <- apply(prob_samples, 2, stats::sd)

    is_unacceptable_specified <- any(nextBest@unacceptable != c(1, 1))

    prob_mat <- if (!is_unacceptable_specified) {
      cbind(underdosing = prob_underdosing, target = prob_target, overdose = prob_overdose)
    } else {
      prob_unacceptable <- colMeans(
        h_in_range(prob_samples, nextBest@unacceptable, bounds_closed = c(FALSE, TRUE))
      )
      prob_excessive <- prob_overdose
      prob_overdose <- prob_excessive + prob_unacceptable
      cbind(
        underdosing = prob_underdosing,
        target = prob_target,
        excessive = prob_excessive,
        unacceptable = prob_unacceptable
      )
    }

    posterior_loss <- as.vector(nextBest@losses %*% t(prob_mat))
    names(posterior_loss) <- data@doseGrid

    probs <- cbind(
      dose = data@doseGrid,
      prob_mat = prob_mat,
      mean = prob_mean,
      std_dev = prob_sd,
      posterior_loss = posterior_loss
    )

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo, levels = TRUE) &
      (prob_overdose < nextBest@max_overdose_prob)

    # Next best dose is the dose with the minimum loss function.
    next_dose <- if (any(is_dose_eligible)) {
      next_best_level <- which.min(posterior_loss[is_dose_eligible])
      data@doseGrid[is_dose_eligible][next_best_level]
    } else {
      NA
    }

    # Build plot.
    p <- h_next_best_ncrm_loss_plot(
      prob_mat = prob_mat,
      posterior_loss = posterior_loss,
      max_overdose_prob = nextBest@max_overdose_prob,
      dose_grid = data@doseGrid,
      max_eligible_dose_level = sum(is_dose_eligible),
      doselimit = doselimit,
      next_dose = next_dose,
      is_unacceptable_specified = is_unacceptable_specified
    )

    c(list(value = next_dose, probs = probs), p)
  }
)

## NextBestThreePlusThree ----

#' @describeIn nextBest find the next best dose based on the 3+3 method.
#'
#' @aliases nextBest-NextBestThreePlusThree
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestThreePlusThree.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestThreePlusThree",
    doselimit = "missing",
    samples = "missing",
    model = "missing",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {

    # The last dose level tested (not necessarily the maximum one).
    last_level <- tail(data@xLevel, 1L)

    # Get number of patients per grid's dose and DLT rate at the last level.
    nPatients <- table(factor(data@x, levels = data@doseGrid))
    n_dlts_last_level <- sum(data@y[data@xLevel == last_level])
    dlt_rate_last_level <- n_dlts_last_level / nPatients[last_level]

    level_change <- if (dlt_rate_last_level < 1 / 3) {
      # Escalate it, unless this is the highest level or the higher dose was already tried.
      ifelse((last_level == data@nGrid) || (nPatients[last_level + 1L] > 0), 0L, 1L)
    } else {
      # Rate is too high, deescalate it, unless an edge case of 1/3, where the decision
      # depends on the num. of patients: if >3, then deescalate it, otherwise stay.
      ifelse((dlt_rate_last_level == 1 / 3) && (nPatients[last_level] <= 3L), 0L, -1L)
    }
    next_dose_level <- last_level + level_change

    # Do we stop here? Only if we have no MTD, or the next level has been tried
    # enough (more than three patients already).
    if (next_dose_level == 0L) {
      next_dose <- NA
      stop_here <- TRUE
    } else {
      next_dose <- data@doseGrid[next_dose_level]
      stop_here <- nPatients[next_dose_level] > 3L
    }

    list(value = next_dose, stopHere = stop_here)
  }
)

## NextBestDualEndpoint ----

#' @describeIn nextBest find the next best dose based on the dual endpoint
#'   model. The additional list element `probs` contains the target and
#'   overdosing probabilities (across all doses in the dose grid) used in the
#'   derivation of the next best dose.
#'
#' @aliases nextBest-NextBestDualEndpoint
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestDualEndpoint.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestDualEndpoint",
    doselimit = "numeric",
    samples = "Samples",
    model = "DualEndpoint",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Biomarker samples at the dose grid points.
    biom_samples <- samples@data$betaW

    prob_target <- if (nextBest@target_relative) {
      # If 'Emax' parameter available, target biomarker level will be relative to 'Emax',
      # otherwise, it will be relative to the maximum biomarker level achieved
      # in dose range for a given sample.
      if ("Emax" %in% names(samples)) {
        lwr <- nextBest@target[1] * samples@data$Emax
        upr <- nextBest@target[2] * samples@data$Emax
        colMeans(apply(biom_samples, 2L, function(s) (s >= lwr) & (s <= upr)))
      } else {
        target_levels <- apply(biom_samples, 1L, function(x) {
          rng <- range(x)
          min(which(h_in_range(x, nextBest@target * diff(rng) + rng[1] + c(0, 1e-10), bounds_closed = c(FALSE, TRUE))))
        })
        prob_target <- as.vector(table(factor(target_levels, levels = 1:data@nGrid)))
        prob_target / nrow(biom_samples)
      }
    } else {
      colMeans(h_in_range(biom_samples, nextBest@target))
    }

    # Now, compute probabilities to be in overdose tox interval, then flag
    # eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)
    prob_overdose <- colMeans(h_in_range(prob_samples, nextBest@overdose, bounds_closed = c(FALSE, TRUE)))

    is_dose_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo, levels = TRUE) &
      (prob_overdose < nextBest@max_overdose_prob)

    next_dose <- if (any(is_dose_eligible)) {
      # If maximum target probability is higher the threshold, then take that
      # level, otherwise stick to the maximum level that is eligible.
      # next_dose_level is relative to eligible doses.
      next_dose_level <- ifelse(
        test = any(prob_target[is_dose_eligible] > nextBest@target_thresh),
        yes = which.max(prob_target[is_dose_eligible]),
        no = sum(is_dose_eligible)
      )
      data@doseGrid[is_dose_eligible][next_dose_level]
    } else {
      NA
    }

    # Build plots, first for the target probability.
    p1 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = data@doseGrid, y = prob_target * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(data@doseGrid)) / 2,
        colour = "darkgreen",
        fill = "darkgreen"
      ) +
      ylim(c(0, 100)) +
      ylab(paste("Target probability [%]"))

    if (is.finite(doselimit)) {
      p1 <- p1 + geom_vline(xintercept = doselimit, lwd = 1.1, lty = 2, colour = "black")
    }

    if (any(is_dose_eligible)) {
      p1 <- p1 +
        geom_vline(xintercept = data@doseGrid[sum(is_dose_eligible)], lwd = 1.1, lty = 2, colour = "red") +
        geom_point(
          data = data.frame(x = next_dose, y = prob_target[is_dose_eligible][next_dose_level] * 100 + 0.03),
          aes(x = x, y = y),
          size = 3,
          pch = 25,
          col = "red",
          bg = "red"
        )
    }

    # Second, for the overdosing probability.
    p2 <- ggplot() +
      geom_bar(
        data = data.frame(Dose = data@doseGrid, y = prob_overdose * 100),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(data@doseGrid)) / 2,
        colour = "red",
        fill = "red"
      ) +
      geom_hline(yintercept = nextBest@max_overdose_prob * 100, lwd = 1.1, lty = 2, colour = "black") +
      ylim(c(0, 100)) +
      ylab("Overdose probability [%]")

    # Place them below each other.
    plot_joint <- gridExtra::arrangeGrob(p1, p2, nrow = 2)

    list(
      value = next_dose,
      plot = plot_joint,
      singlePlots = list(plot1 = p1, plot2 = p2),
      probs = cbind(dose = data@doseGrid, target = prob_target, overdose = prob_overdose)
    )
  }
)

## NextBestMinDist ----

#' @describeIn nextBest gives the dose which is below the dose limit and has an
#'   estimated DLT probability which is closest to the target dose.
#'
#' @aliases nextBest-NextBestMinDist
#'
#' @export
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestMinDist",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
    modelfit <- fit(samples, model, data)
    doses <- modelfit$dose
    is_dose_eligible <- doses <= doselimit
    doses_eligible <- doses[is_dose_eligible]
    dlt_prob <- modelfit$middle[is_dose_eligible]
    next_dose_level <- which.min(abs(dlt_prob - nextBest@target))
    next_dose <- doses_eligible[next_dose_level]

    list(value = next_dose)
  }
)

## NextBestInfTheory ----

#' @describeIn nextBest gives the appropriate dose within an information
#'   theoretic framework.
#'
#' @aliases nextBest-NextBestInfTheory
#'
#' @export
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestInfTheory",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)

    criterion <- colMeans(h_info_theory_dist(prob_samples, nextBest@target, nextBest@asymmetry))

    is_dose_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo, levels = TRUE)
    doses_eligible <- data@doseGrid[is_dose_eligible]
    next_best_level <- which.min(criterion[is_dose_eligible])
    next_best <- doses_eligible[next_best_level]
    list(value = next_best)
  }
)

## NextBestTD ----

#' @describeIn nextBest find the next best dose based only on the DLT responses
#'   and for [`LogisticIndepBeta`] model class object without DLT samples.
#'
#' @param model (`ModelTox`)\cr the DLT model.
#' @param in_sim (`flag`)\cr is this method used in simulations? Default as `FALSE`.
#'   If this flag is `TRUE` and target dose estimates (during trial and end-of-trial)
#'   are outside of the dose grid range, the information message is printed by
#'   this method.
#'
#' @aliases nextBest-NextBestTD
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestTD.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestTD",
    doselimit = "numeric",
    samples = "missing",
    model = "LogisticIndepBeta",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, model, data, in_sim = FALSE, ...) {
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Target dose estimates, i.e. the dose with probability of the occurrence of
    # a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt <- dose(x = prob_target_drt, model)
    dose_target_eot <- dose(x = prob_target_eot, model)

    # Find the next best doses in the doseGrid. The next best dose is the dose
    # at level closest and below the target dose estimate.
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    doses_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo)

    next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
    next_dose_drt <- doses_eligible[next_dose_lev_drt]

    next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
    next_dose_eot <- doses_eligible[next_dose_lev_eot]

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
    ci_dose_target_eot <- exp(log(dose_target_eot) + c(-1, 1) * 1.96 * sqrt(var_dose_target_eot))
    cir_dose_target_eot <- ci_dose_target_eot[2] / ci_dose_target_eot[1]

    # Build plot.
    p <- h_next_best_td_plot(
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      data = data,
      prob_dlt = prob(dose = data@doseGrid, model = model),
      doselimit = doselimit,
      next_dose = next_dose_drt
    )

    if (!h_in_range(dose_target_drt, range = h_dose_grid_range(data), bounds_closed = TRUE) && !in_sim) {
      print(paste("TD", prob_target_drt * 100, "=", dose_target_drt, "not within dose grid"))
    }
    if (!h_in_range(dose_target_eot, range = h_dose_grid_range(data), bounds_closed = TRUE) && !in_sim) {
      print(paste("TD", prob_target_eot * 100, "=", dose_target_eot, "not within dose grid"))
    }

    list(
      next_dose_drt = next_dose_drt,
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      next_dose_eot = next_dose_eot,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      ci_dose_target_eot = ci_dose_target_eot,
      ci_ratio_dose_target_eot = cir_dose_target_eot,
      plot = p
    )
  }
)

## NextBestTDsamples ----

#' @describeIn nextBest find the next best dose based only on the DLT responses
#'   and for [`LogisticIndepBeta`] model class object involving DLT samples.
#'
#' @aliases nextBest-NextBestTDsamples
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestTDsamples.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestTDsamples",
    doselimit = "numeric",
    samples = "Samples",
    model = "LogisticIndepBeta",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {

    # Generate target dose samples, i.e. the doses with probability of the
    # occurrence of a DLT that equals to the nextBest@prob_target_drt
    # (or nextBest@prob_target_eot, respectively).
    dose_target_drt_samples <- dose(x = nextBest@prob_target_drt, model, samples)
    dose_target_eot_samples <- dose(x = nextBest@prob_target_eot, model, samples)

    # Derive the prior/posterior estimates based on two above samples.
    dose_target_drt <- nextBest@derive(dose_target_drt_samples)
    dose_target_eot <- nextBest@derive(dose_target_eot_samples)

    # Find the next doses in the doseGrid. The next dose is the dose at level
    # closest and below the dose_target_drt (or dose_target_eot, respectively).
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    doses_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo)

    next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
    next_dose_drt <- doses_eligible[next_dose_lev_drt]

    next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
    next_dose_eot <- doses_eligible[next_dose_lev_eot]

    # 95% credibility interval.
    ci_dose_target_eot <- as.numeric(quantile(dose_target_eot_samples, probs = c(0.025, 0.975)))
    cir_dose_target_eot <- ci_dose_target_eot[2] / ci_dose_target_eot[1]

    # Build plot.
    p <- h_next_best_tdsamples_plot(
      dose_target_drt_samples = dose_target_drt_samples,
      dose_target_eot_samples = dose_target_eot_samples,
      dose_target_drt = dose_target_drt,
      dose_target_eot = dose_target_eot,
      dose_grid_range = range(data@doseGrid),
      nextBest = nextBest,
      doselimit = doselimit,
      next_dose = next_dose_drt
    )

    list(
      next_dose_drt = next_dose_drt,
      prob_target_drt = nextBest@prob_target_drt,
      dose_target_drt = dose_target_drt,
      next_dose_eot = next_dose_eot,
      prob_target_eot = nextBest@prob_target_eot,
      dose_target_eot = dose_target_eot,
      ci_dose_target_eot = ci_dose_target_eot,
      ci_ratio_dose_target_eot = cir_dose_target_eot,
      plot = p
    )
  }
)

## NextBestMaxGain ----

#' @describeIn nextBest find the next best dose based only on pseudo DLT model
#'   [`ModelTox`] and [`Effloglog`] efficacy model without samples.
#'
#' @param model (`ModelTox`)\cr the DLT model.
#' @param model_eff (`Effloglog`)\cr the efficacy model.
#' @param in_sim (`flag`)\cr is this method used in simulations? Default as `FALSE`.
#'   If this flag is `TRUE` and target dose estimates (during trial and end-of-trial)
#'   are outside of the dose grid range, the information message is printed by
#'   this method.
#'
#' @aliases nextBest-NextBestMaxGain
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestMaxGain.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestMaxGain",
    doselimit = "numeric",
    samples = "missing",
    model = "ModelTox",
    data = "DataDual"
  ),
  definition = function(nextBest, doselimit = Inf, model, data, model_eff, in_sim = FALSE, ...) {
    assert_class(model_eff, "Effloglog")
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Target dose estimates, i.e. the dose with probability of the occurrence of
    # a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt <- dose(x = prob_target_drt, model)
    dose_target_eot <- dose(x = prob_target_eot, model)

    # Find the dose which gives the maximum gain.
    dose_grid_range <- h_dose_grid_range(data)
    opt <- optim(
      par = dose_grid_range[1],
      fn = function(DOSE) {
        -gain(DOSE, model_dle = model, model_eff = model_eff)
      },
      method = "L-BFGS-B",
      lower = dose_grid_range[1],
      upper = dose_grid_range[2]
    )
    dose_mg <- opt$par # this is G*. # no lintr
    max_gain <- -opt$value

    # Print info message if dose target is outside of the range.
    if (!h_in_range(dose_target_drt, range = h_dose_grid_range(data), bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated TD", prob_target_drt * 100, "=", dose_target_drt, "not within dose grid"))
    }
    if (!h_in_range(dose_target_eot, range = h_dose_grid_range(data), bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated TD", prob_target_eot * 100, "=", dose_target_eot, "not within dose grid"))
    }
    if (!h_in_range(dose_mg, range = h_dose_grid_range(data), bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated max gain dose =", dose_mg, "not within dose grid"))
    }

    # Get closest grid doses for a given target doses.
    nb_doses_at_grid <- h_next_best_mg_doses_at_grid(
      dose_target_drt = dose_target_drt,
      dose_target_eot = dose_target_eot,
      dose_mg = dose_mg,
      dose_grid = data@doseGrid,
      doselimit = doselimit,
      placebo = data@placebo
    )

    # 95% credibility intervals and corresponding ratios for maximum gain dose and target dose eot.
    ci <- h_next_best_mg_ci(
      dose_target = dose_target_eot,
      dose_mg = dose_mg,
      prob_target = prob_target_eot,
      placebo = data@placebo,
      model = model,
      model_eff = model_eff
    )

    # Build plot.
    p <- h_next_best_mg_plot(
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      dose_mg = dose_mg,
      max_gain = max_gain,
      next_dose = nb_doses_at_grid$next_dose,
      doselimit = doselimit,
      data = data,
      model = model,
      model_eff = model_eff
    )

    list(
      next_dose = nb_doses_at_grid$next_dose,
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      next_dose_drt = nb_doses_at_grid$next_dose_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      next_dose_eot = nb_doses_at_grid$next_dose_eot,
      dose_max_gain = dose_mg,
      next_dose_max_gain = nb_doses_at_grid$next_dose_mg,
      ci_dose_target_eot = ci$ci_dose_target,
      ci_ratio_dose_target_eot = ci$ci_ratio_dose_target,
      ci_dose_max_gain = ci$ci_dose_mg,
      ci_ratio_dose_max_gain = ci$ci_ratio_dose_mg,
      plot = p
    )
  }
)

## NextBestMaxGainSamples ----

#' @describeIn nextBest find the next best dose based on DLT and efficacy
#'   responses with DLT and efficacy samples.
#'
#' @param model (`ModelTox`)\cr the DLT model.
#' @param model_eff (`Effloglog` or `EffFlexi`)\cr the efficacy model.
#' @param samples_eff (`Samples`)\cr posterior samples from `model_eff` parameters
#'   given `data`.
#' @param in_sim (`flag`)\cr is this method used in simulations? Default as `FALSE`.
#'   If this flag is `TRUE` and target dose estimates (during trial and end-of-trial)
#'   are outside of the dose grid range, the information message is printed by
#'   this method.
#'
#' @aliases nextBest-NextBestMaxGainSamples
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestMaxGainSamples.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestMaxGainSamples",
    doselimit = "numeric",
    samples = "Samples",
    model = "ModelTox",
    data = "DataDual"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, model_eff, samples_eff, in_sim = FALSE, ...) {
    assert_true(test_class(model_eff, "Effloglog") || test_class(model_eff, "EffFlexi"))
    assert_class(samples_eff, "Samples")
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Generate target dose samples, i.e. the doses with probability of the
    # occurrence of a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt_samples <- dose(x = prob_target_drt, model, samples = samples)
    dose_target_eot_samples <- dose(x = prob_target_eot, model, samples = samples)

    # Derive the prior/posterior estimates based on two above samples.
    dose_target_drt <- nextBest@derive(dose_target_drt_samples)
    dose_target_eot <- nextBest@derive(dose_target_eot_samples)

    # Gain samples.
    gain_samples <- sapply(data@doseGrid, gain, model, samples, model_eff, samples_eff)
    # For every sample, get the dose (from the dose grid) that gives the maximum gain value.
    dose_lev_mg_samples <- apply(gain_samples, 1, which.max)
    dose_mg_samples <- data@doseGrid[dose_lev_mg_samples]
    # Maximum gain dose estimate is the nth percentile of the maximum gain dose samples.
    dose_mg <- nextBest@mg_derive(dose_mg_samples)
    gain_values <- apply(gain_samples, 2, FUN = nextBest@mg_derive)

    # Print info message if dose target is outside of the range.
    dose_grid_range <- h_dose_grid_range(data)
    if (!h_in_range(dose_target_drt, range = dose_grid_range, bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated TD", prob_target_drt * 100, "=", dose_target_drt, "not within dose grid"))
    }
    if (!h_in_range(dose_target_eot, range = dose_grid_range, bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated TD", prob_target_eot * 100, "=", dose_target_eot, "not within dose grid"))
    }
    if (!h_in_range(dose_mg, range = dose_grid_range, bounds_closed = FALSE) && !in_sim) {
      print(paste("Estimated max gain dose =", dose_mg, "not within dose grid"))
    }

    # Get closest grid doses for a given target doses.
    nb_doses_at_grid <- h_next_best_mg_doses_at_grid(
      dose_target_drt = dose_target_drt,
      dose_target_eot = dose_target_eot,
      dose_mg = dose_mg,
      dose_grid = data@doseGrid,
      doselimit = doselimit,
      placebo = data@placebo
    )

    # 95% credibility intervals and corresponding ratios for maximum gain dose and target dose eot.
    ci_dose_mg <- as.numeric(quantile(dose_mg_samples, probs = c(0.025, 0.975)))
    cir_dose_mg <- ci_dose_mg[2] / ci_dose_mg[1]

    ci_dose_target_eot <- as.numeric(quantile(dose_target_eot, probs = c(0.025, 0.975)))
    cir_dose_target_eot <- ci_dose_target_eot[2] / ci_dose_target_eot[1]

    # Build plot.
    p <- h_next_best_mgsamples_plot(
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      dose_mg = dose_mg,
      dose_mg_samples = dose_mg_samples,
      next_dose = nb_doses_at_grid$next_dose,
      doselimit = doselimit,
      dose_grid_range = dose_grid_range
    )

    list(
      next_dose = nb_doses_at_grid$next_dose,
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      next_dose_drt = nb_doses_at_grid$next_dose_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      next_dose_eot = nb_doses_at_grid$next_dose_eot,
      dose_max_gain = dose_mg,
      next_dose_max_gain = nb_doses_at_grid$next_dose_mg,
      ci_dose_target_eot = ci_dose_target_eot,
      ci_ratio_dose_target_eot = cir_dose_target_eot,
      ci_dose_max_gain = ci_dose_mg,
      ci_ratio_dose_max_gain = cir_dose_mg,
      plot = p
    )
  }
)

## NextBestProbMTD ----

#' @describeIn nextBest find the next best dose based on DLT and efficacy
#'   responses with DLT and efficacy samples.
#'
#' @aliases nextBest-NextBestProbMTD
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestProbMTD.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestProbMTD",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)

    # determine which dose level is just below the target
    # and calculate the rel frequency per dose level
    # first element of vector = no dose safe,
    # second element = 1st dose safe, etc.
    prob_mtd_dist <- table(factor(apply(prob_samples <= nextBest@target, 1, sum),
                                  levels = 0:data@nGrid)) / size(samples)

    # Calculate the relative frequency that a dose is safe applying the
    # specified method for handling of not allocated frequency
    # (handling of first element of the vector according to the selected method)
    # If method = none: use raw frequencies corresponding to planned doses
    #   to determine the next best dose, i.e. ignore the cases where no dose is
    #   safe.
    ######### which denominator is used? Number of iterations? Or number of itereations-number of cases no dose safe?
    ######### here the maximum of no dose safe and first dose is used. TBD.
    # If method = min: first dose = sum of no dose is safe (below target) and first
    #   dose is below target
    # If method = max: last dose = sum of no dose is safe (below target) and last
    #   dose is below target

    allocation_crit <- as.vector(prob_mtd_dist)
    names(allocation_crit) <- names(prob_mtd_dist)

    # in case that placebo is used, the placebo frequency is added
    # to no dose safe before further processing
    if (data@placebo) {
      allocation_crit[1] <- sum(allocation_crit[1:2])
      allocation_crit <- allocation_crit[-2]}

    if (nextBest@method == "min") {
      allocation_crit[2] <- sum(allocation_crit[1:2])
    } else if (nextBest@method == "max") {
      allocation_crit[length(allocation_crit)] <- sum(allocation_crit[1], tail(allocation_crit, 1))
    } else if (nextBest@method == "none") {
      allocation_crit[2] <- max(allocation_crit[1:2])
    }
    allocation_crit <- allocation_crit[-1]

    # Determine the dose with the highest frequency, exclude first element of
    # vector, i.e. no dose is safe (as covered before)
    dose_target <- data@doseGrid[which.max(allocation_crit)]

    # Determine next dose
    doses_eligible <- h_next_best_eligible_doses(data@doseGrid, doselimit, data@placebo)
    next_dose_level <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level]

    ### How to display placebo and no dose safe?
    # Create a plot.
    p <- ggplot(
      data = data.frame(
        x = as.factor(data@doseGrid),
        y = as.numeric(allocation_crit) * 100
      ),
      fill = "grey50",
      colour = "grey50"
    ) +
      geom_col(aes(x, y)) +
      scale_x_discrete() +
      geom_text(
        mapping = aes(
          x = x,
          y = y,
          label = paste(round(y, 1), "%")
        ),
        size = 3,
        hjust = 1.5,
        vjust = -0.5
      ) +
      geom_vline(
        xintercept = as.factor(dose_target),
        linetype = "dotted",
        size = 1,
        colour = "green"
      ) +
      geom_text(
        data = data.frame(x = as.factor(dose_target)),
        aes(.data$x, 0),
        label = "Best",
        vjust = -0.5,
        hjust = -0.5,
        colour = "green",
        angle = 90
      ) +
      xlab("Dose") +
      ylab(paste("Allocation criterion [%]"))


    if (is.finite(doselimit)) {
      p <- p +
        geom_vline(
          xintercept = as.factor(doselimit),
          linetype = "dotdash",
          colour = "red", lwd = 1.1
        ) +
        geom_text(
          data = data.frame(x = as.factor(doselimit)),
          aes(.data$x, 0),
          label = "Max",
          vjust = -0.5,
          hjust = -2,
          colour = "red",
          angle = 90
        )
    }
    p <- p +
      geom_vline(
        xintercept = as.factor(next_dose),
        colour = "blue", lwd = 0.6
      ) +
      geom_text(
        data = data.frame(x = as.factor(next_dose)),
        aes(.data$x, 0),
        label = "Next",
        vjust = -0.5,
        hjust = -3,
        colour = "blue",
        angle = 90
      )

    list(value = next_dose, plot = p)

  })

# nolint start

## --------------------------------------------------
## Determine the maximum possible next dose
## --------------------------------------------------

##' Determine the maximum possible next dose
##'
##' Determine the upper limit of the next dose based on the increments rule.
##'
##' This function outputs the maximum possible next dose, based on the
##' corresponding rule \code{increments} and the \code{data}.
##'
##' @param increments The rule, an object of class
##' \code{\linkS4class{Increments}}
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots further arguments
##' @return the maximum possible next dose
##'
##' @export
##' @keywords methods
setGeneric("maxDose",
  def =
    function(increments, data, ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("maxDose")
    },
  valueClass = "numeric"
)


## --------------------------------------------------
## The maximum allowable relative increments in intervals method
## --------------------------------------------------

##' @describeIn maxDose Determine the maximum possible next dose based on
##' relative increments
##'
##' @example examples/Rules-method-maxDose-IncrementsRelative.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementsRelative",
      data = "Data"
    ),
  def =
    function(increments, data, ...) {
      ## determine what was the last dose
      lastDose <- tail(data@x, 1)

      ## determine in which interval this dose was
      lastInterval <-
        findInterval(
          x = lastDose,
          vec = increments@intervals
        )

      ## so the maximum next dose is
      ret <-
        (1 + increments@increments[lastInterval]) *
          lastDose

      return(ret)
    }
)

# maxDose-IncrementsNumDoseLevels ----

#' @rdname maxDose
#'
#' @description Increments control based on number of dose levels
#'   Increment rule to determine the maximum possible next dose based on
#'   maximum dose levels to increment for the next dose.
#'   Increment rule can be applied to last dose or maximum dose given so far.
#'
#' @aliases maxDose-IncrementsNumDoseLevels
#' @example examples/Rules-method-maxDose-IncrementsNumDoseLevels.R
#' @export
#'
setMethod(
  "maxDose",
  signature = signature(
    increments = "IncrementsNumDoseLevels",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    # Determine what is the basis level for increment,
    # i.e. the last dose or the max dose applied.
    basis_dose_level <- ifelse(
      increments@basis_level == "last",
      tail(
        data@xLevel,
        1
      ),
      max(data@xLevel)
    )

    max_next_dose_level <- min(
      length(data@doseGrid),
      basis_dose_level + increments@max_levels
    )

    data@doseGrid[max_next_dose_level]
  }
)


# maxDose-IncrementsHSRBeta ----

#' @rdname maxDose
#'
#' @description Determine the maximum possible dose for escalation.
#'
#' @aliases maxDose-IncrementsHSRBeta
#' @example examples/Rules-method-maxDose-IncrementsHSRBeta.R
#' @export
setMethod(
  "maxDose",
  signature = signature(
    increments = "IncrementsHSRBeta",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    # Summary of observed data per dose level.
    y <- factor(data@y, levels = c("0", "1"))
    dlt_tab <- table(y, data@x)

    # Ignore placebo if applied.
    if (data@placebo == TRUE & min(data@x) == data@doseGrid[1]) {
      dlt_tab <- dlt_tab[, -1]
    }

    # Extract dose names as these get lost if only one dose available.
    non_plcb_doses <- unique(sort(as.numeric(colnames(dlt_tab))))

    # Toxicity probability per dose level.
    x <- dlt_tab[2, ]
    n <- apply(dlt_tab, 2, sum)
    tox_prob <- pbeta(
      increments@target,
      x + increments@a,
      n - x + increments@b,
      lower.tail = FALSE
    )

    # Return the min toxic dose level or maximum dose level if no dose is toxic,
    # while ignoring placebo.
    dose_tox <- if (sum(tox_prob > increments@prob) > 0) {
      min(non_plcb_doses[which(tox_prob > increments@prob)])
    } else {
      # Add small value to max dose, so that the max dose is always smaller.
      max(data@doseGrid) + 0.01
    }

    # Determine the next maximum possible dose.
    # In case that the first active dose is above probability threshold,
    # the first active dose is reported as maximum. I.e. in case that placebo is used,
    # the second dose is reported. Please note that this rule should be used together
    # with the hard safety stopping rule to avoid inconsistent results.
    max(data@doseGrid[data@doseGrid < dose_tox], data@doseGrid[data@placebo + 1])
  }
)

## --------------------------------------------------
## The maximum allowable relative increments, with special rules for
## part 1 and beginning of part 2, method method
## --------------------------------------------------

##' @describeIn maxDose Determine the maximum possible next dose based on
##' relative increments and part 1 and 2
##' @example examples/Rules-method-maxDose-IncrementsRelativeParts.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementsRelativeParts",
      data = "DataParts"
    ),
  def =
    function(increments, data, ...) {

      ## determine if there are already cohorts
      ## belonging to part 2:
      alreadyInPart2 <- any(data@part == 2L)

      ## if so, we just call the next higher method
      if (alreadyInPart2) {
        callNextMethod(increments, data, ...)
      } else {
        ## otherwise we have special rules.

        ## what dose level (index) has the highest dose
        ## so far?
        lastDoseLevel <- matchTolerance(
          max(data@x),
          data@part1Ladder
        )

        ## determine the next maximum dose
        ret <-
          if (data@nextPart == 1L) {
            ## here the next cohort will still be in part 1.
            ## Therefore we just make one step on the part 1 ladder:
            data@part1Ladder[lastDoseLevel + 1L]
          } else {
            ## the next cohort will start part 2.

            ## if there was a DLT so far:
            if (any(data@y == 1L)) {
              data@part1Ladder[lastDoseLevel + increments@dlt_start]
            } else {
              ## otherwise
              if (increments@clean_start > 0) {
                ## if we want to start part 2 higher than
                ## the last part 1 dose, use usual increments
                callNextMethod(increments, data, ...)
              } else {
                ## otherwise
                data@part1Ladder[lastDoseLevel + increments@clean_start]
              }
            }
          }

        return(ret)
      }
    }
)


## --------------------------------------------------
## The maximum allowable relative increments in terms of DLTs
## --------------------------------------------------

##' @describeIn maxDose Determine the maximum possible next dose based on
##' relative increments determined by DLTs so far
##'
##' @example examples/Rules-method-maxDose-IncrementsRelativeDLT.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementsRelativeDLT",
      data = "Data"
    ),
  def =
    function(increments, data, ...) {
      ## determine what was the last dose
      lastDose <- tail(data@x, 1)

      ## determine how many DLTs have occurred so far
      dltHappened <- sum(data@y)

      ## determine in which interval this is
      interval <-
        findInterval(
          x = dltHappened,
          vec = increments@dlt_intervals
        )

      ## so the maximum next dose is
      ret <-
        (1 + increments@increments[interval]) *
          lastDose

      return(ret)
    }
)

## --------------------------------------------------
## The maximum allowable relative increments in terms of DLTs
## --------------------------------------------------

##' @describeIn maxDose Determine the maximum possible next dose based on
##' relative increments determined by DLTs in the current cohort.
##'
##' @example examples/Rules-method-maxDose-IncrementsRelativeDLTCurrent.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementsRelativeDLTCurrent",
      data = "Data"
    ),
  def =
    function(increments, data, ...) {

      # Determine what was the last dose.
      lastDose <- tail(data@x, 1)

      # Determine how many DLTs have occurred in last cohort.
      lastCohort <- tail(data@cohort, 1)
      index <- which(data@cohort == lastCohort)
      dltHappened <- sum(data@y[index])

      # Determine in which interval this is.
      interval <-
        findInterval(
          x = dltHappened,
          vec = increments@dlt_intervals
        )

      (1 + increments@increments[interval]) * lastDose
    }
)


## --------------------------------------------------
## The maximum allowable relative increments in terms of DLTs
## --------------------------------------------------

##' @describeIn maxDose Determine the maximum possible next dose based on
##' multiple increment rules (taking the minimum across individual increments).
##'
##' @example examples/Rules-method-maxDose-IncrementsMin.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementsMin",
      data = "Data"
    ),
  def =
    function(increments, data, ...) {

      ## apply the multiple increment rules
      individualResults <-
        sapply(increments@increments_list,
          maxDose,
          data = data,
          ...
        )

      ## so the maximum increment is the minimum across the individual increments
      ret <- min(individualResults)

      return(ret)
    }
)


## ============================================================

## --------------------------------------------------
## "AND" combination of stopping rules
## --------------------------------------------------

##' The method combining two atomic stopping rules
##'
##' @param e1 First \code{\linkS4class{Stopping}} object
##' @param e2 Second \code{\linkS4class{Stopping}} object
##' @return The \code{\linkS4class{StoppingAll}} object
##'
##' @example examples/Rules-method-and-stopping-stopping.R
##' @keywords methods
setMethod("&",
  signature(
    e1 = "Stopping",
    e2 = "Stopping"
  ),
  def =
    function(e1, e2) {
      StoppingAll(list(e1, e2))
    }
)

##' The method combining a stopping list and an atomic
##'
##' @param e1 \code{\linkS4class{StoppingAll}} object
##' @param e2 \code{\linkS4class{Stopping}} object
##' @return The modified \code{\linkS4class{StoppingAll}} object
##'
##' @example examples/Rules-method-and-stoppingAll-stopping.R
##' @keywords methods
setMethod("&",
  signature(
    e1 = "StoppingAll",
    e2 = "Stopping"
  ),
  def =
    function(e1, e2) {
      e1@stop_list <- c(
        e1@stop_list,
        e2
      )
      return(e1)
    }
)

##' The method combining an atomic and a stopping list
##'
##' @param e1 \code{\linkS4class{Stopping}} object
##' @param e2 \code{\linkS4class{StoppingAll}} object
##' @return The modified \code{\linkS4class{StoppingAll}} object
##'
##' @example examples/Rules-method-and-stopping-stoppingAll.R
##' @keywords methods
setMethod("&",
  signature(
    e1 = "Stopping",
    e2 = "StoppingAll"
  ),
  def =
    function(e1, e2) {
      e2@stop_list <- c(
        e1,
        e2@stop_list
      )
      return(e2)
    }
)

## --------------------------------------------------
## "OR" combination of stopping rules
## --------------------------------------------------

##' The method combining two atomic stopping rules
##'
##' @param e1 First \code{\linkS4class{Stopping}} object
##' @param e2 Second \code{\linkS4class{Stopping}} object
##' @return The \code{\linkS4class{StoppingAny}} object
##'
##' @aliases |,Stopping,Stopping-method
##' @name or-Stopping-Stopping
##' @example examples/Rules-method-or-stopping-stopping.R
##' @keywords methods
setMethod("|",
  signature(
    e1 = "Stopping",
    e2 = "Stopping"
  ),
  def =
    function(e1, e2) {
      StoppingAny(list(e1, e2))
    }
)

##' The method combining a stopping list and an atomic
##'
##' @param e1 \code{\linkS4class{StoppingAny}} object
##' @param e2 \code{\linkS4class{Stopping}} object
##' @return The modified \code{\linkS4class{StoppingAny}} object
##'
##' @aliases |,StoppingAny,Stopping-method
##' @name or-Stopping-StoppingAny
##' @example examples/Rules-method-or-stoppingAny-stopping.R
##' @keywords methods
setMethod("|",
  signature(
    e1 = "StoppingAny",
    e2 = "Stopping"
  ),
  def =
    function(e1, e2) {
      e1@stop_list <- c(
        e1@stop_list,
        e2
      )
      return(e1)
    }
)

##' The method combining an atomic and a stopping list
##'
##' @param e1 \code{\linkS4class{Stopping}} object
##' @param e2 \code{\linkS4class{StoppingAny}} object
##' @return The modified \code{\linkS4class{StoppingAny}} object
##'
##' @aliases |,Stopping,StoppingAny-method
##' @name or-StoppingAny-Stopping
##' @example examples/Rules-method-or-stopping-stoppingAny.R
##' @keywords methods
setMethod("|",
  signature(
    e1 = "Stopping",
    e2 = "StoppingAny"
  ),
  def =
    function(e1, e2) {
      e2@stop_list <- c(
        e1,
        e2@stop_list
      )
      return(e2)
    }
)



## --------------------------------------------------
## Stop the trial?
## --------------------------------------------------

##' Stop the trial?
##'
##' This function returns whether to stop the trial.
##'
##' @param stopping The rule, an object of class
##' \code{\linkS4class{Stopping}}
##' @param dose the recommended next best dose
##' @param samples the \code{\linkS4class{Samples}} object
##' @param model The model input, an object of class \code{\linkS4class{GeneralModel}}
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots additional arguments
##'
##' @return logical value: \code{TRUE} if the trial can be stopped, \code{FALSE}
##' otherwise. It should have an attribute \code{message} which gives the reason
##' for the decision.
##'
##' @export
##' @example examples/Rules-method-CombiningStoppingRulesAndOr.R
##' @keywords methods
setGeneric("stopTrial",
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## if the recommended next dose is NA,
      ## stop in any case.
      if (is.na(dose)) {
        return(structure(TRUE,
          message = "Recommended next best dose is NA"
        ))
      } else if (data@placebo && dose == min(data@doseGrid)) {
        return(structure(TRUE,
          message = "Recommended next best dose is placebo dose"
        ))
      }

      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("stopTrial")
    },
  valueClass = "logical"
)


## --------------------------------------------------
## Stopping based on multiple stopping rules
## --------------------------------------------------

##' @describeIn stopTrial Stop based on multiple stopping rules
##' @example examples/Rules-method-stopTrial-StoppingList.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingList",
      dose = "ANY",
      samples = "ANY",
      model = "ANY",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## evaluate the individual stopping rules
      ## in the list
      individualResults <-
        if (missing(samples)) {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            samples = samples,
            model = model,
            data = data,
            ...
          )
        }

      ## summarize to obtain overall result
      overallResult <- stopping@summary(as.logical(individualResults))

      ## retrieve individual text messages,
      ## but let them in the list structure
      overallText <- lapply(individualResults, attr, "message")

      return(structure(overallResult,
        message = overallText
      ))
    }
)

## --------------------------------------------------
## Stopping based on fulfillment of all multiple stopping rules
## --------------------------------------------------

##' @describeIn stopTrial Stop based on fulfillment of all multiple stopping
##' rules
##'
##' @example examples/Rules-method-stopTrial-StoppingAll.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingAll",
      dose = "ANY",
      samples = "ANY",
      model = "ANY",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## evaluate the individual stopping rules
      ## in the list
      individualResults <-
        if (missing(samples)) {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            samples = samples,
            model = model,
            data = data,
            ...
          )
        }

      ## summarize to obtain overall result
      overallResult <- all(as.logical(individualResults))

      ## retrieve individual text messages,
      ## but let them in the list structure
      overallText <- lapply(individualResults, attr, "message")

      return(structure(overallResult,
        message = overallText
      ))
    }
)


## --------------------------------------------------
## Stopping based on fulfillment of any stopping rule
## --------------------------------------------------

##' @describeIn stopTrial Stop based on fulfillment of any stopping rule
##'
##' @example examples/Rules-method-stopTrial-StoppingAny.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingAny",
      dose = "ANY",
      samples = "ANY",
      model = "ANY",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## evaluate the individual stopping rules
      ## in the list
      individualResults <-
        if (missing(samples)) {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stop_list,
            stopTrial,
            dose = dose,
            samples = samples,
            model = model,
            data = data,
            ...
          )
        }

      ## summarize to obtain overall result
      overallResult <- any(as.logical(individualResults))

      ## retrieve individual text messages,
      ## but let them in the list structure
      overallText <- lapply(individualResults, attr, "message")

      return(structure(overallResult,
        message = overallText
      ))
    }
)




## --------------------------------------------------
## Stopping based on number of cohorts near to next best dose
## --------------------------------------------------

##' @describeIn stopTrial Stop based on number of cohorts near to next best dose
##'
##' @example examples/Rules-method-stopTrial-StoppingCohortsNearDose.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingCohortsNearDose",
      dose = "numeric",
      samples = "ANY",
      model = "ANY",
      data = "Data"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## determine the range where the cohorts must lie in
      lower <- (100 - stopping@percentage) / 100 * dose
      upper <- (100 + stopping@percentage) / 100 * dose

      ## which patients lie there?
      indexPatients <- which((data@x >= lower) & (data@x <= upper))

      ## how many cohorts?
      nCohorts <- length(unique(data@cohort[indexPatients]))

      ## so can we stop?
      doStop <- nCohorts >= stopping@nCohorts

      ## generate message
      text <- paste(nCohorts,
        " cohorts lie within ",
        stopping@percentage,
        "% of the next best dose ",
        dose,
        ". This ",
        ifelse(doStop, "reached", "is below"),
        " the required ",
        stopping@nCohorts,
        " cohorts",
        sep = ""
      )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)


## -------------------------------------------------------------
## Stopping based on number of patients near to next best dose
## -------------------------------------------------------------

##' @describeIn stopTrial Stop based on number of patients near to next best
##' dose
##'
##' @example examples/Rules-method-stopTrial-StoppingPatientsNearDose.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingPatientsNearDose",
      dose = "numeric",
      samples = "ANY",
      model = "ANY",
      data = "Data"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## determine the range where the cohorts must lie in
      lower <- (100 - stopping@percentage) / 100 * dose
      upper <- (100 + stopping@percentage) / 100 * dose

      ## how many patients lie there?
      nPatients <- sum((data@x >= lower) & (data@x <= upper))

      ## so can we stop?
      doStop <- nPatients >= stopping@nPatients

      ## generate message
      text <- paste(nPatients,
        " patients lie within ",
        stopping@percentage,
        "% of the next best dose ",
        dose,
        ". This ",
        ifelse(doStop, "reached", "is below"),
        " the required ",
        stopping@nPatients,
        " patients",
        sep = ""
      )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)

## --------------------------------------------------
## Stopping based on minimum number of cohorts
## --------------------------------------------------

##' @describeIn stopTrial Stop based on minimum number of cohorts
##'
##' @example examples/Rules-method-stopTrial-StoppingMinCohorts.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingMinCohorts",
      dose = "ANY",
      samples = "ANY",
      model = "ANY",
      data = "Data"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## determine number of cohorts
      nCohorts <- length(unique(data@cohort))

      ## so can we stop?
      doStop <- nCohorts >= stopping@nCohorts

      ## generate message
      text <-
        paste(
          "Number of cohorts is",
          nCohorts,
          "and thus",
          ifelse(doStop, "reached", "below"),
          "the prespecified minimum number",
          stopping@nCohorts
        )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)

## --------------------------------------------------
## Stopping based on minimum number of patients
## --------------------------------------------------

##' @describeIn stopTrial Stop based on minimum number of patients
##'
##' @example examples/Rules-method-stopTrial-StoppingMinPatients.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingMinPatients",
      dose = "ANY",
      samples = "ANY",
      model = "ANY",
      data = "Data"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## so can we stop?
      doStop <- data@nObs >= stopping@nPatients

      ## generate message
      text <-
        paste(
          "Number of patients is",
          data@nObs,
          "and thus",
          ifelse(doStop, "reached", "below"),
          "the prespecified minimum number",
          stopping@nPatients
        )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)


## --------------------------------------------------
## Stopping based on probability of target tox interval
## --------------------------------------------------

##' @describeIn stopTrial Stop based on probability of target tox interval
##'
##' @example examples/Rules-method-stopTrial-StoppingTargetProb.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingTargetProb",
      dose = "numeric",
      samples = "Samples",
      model = "GeneralModel",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## first we have to get samples from the dose-tox
      ## curve at the dose.
      probSamples <- prob(
        dose = dose,
        model,
        samples
      )

      ## Now compute probability to be in target interval
      probTarget <-
        mean((probSamples >= stopping@target[1]) &
          (probSamples <= stopping@target[2]))

      ## so can we stop?
      doStop <- probTarget >= stopping@prob

      ## generate message
      text <-
        paste(
          "Probability for target toxicity is",
          round(probTarget * 100),
          "% for dose",
          dose,
          "and thus",
          ifelse(doStop, "above", "below"),
          "the required",
          round(stopping@prob * 100),
          "%"
        )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)


## --------------------------------------------------
## Stopping based on MTD distribution
## --------------------------------------------------

##' @describeIn stopTrial Stop based on MTD distribution
##'
##' @example examples/Rules-method-stopTrial-StoppingMTDdistribution.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingMTDdistribution",
      dose = "numeric",
      samples = "Samples",
      model = "GeneralModel",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## First, generate the MTD samples.

      ## add prior data and samples to the
      ## function environment so that they
      ## can be used.
      mtdSamples <- dose(
        x = stopping@target,
        model,
        samples
      )

      ## what is the absolute threshold?
      absThresh <- stopping@thresh * dose

      ## what is the probability to be above this dose?
      prob <- mean(mtdSamples > absThresh)

      ## so can we stop?
      doStop <- prob >= stopping@prob

      ## generate message
      text <-
        paste(
          "Probability of MTD above",
          round(stopping@thresh * 100),
          "% of current dose",
          dose,
          "is",
          round(prob * 100),
          "% and thus",
          ifelse(doStop, "above", "below"),
          "the required",
          round(stopping@prob * 100),
          "%"
        )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)

## StoppingMTDCV ----

#' @rdname stopTrial
#'
#' @description Stopping rule based precision of the MTD estimation.
#'   The trial is stopped, when the MTD can be estimated with sufficient precision.
#'   The criteria is based on the robust coefficient of variation (CV) calculated
#'   from the posterior distribution.
#'   The robust CV is defined `mad(MTD) / median(MTD)`, where `mad` is the median
#'   absolute deviation.
#'
#' @aliases stopTrial-StoppingMTDCV
#' @example examples/Rules-method-stopTrial-StoppingMTDCV.R
#' @export
#'
setMethod(
  "stopTrial",
  signature = signature(
    stopping = "StoppingMTDCV",
    dose = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    mtd_samples <- dose(
      x = stopping@target,
      model,
      samples
    )
    # CV of MTD expressed as percentage, derived based on MTD posterior samples.
    mtd_cv <- (mad(mtd_samples) / median(mtd_samples)) * 100
    do_stop <- (mtd_cv <= stopping@thresh_cv) && (mtd_cv >= 0)

    msg <- paste(
      "CV of MTD is",
      round(mtd_cv),
      "% and thus",
      ifelse(do_stop, "below", "above"),
      "the required precision threshold of",
      round(stopping@thresh_cv),
      "%"
    )

    structure(do_stop, message = msg)
  }
)


## StoppingLowestDoseHSRBeta ----

#' @rdname stopTrial
#'
#' @description Stopping based based on the lowest non placebo dose. The trial is
#'  stopped when the lowest non placebo dose meets the Hard
#'  Safety Rule, i.e. it is deemed to be overly toxic. Stopping is based on the
#'  observed data at the lowest dose level using a Bin-Beta model
#'  based on DLT probability.
#'
#' @aliases stopTrial-StoppingLowestDoseHSRBeta
#' @example examples/Rules-method-stopTrial-StoppingLowestDoseHSRBeta.R
#' @export
setMethod(
  "stopTrial",
  signature = signature(
    stopping = "StoppingLowestDoseHSRBeta",
    dose = "numeric",
    samples = "Samples"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Actual number of patients at first active dose.
    n <- sum(data@x == data@doseGrid[data@placebo + 1])

    # Determine toxicity probability of the first active dose.
    tox_prob_first_dose <-
      if (n > 0) {
        x <- sum(data@y[which(data@x == data@doseGrid[data@placebo + 1])])
        pbeta(stopping@target, x + stopping@a, n - x + stopping@b, lower.tail = FALSE)
      } else {
        0
      }

    do_stop <- tox_prob_first_dose > stopping@prob

    # generate message
    msg <- if (n == 0) {
      "Lowest active dose not tested, stopping rule not applied."
    } else {
      paste(
        "Probability that the lowest active dose of ",
        data@doseGrid[data@placebo + 1],
        " being toxic based on posterior Beta distribution using a Beta(",
        stopping@a, ",", stopping@b, ") prior is ",
        round(tox_prob_first_dose * 100),
        "% and thus ",
        ifelse(do_stop, "above", "below"),
        " the required ",
        round(stopping@prob * 100),
        "% threshold.",
        sep = ""
      )
    }

    structure(do_stop, message = msg)
  }
)

## --------------------------------------------------
## Stopping based on probability of targeting biomarker
## --------------------------------------------------

##' @describeIn stopTrial Stop based on probability of targeting biomarker
##'
##' @example examples/Rules-method-stopTrial-StoppingTargetBiomarker.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingTargetBiomarker",
      dose = "numeric",
      samples = "Samples",
      model = "DualEndpoint",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      ## compute the target biomarker prob at this dose
      ## get the biomarker level samples
      ## at the dose grid points.
      biomLevelSamples <- biomarker(xLevel = seq_len(data@nGrid), model, samples)

      ## if target is relative to maximum
      if (stopping@is_relative) {

        ## If there is an 'Emax' parameter, target biomarker level will
        ## be relative to 'Emax', otherwise will be relative to the
        ## maximum biomarker level achieved in the given dose range.
        if ("Emax" %in% names(samples)) {

          ## For each sample, look which dose is maximizing the
          ## simultaneous probability to be in the target biomarker
          ## range and below overdose toxicity
          probTarget <- numeric(ncol(biomLevelSamples))
          probTarget <- sapply(
            seq(1, ncol(biomLevelSamples)),
            function(x) {
              sum(biomLevelSamples[, x] >= stopping@target[1] * samples@data$Emax &
                biomLevelSamples[, x] <= stopping@target[2] * samples@data$Emax) / nrow(biomLevelSamples)
            }
          )
        } else {

          ## For each sample, look which was the minimum dose giving
          ## relative target level
          targetIndex <- apply(
            biomLevelSamples, 1L,
            function(x) {
              rnx <- range(x)
              min(which((x >= stopping@target[1] * diff(rnx) + rnx[1]) &
                (x <= stopping@target[2] * diff(rnx) + rnx[1] + 1e-10)))
            }
          )

          probTarget <- numeric(ncol(biomLevelSamples))
          tab <- table(targetIndex)
          probTarget[as.numeric(names(tab))] <- tab
          probTarget <- probTarget / nrow(biomLevelSamples)
        }
      } else {
        ## otherwise target is absolute

        # For each sample, look which dose is maximizing the
        ## simultaneous probability to be in the target biomarker
        ## range and below overdose toxicity
        probTarget <- numeric(ncol(biomLevelSamples))
        probTarget <- sapply(
          seq(1, ncol(biomLevelSamples)),
          function(x) {
            sum(biomLevelSamples[, x] >= stopping@target[1] &
              biomLevelSamples[, x] <= stopping@target[2]) /
              nrow(biomLevelSamples)
          }
        )
      }

      ## so for this dose we have:
      probTarget <- probTarget[which(data@doseGrid == dose)]

      ## so can we stop?
      doStop <- probTarget >= stopping@prob

      ## generate message
      text <-
        paste(
          "Probability for target biomarker is",
          round(probTarget * 100),
          "% for dose",
          dose,
          "and thus",
          ifelse(doStop, "above", "below"),
          "the required",
          round(stopping@prob * 100),
          "%"
        )

      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)

## StoppingSpecificDose ----

#' @describeIn stopTrial if Stopping rule is met for specific dose of the planned
#' dose grid and not just for the default next best dose.
#'
#' @aliases stopTrial-StoppingSpecificDose
#'
#' @export
#' @example examples/Rules-method-stopTrial-StoppingSpecificDose.R
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingSpecificDose",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Specific dose must be a part of the dose grid.
    assert_subset(x = stopping@dose@.Data, choices = data@doseGrid)

    # Evaluate the original (wrapped) stopping rule at the specific dose.
    result <- stopTrial(
      stopping = stopping@rule,
      dose = stopping@dose@.Data,
      samples = samples,
      model = model,
      data = data,
      ...
    )
    # Correct the text message from the original stopping rule.
    attr(result, "message") <- gsub(
      pattern = "next best",
      replacement = "specific",
      x = attr(result, "message"),
      ignore.case = TRUE
    )
    result
  }
)



## --------------------------------------------------
## Stopping when the highest dose is reached
## --------------------------------------------------

##' @describeIn stopTrial Stop when the highest dose is reached
##'
##' @example examples/Rules-method-stopTrial-StoppingHighestDose.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingHighestDose",
      dose = "numeric",
      samples = "ANY",
      model = "ANY",
      data = "Data"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      isHighestDose <- (dose == data@doseGrid[data@nGrid])
      return(structure(isHighestDose,
        message =
          paste(
            "Next best dose is", dose, "and thus",
            ifelse(isHighestDose, "the",
              "not the"
            ),
            "highest dose"
          )
      ))
    }
)

## ============================================================

## --------------------------------------------------
## "MAX" combination of cohort size rules
## --------------------------------------------------

##' "MAX" combination of cohort size rules
##'
##' This function combines cohort size rules by taking
##' the maximum of all sizes.
##'
##' @param \dots Objects of class \code{\linkS4class{CohortSize}}
##' @return the combination as an object of class
##' \code{\linkS4class{CohortSizeMax}}
##'
##' @seealso \code{\link{minSize}}
##' @export
##' @keywords methods
setGeneric("maxSize",
  def =
    function(...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("maxSize")
    },
  valueClass = "CohortSizeMax"
)

##' @describeIn maxSize The method combining cohort size rules by taking maximum
##' @example examples/Rules-method-maxSize.R
setMethod("maxSize",
  "CohortSize",
  def =
    function(...) {
      CohortSizeMax(list(...))
    }
)

## --------------------------------------------------
## "MIN" combination of cohort size rules
## --------------------------------------------------

##' "MIN" combination of cohort size rules
##'
##' This function combines cohort size rules by taking
##' the minimum of all sizes.
##'
##' @param \dots Objects of class \code{\linkS4class{CohortSize}}
##' @return the combination as an object of class
##' \code{\linkS4class{CohortSizeMin}}
##'
##' @seealso \code{\link{maxSize}}
##' @export
##' @keywords methods
setGeneric("minSize",
  def =
    function(...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("minSize")
    },
  valueClass = "CohortSizeMin"
)

##' @describeIn minSize The method combining cohort size rules by taking minimum
##' @example examples/Rules-method-minSize.R
setMethod("minSize",
  "CohortSize",
  def =
    function(...) {
      CohortSizeMin(list(...))
    }
)

# size ----

## CohortSizeRange ----

#' @describeIn size Determines the size of the next cohort based on the range
#'   into which the next dose falls into.
#'
#' @param dose the next dose.
#' @param data the data input, an object of class [`Data`].
#'
#' @aliases size-CohortSizeRange
#' @example examples/Rules-method-size-CohortSizeRange.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeRange"
  ),
  definition = function(object, dose, data) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      return(0L)
    }
    assert_class(data, "Data")

    # Determine in which interval the next dose is.
    interval <- findInterval(x = dose, vec = object@intervals)
    object@cohort_size[interval]
  }
)

## CohortSizeDLT ----

#' @describeIn size Determines the size of the next cohort based on the number
#'   of DLTs so far.
#'
#' @param dose the next dose.
#' @param data the data input, an object of class [`Data`].
#'
#' @aliases size-CohortSizeDLT
#' @example examples/Rules-method-size-CohortSizeDLT.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeDLT"
  ),
  definition = function(object, dose, data) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      return(0L)
    }
    assert_class(data, "Data")

    # Determine how many DLTs have occurred so far.
    dlt_happened <- sum(data@y)

    # Determine in which interval this is.
    interval <- findInterval(x = dlt_happened, vec = object@dlt_intervals)
    object@cohort_size[interval]
  }
)

## CohortSizeMax ----

#' @describeIn size Determines the size of the next cohort based on maximum of
#'   multiple cohort size rules.
#'
#' @param dose the next dose.
#' @param data the data input, an object of class [`Data`].
#'
#' @aliases size-CohortSizeMax
#' @example examples/Rules-method-size-CohortSizeMax.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeMax"
  ),
  definition = function(object, dose, data) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      return(0L)
    }
    assert_class(data, "Data")

    # Evaluate the individual cohort size rules in the list.
    individual_results <- sapply(
      object@cohort_size_list,
      size,
      dose = dose,
      data = data
    )
    # The overall result.
    max(individual_results)
  }
)

## CohortSizeMin ----

#' @describeIn size Determines the size of the next cohort based on minimum of
#'   multiple cohort size rules.
#'
#' @param dose the next dose.
#' @param data the data input, an object of class [`Data`].
#'
#' @aliases size-CohortSizeMin
#' @example examples/Rules-method-size-CohortSizeMin.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeMin"
  ),
  definition = function(object, dose, data) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      return(0L)
    }
    assert_class(data, "Data")

    # Evaluate the individual cohort size rules in the list.
    individual_results <- sapply(
      object@cohort_size_list,
      size,
      dose = dose,
      data = data
    )
    # The overall result.
    min(individual_results)
  }
)

## CohortSizeConst ----

#' @describeIn size Constant cohort size.
#'
#' @param dose the next dose.
#' @param ... not used.
#'
#' @aliases size-CohortSizeConst
#' @example examples/Rules-method-size-CohortSizeConst.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeConst"
  ),
  definition = function(object, dose, ...) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      0L
    } else {
      object@size
    }
  }
)

## CohortSizeParts ----

#' @describeIn size Determines the size of the next cohort based on the parts.
#'
#' @param dose the next dose.
#' @param data the data input, an object of class [`Data`].
#'
#' @aliases size-CohortSizeParts
#' @example examples/Rules-method-size-CohortSizeParts.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeParts"
  ),
  definition = function(object, dose, data) {
    # If the recommended next dose is NA, don't check it and return 0.
    if (is.na(dose)) {
      return(0L)
    } else {
      assert_class(data, "DataParts")
      object@sizes[data@nextPart]
    }
  }
)

## ------------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the upper to the lower 95% credibility interval
## ------------------------------------------------------------------------------------------------
##' @describeIn stopTrial Stop based on 'StoppingTDCIRatio' class when
##' reaching the target ratio of the upper to the lower 95% credibility
##' interval of the estimate (TDtargetEndOfTrial). This is a stopping rule which incorporate only
##' DLE responses and DLE samples are given
##'
##' @example examples/Rules-method-stopTrialCITDsamples.R
##'
##' @export
##' @keywords methods
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingTDCIRatio",
    dose = "ANY",
    samples = "Samples",
    model = "ModelTox",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    assert_probability(stopping@prob_target)

    dose_target_samples <- dose(
      x = stopping@prob_target,
      model = model,
      samples = samples
    )
    # 95% credibility interval.
    dose_target_ci <- quantile(dose_target_samples, probs = c(0.025, 0.975))
    dose_target_ci_ratio <- dose_target_ci[[2]] / dose_target_ci[[1]]

    do_stop <- dose_target_ci_ratio <= stopping@target_ratio
    text <- paste0(
      "95% CI is (",
      paste(dose_target_ci, collapse = ", "),
      "), Ratio = ",
      round(dose_target_ci_ratio, 4),
      " is ",
      ifelse(do_stop, "less than or equal to ", "greater than "),
      "target_ratio = ", stopping@target_ratio
    )
    structure(do_stop, messgae = text)
  }
)

## ----------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the upper to the lower 95% credibility interval
## ------------------------------------------------------------------------------------------------
##' @describeIn stopTrial Stop based on 'StoppingTDCIRatio' class
##' when reaching the target ratio of the upper to the lower 95% credibility
##' interval of the estimate (TDtargetEndOfTrial). This is a stopping rule which incorporate only
##' DLE responses and no DLE samples are involved
##' @example examples/Rules-method-stopTrialCITD.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingTDCIRatio",
      dose = "ANY",
      samples = "missing",
      model = "ModelTox",
      data = "ANY"
    ),
  def =
    function(stopping, dose, model, data, ...) {
      assert_probability(stopping@prob_target)

      prob_target <- stopping@prob_target
      dose_target_samples <- dose(x = prob_target, model = model)
      ## Find the variance of the log of the dose_target_samples(eta)
      M1 <- matrix(c(-1 / (model@phi2), -(log(prob_target / (1 - prob_target)) - model@phi1) / (model@phi2)^2), 1, 2)
      M2 <- model@Pcov
      varEta <- as.vector(M1 %*% M2 %*% t(M1))

      ## Find the upper and lower limit of the 95% credibility interval
      CI <- exp(log(dose_target_samples) + c(-1, 1) * 1.96 * sqrt(varEta))
      ratio <- CI[2] / CI[1]

      ## so can we stop?
      doStop <- ratio <= stopping@target_ratio
      ## generate messgae
      text <- paste(
        "95% CI is (", round(CI[1], 4), ",", round(CI[2], 4), "), Ratio =", round(ratio, 4), "is ", ifelse(doStop, "is less than or equal to", "greater than"),
        "target_ratio =", stopping@target_ratio
      )
      ## return both
      return(structure(doStop,
        messgae = text
      ))
    }
)

## --------------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the upper to the lower 95% credibility interval
## ------------------------------------------------------------------------------------------------
##' @describeIn stopTrial Stop based on reaching the target ratio of the upper to the lower 95% credibility
##' interval of the estimate (the minimum of Gstar and TDtargetEndOfTrial). This is a stopping rule which
##' incorporate DLE and efficacy responses and DLE and efficacy samples are also used.
##'
##' @param TDderive the function which derives from the input, a vector of the posterior samples called
##' \code{TDsamples} of the dose
##' which has the probability of the occurrence of DLE equals to either the targetDuringTrial or
##' targetEndOfTrial, the final next best TDtargetDuringTrial (the dose with probability of the
##' occurrence of DLE equals to the targetDuringTrial)and TDtargetEndOfTrial estimate.
##' @param Effmodel the efficacy model of \code{\linkS4class{ModelEff}} class object
##' @param Effsamples the efficacy samples of \code{\linkS4class{Samples}} class object
##' @param Gstarderive the function which derives from the input, a vector of the posterior Gstar (the dose
##' which gives the maximum gain value) samples
##' called \code{Gstarsamples}, the final next best Gstar estimate.
##'
##' @example examples/Rules-method-stopTrialCIMaxGainSamples.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingMaxGainCIRatio",
      dose = "ANY",
      samples = "Samples",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(stopping, dose, samples, model, data, TDderive, Effmodel, Effsamples, Gstarderive, ...) {
      prob_target <- stopping@prob_target

      ## checks
      stopifnot(is.probability(prob_target))
      stopifnot(is(Effmodel, "ModelEff"))
      stopifnot(is(Effsamples, "Samples"))
      stopifnot(is.function(TDderive))
      stopifnot(is.function(Gstarderive))

      ## find the TDtarget End of Trial samples
      TDtargetEndOfTrialSamples <- dose(
        x = prob_target,
        model = model,
        samples = samples
      )
      ## Find the TDtarget End of trial estimate
      TDtargetEndOfTrialEstimate <- TDderive(TDtargetEndOfTrialSamples)

      ## Find the gain value samples then the GstarSamples
      points <- data@doseGrid

      GainSamples <- matrix(
        nrow = size(samples),
        ncol = length(points)
      )

      ## evaluate the probs, for all gain samples.
      for (i in seq_along(points))
      {
        ## Now we want to evaluate for the
        ## following dose:
        GainSamples[, i] <- gain(
          dose = points[i],
          model,
          samples,
          Effmodel,
          Effsamples
        )
      }

      ## Find the maximum gain value samples
      MaxGainSamples <- apply(GainSamples, 1, max)

      ## Obtain Gstar samples, samples for the dose level which gives the maximum gain value
      IndexG <- apply(GainSamples, 1, which.max)
      GstarSamples <- data@doseGrid[IndexG]

      ## Find the Gstar estimate

      Gstar <- Gstarderive(GstarSamples)
      ## Find the 95% credibility interval of Gstar and its ratio of the upper to the lower limit
      CIGstar <- quantile(GstarSamples, probs = c(0.025, 0.975))
      ratioGstar <- as.numeric(CIGstar[2] / CIGstar[1])

      ## Find the 95% credibility interval of TDtargetEndOfTrial and its ratio of the upper to the lower limit
      CITDEOT <- quantile(TDtargetEndOfTrialSamples, probs = c(0.025, 0.975))
      ratioTDEOT <- as.numeric(CITDEOT[2] / CITDEOT[1])

      ## Find which is smaller (TDtargetEndOfTrialEstimate or Gstar)

      if (TDtargetEndOfTrialEstimate <= Gstar) {

        ## Find the upper and lower limit of the 95% credibility interval and its ratio of the smaller
        CI <- CITDEOT
        ratio <- ratioTDEOT
        chooseTD <- TRUE
      } else {
        CI <- CIGstar
        ratio <- ratioGstar
        chooseTD <- FALSE
      }

      ## so can we stop?
      doStop <- ratio <= stopping@target_ratio
      ## generate messgae
      text1 <- paste(
        "Gstar estimate is", round(Gstar, 4), "with 95% CI (", round(CIGstar[1], 4), ",", round(CIGstar[2], 4),
        ") and its ratio =",
        round(ratioGstar, 4)
      )
      text2 <- paste(
        "TDtargetEndOfTrial estimate is ", round(TDtargetEndOfTrialEstimate, 4),
        "with 95% CI (", round(CITDEOT[1], 4), ",", round(CITDEOT[2], 4), ") and its ratio=",
        round(ratioTDEOT, 4)
      )
      text3 <- paste(
        ifelse(chooseTD, "TDatrgetEndOfTrial estimate", "Gstar estimate"), "is smaller with ratio =",
        round(ratio, 4), " which is ", ifelse(doStop, "is less than or equal to", "greater than"),
        "target_ratio =", stopping@target_ratio
      )
      text <- c(text1, text2, text3)
      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)

## -----------------------------------------------------------------------------------------------
## Stopping based on a target ratio of the upper to the lower 95% credibility interval
## --------------------------------------------------------------------------------------------
##' @describeIn stopTrial Stop based on reaching the target ratio of the upper to the lower 95% credibility
##' interval of the estimate (the minimum of Gstar and TDtargetEndOfTrial). This is a stopping rule which
##' incorporate DLE and efficacy responses without DLE and efficacy samples involved.
##' @example examples/Rules-method-stopTrialCIMaxGain.R
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingMaxGainCIRatio",
      dose = "ANY",
      samples = "missing",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(stopping, dose, model, data, Effmodel, ...) {
      prob_target <- stopping@prob_target

      ## checks
      stopifnot(is.probability(prob_target))
      stopifnot(is(Effmodel, "ModelEff"))


      ## find the TDtarget End of Trial
      TDtargetEndOfTrial <- dose(
        x = prob_target,
        model = model
      )

      ## Find the dose with maximum gain value
      Gainfun <- function(DOSE) {
        -gain(DOSE, model_dle = model, model_eff = Effmodel)
      }

      # if(data@placebo) {
      # n <- length(data@doseGrid)
      # LowestDose <- sort(data@doseGrid)[2]} else {
      LowestDose <- min(data@doseGrid)
      # }

      Gstar <- (optim(LowestDose, Gainfun, method = "L-BFGS-B", lower = LowestDose, upper = max(data@doseGrid))$par)
      MaxGain <- -(optim(LowestDose, Gainfun, method = "L-BFGS-B", lower = LowestDose, upper = max(data@doseGrid))$value)
      if (data@placebo) {
        logGstar <- log(Gstar + Effmodel@const)
      } else {
        logGstar <- log(Gstar)
      }



      ## From paper (Yeung et. al 2015)

      meanEffGstar <- Effmodel@theta1 + Effmodel@theta2 * log(logGstar)

      denom <- (model@phi2) * (meanEffGstar) * (1 + logGstar * model@phi2)

      dgphi1 <- -(meanEffGstar * logGstar * model@phi2 - Effmodel@theta2) / denom

      dgphi2 <- -((meanEffGstar) * logGstar + meanEffGstar * (logGstar)^2 * model@phi2 - Effmodel@theta2 * logGstar) / denom

      dgtheta1 <- -(logGstar * model@phi2) / denom

      dgtheta2 <- -(logGstar * exp(model@phi1 + model@phi2 * logGstar) * model@phi2 * log(logGstar) - 1 - exp(model@phi1 + model@phi2 * logGstar)) / denom

      # DLEPRO <- exp(model@phi1+model@phi2*logGstar)

      # dgphi1 <- Effmodel@theta2*DLEPRO - logGstar*model@phi2*meanEffGstar*DLEPRO

      # dgphi2 <- logGstar*DLEPRO *(Effmodel@theta2-(meanEffGstar)+model@phi2)

      # dgtheta1 <- -logGstar*DLEPRO*model@phi2

      # dgtheta2 <- 1+DLEPRO-logGstar*DLEPRO*model@phi2*log(logGstar)

      deltaG <- matrix(c(dgphi1, dgphi2, dgtheta1, dgtheta2), 4, 1)


      ## Find the variance of the log Gstar
      ## First find the covariance matrix of all the parameters, phi1, phi2, theta1 and theta2
      ## such that phi1 and phi2 and independent of theta1 and theta2
      emptyMatrix <- matrix(0, 2, 2)
      covBETA <- cbind(rbind(model@Pcov, emptyMatrix), rbind(emptyMatrix, Effmodel@Pcov))
      varlogGstar <- as.vector(t(deltaG) %*% covBETA %*% deltaG)

      ## Find the upper and lower limit of the 95% credibility interval of Gstar
      CIGstar <- exp(logGstar + c(-1, 1) * 1.96 * sqrt(varlogGstar))

      ## The ratio of the upper to the lower 95% credibility interval
      ratioGstar <- CIGstar[2] / CIGstar[1]

      ## Find the variance of the log of the TDtargetEndOfTrial(eta)
      M1 <- matrix(c(-1 / (model@phi2), -(log(prob_target / (1 - prob_target)) - model@phi1) / (model@phi2)^2), 1, 2)
      M2 <- model@Pcov

      varEta <- as.vector(M1 %*% M2 %*% t(M1))

      ## Find the upper and lower limit of the 95% credibility interval of
      ## TDtargetEndOfTrial
      CITDEOT <- exp(log(TDtargetEndOfTrial) + c(-1, 1) * 1.96 * sqrt(varEta))

      ## The ratio of the upper to the lower 95% credibility interval
      ratioTDEOT <- CITDEOT[2] / CITDEOT[1]

      if (Gstar <= TDtargetEndOfTrial) {
        chooseTD <- FALSE
        CI <- c()
        CI[2] <- CIGstar[2]
        CI[1] <- CIGstar[1]
        ratio <- ratioGstar
      } else {
        chooseTD <- TRUE
        CI <- c()
        CI[2] <- CITDEOT[2]
        CI[1] <- CITDEOT[1]
        ratio <- ratioTDEOT
      }
      ## so can we stop?
      doStop <- ratio <= stopping@target_ratio
      ## generate message

      text1 <- paste(
        "Gstar estimate is", round(Gstar, 4), "with 95% CI (", round(CIGstar[1], 4), ",", round(CIGstar[2], 4),
        ") and its ratio =",
        round(ratioGstar, 4)
      )
      text2 <- paste(
        "TDtargetEndOfTrial estimate is ", round(TDtargetEndOfTrial, 4),
        "with 95% CI (", round(CITDEOT[1], 4), ",", round(CITDEOT[2], 4), ") and its ratio=",
        round(ratioTDEOT, 4)
      )
      text3 <- paste(
        ifelse(chooseTD, "TDatrgetEndOfTrial estimate", "Gstar estimate"), "is smaller with ratio =",
        round(ratio, 4), "which is ", ifelse(doStop, "is less than or equal to", "greater than"),
        "target_ratio =", stopping@target_ratio
      )
      text <- c(text1, text2, text3)
      ## return both
      return(structure(doStop,
        message = text
      ))
    }
)


## ============================================================

## -----------------------------------------------------
## Determine the safety window length of the next cohort
## -----------------------------------------------------

##' Determine the safety window length of the next cohort
##'
##' This function determines the safety window length of
##' the next cohort.
##'
##' @param safetyWindow The rule, an object of class
##' \code{\linkS4class{SafetyWindow}}
##' @param size The next cohort size
##' @param data The data input, an object of class \code{\linkS4class{DataDA}}
##' @param \dots additional arguments
##'
##' @return the `windowLength` as a list of safety window parameters
##' (`gap`, `follow`, `follow_min`)
##'
##' @export
##' @keywords methods
setGeneric("windowLength",
  def =
    function(safetyWindow, size, ...) {
      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("windowLength")
    },
  valueClass = "list"
)


## ============================================================

## --------------------------------------------------
## The SafetyWindowSize method
## --------------------------------------------------

##' @describeIn windowLength Determine safety window length based
##' on the cohort size
##'
##' @example examples/Rules-method-windowLength-SafetyWindowSize.R
setMethod("windowLength",
  signature =
    signature(
      safetyWindow = "SafetyWindowSize",
      size = "ANY"
    ),
  def =
    function(safetyWindow, size, data, ...) {

      ## determine in which interval the next size is
      interval <-
        findInterval(
          x = size,
          vec = safetyWindow@size
        )

      ## so the safety window length is
      patientGap <- head(c(
        0, safetyWindow@gap[[interval]],
        rep(tail(safetyWindow@gap[[interval]], 1), 100)
      ), size)
      patientFollow <- safetyWindow@follow
      patientFollowMin <- safetyWindow@follow_min

      ret <- list(patientGap = patientGap, patientFollow = patientFollow, patientFollowMin = patientFollowMin)

      return(ret)
    }
)

## ============================================================

## --------------------------------------------------
## Constant safety window length
## --------------------------------------------------

##' @describeIn windowLength Constant safety window length
##' @example examples/Rules-method-windowLength-SafetyWindowConst.R
setMethod("windowLength",
  signature =
    signature(
      safetyWindow = "SafetyWindowConst",
      size = "ANY"
    ),
  def =
    function(safetyWindow, size, ...) {

      ## first element should be 0.
      patientGap <- head(c(
        0, safetyWindow@gap,
        rep(tail(safetyWindow@gap, 1), 100)
      ), size)
      patientFollow <- safetyWindow@follow
      patientFollowMin <- safetyWindow@follow_min

      ret <- list(
        patientGap = patientGap,
        patientFollow = patientFollow,
        patientFollowMin = patientFollowMin
      )

      return(ret)
    }
)

# nolint end
