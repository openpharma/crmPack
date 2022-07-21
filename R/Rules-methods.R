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
#'   empty vector, then no dose limit will be applied in the course of dose
#'   recommendation calculation, and a corresponding warning is given.
#' @param samples (`Samples`)\cr posterior samples from `model` parameters given
#'   `data`.
#' @param model (`Model`)\cr model that was used to generate the samples.
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
  def = function(nextBest,
                 doselimit,
                 samples,
                 model,
                 data,
                 ...) {
    if (!missing(doselimit) && length(doselimit) != 0) {
      assert_number(doselimit, lower = 0, finite = TRUE)
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
    model = "Model",
    data = "Data"
  ),
  definition = function(nextBest,
                        doselimit,
                        samples,
                        model,
                        data,
                        ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Which doses on grid are <= maximum possible dose.
    doses_eligible <- data@doseGrid[data@doseGrid <= doselimit]

    # Generate the MTD samples and derive the next best dose.
    mtd_samples <- dose(x = nextBest@target, model, samples)
    mtd_estimate <- nextBest@derive(mtd_samples = mtd_samples)
    # Round to the next possible grid point.
    min_dist_ind <- which.min(abs(doses_eligible - mtd_estimate))
    next_best <- doses_eligible[min_dist_ind]

    # Create a plot.
    p <- ggplot(
      data = data.frame(x = mtd_samples),
      aes(.data$x),
      fill = "grey50",
      colour = "grey50"
    ) +
      geom_density() +
      coord_cartesian(xlim = range(data@doseGrid)) +
      geom_vline(xintercept = mtd_estimate, colour = "black", lwd = 1.1) +
      geom_text(
        data = data.frame(x = mtd_estimate),
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
      geom_vline(xintercept = next_best, colour = "blue", lwd = 1.1) +
      geom_text(
        data = data.frame(x = next_best),
        aes(.data$x, 0),
        label = "Next",
        vjust = -0.5,
        hjust = -1.5,
        colour = "blue",
        angle = 90
      )

    list(value = next_best, plot = p)
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
    model = "Model",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)

    # Estimates of posterior probabilities that are based on the prob. samples
    # which are within overdose/target interval.
    prob_overdose <- colMeans(h_in_range(prob_samples, nextBest@overdose, bounds_closed = c(FALSE, TRUE)))
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- (data@doseGrid <= doselimit) & (prob_overdose < nextBest@max_overdose_prob)

    next_best <- if (any(is_dose_eligible)) {
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
          aes(x = next_best, y = prob_target[is_dose_eligible][next_best_level] * 100 + 0.03),
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
      value = next_best,
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
    model = "Model",
    data = "DataParts"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    # Exception when we are in part I or about to start part II!
    if (all(data@part == 1L)) {
      # Propose the highest possible dose (assuming that the dose limit came
      # from reasonable increments rule, i.e. inrementsRelativeParts).
      if (length(doselimit) == 0L) {
        stop("doselimit needs to be specified given for Part I")
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
    model = "Model",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)
    # Now compute probabilities to be in target and overdose tox interval.
    prob_underdosing <- colMeans(prob_samples < nextBest@target[1])
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))
    prob_overdose <- colMeans(h_in_range(prob_samples, nextBest@overdose, bounds_closed = c(FALSE, TRUE)))
    prob_mean <- colMeans(prob_samples)
    prob_sd <- apply(prob_samples, 2, stats::sd)

    is_unacceptable_specified <- any(nextBest@unacceptable != c(1, 1))

    prob_mat <- if (!is_unacceptable_specified) {
      cbind(
        underdosing = prob_underdosing, target = prob_target, overdose = prob_overdose
      )
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
    is_dose_eligible <- (data@doseGrid <= doselimit) & (prob_overdose < nextBest@max_overdose_prob)
    doses_eligible <- data@doseGrid[is_dose_eligible]
    # Next best dose is the dose with the minimum loss function.
    next_best <- if (any(is_dose_eligible)) {
      next_best_level <- which.min(posterior_loss[is_dose_eligible])
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
        geom_vline(
          xintercept = data@doseGrid[sum(is_dose_eligible)], lwd = 1.1,
          lty = 2, colour = "red"
        )
    }
    p_loss <- ggplot() +
      # For the loss function.
      geom_bar(
        data = data.frame(Dose = data@doseGrid, y = posterior_loss),
        aes(x = .data$Dose, y = .data$y),
        stat = "identity",
        position = "identity",
        width = min(diff(data@doseGrid)) / 2,
        colour = "darkgreen",
        fill = "darkgreen"
      ) +
      geom_point(
        aes(x = next_best, y = max(posterior_loss) + 0.2),
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
          data = data.frame(Dose = data@doseGrid, y = prob_overdose * 100),
          aes(x = .data$Dose, y = .data$y),
          stat = "identity",
          position = "identity",
          width = min(diff(data@doseGrid)) / 2,
          colour = "red",
          fill = "red"
        ) +
        geom_hline(
          yintercept = nextBest@max_overdose_prob * 100, lwd = 1.1, lty = 2,
          colour = "black"
        ) +
        ylim(c(0, 100)) +
        ylab("Overdose probability [%]")

      # Place them below each other.
      plot_joint <- gridExtra::arrangeGrob(p1, p2, p_loss, nrow = 3)
    } else {

      # Plot in case of 4 toxicity intervals. Second, for the overdosing probability.
      p2 <- ggplot() +
        geom_bar(
          data = data.frame(Dose = data@doseGrid, y = prob_excessive * 100),
          aes(x = .data$Dose, y = .data$y),
          stat = "identity",
          position = "identity",
          width = min(diff(data@doseGrid)) / 2,
          colour = "red",
          fill = "red"
        ) +
        ylim(c(0, 100)) +
        ylab("Excessive probability [%]")

      p3 <- ggplot() +
        geom_bar(
          data = data.frame(Dose = data@doseGrid, y = prob_unacceptable * 100),
          aes(x = .data$Dose, y = .data$y),
          stat = "identity",
          position = "identity",
          width = min(diff(data@doseGrid)) / 2,
          colour = "red",
          fill = "red"
        ) +
        ylim(c(0, 100)) +
        ylab("Unacceptable probability [%]")

      # Place them below each other.
      plot_joint <- gridExtra::arrangeGrob(p1, p2, p3, p_loss, nrow = 4)
    }
    if (!is_unacceptable_specified) {
      singlePlots <- list(plot1 = p1, plot2 = p2, plot_loss = p_loss)
    } else {
      singlePlots <- list(plot1 = p1, plot2 = p2, plot3 = p3, plot_loss = p_loss)
    }

    list(
      value = next_best,
      plot = plot_joint,
      singlePlots = singlePlots,
      probs = probs
    )
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
    nDLTs_last_level <- sum(data@y[data@xLevel == last_level])
    DLT_rate_last_level <- nDLTs_last_level / nPatients[last_level]

    level_change <- if (DLT_rate_last_level < 1 / 3) {
      # Escalate it, unless this is the highest level or the higher dose was already tried.
      ifelse((last_level == data@nGrid) || (nPatients[last_level + 1L] > 0), 0L, 1L)
    } else {
      # Rate is too high, deescalate it, unless an edge case of 1/3, where the decision
      # depends on the num. of patients: if >3, then deescalate it, otherwise stay.
      ifelse((DLT_rate_last_level == 1 / 3) && (nPatients[last_level] <= 3L), 0L, -1L)
    }
    next_level <- last_level + level_change

    # Do we stop here? Only if we have no MTD, or the next level has been tried
    # enough (more than three patients already).
    if (next_level == 0L) {
      next_best <- NA
      stop_here <- TRUE
    } else {
      next_best <- data@doseGrid[next_level]
      stop_here <- nPatients[next_level] > 3L
    }

    list(value = next_best, stopHere = stop_here)
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
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Biomarker samples at the dose grid points.
    biom_samples <- samples@data$betaW

    prob_target <- if (nextBest@target_relative) {
      # If 'Emax' parameter available, target biomarker level will be relative to 'Emax',
      # otherwise, it will be relative to the maximum biomarker level achieved
      # in dose range for a given sample.
      if ("Emax" %in% names(samples@data)) {
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
    is_dose_eligible <- (data@doseGrid <= doselimit) & (prob_overdose < nextBest@max_overdose_prob)
    # Exclude placebo (if any) from the recommended next doses.
    if (data@placebo & data@nObs >= 2L) {
      is_dose_eligible <- is_dose_eligible[-1]
    }

    next_best <- if (any(is_dose_eligible)) {
      # If maximum target probability is higher the threshold, then take that
      # level, otherwise stick to the maximum level that is eligible.
      # next_best_level is relative to eligible doses.
      next_best_level <- ifelse(
        test = any(prob_target[is_dose_eligible] > nextBest@target_thresh),
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
          data = data.frame(x = next_best, y = prob_target[is_dose_eligible][next_best_level] * 100 + 0.03),
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
      value = next_best,
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
    model = "Model",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    modelfit <- fit(samples, model, data)
    doses <- modelfit$dose
    is_dose_eligible <- doses <= doselimit
    doses_eligible <- doses[is_dose_eligible]
    dlt_prob <- modelfit$middle[is_dose_eligible]
    next_best_level <- which.min(abs(dlt_prob - nextBest@target))
    next_best <- doses_eligible[next_best_level]

    list(value = next_best)
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
    model = "Model",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(data@doseGrid, prob, model = model, samples = samples)

    criterion <- colMeans(h_info_theory_dist(prob_samples, nextBest@target, nextBest@asymmetry))

    is_dose_eligible <- data@doseGrid <= doselimit
    doses_eligible <- data@doseGrid[is_dose_eligible]
    next_best_level <- which.min(criterion[is_dose_eligible])
    next_best <- doses_eligible[next_best_level]
    list(value = next_best)
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
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # Generate TDtarget during a trial and TDtarget at the end of trial samples.
    target_in_trial_samples <- dose(x = nextBest@targetDuringTrial, model, samples)
    target_trial_end_samples <- dose(x = nextBest@targetEndOfTrial, model, samples)

    # Derive the prior/posterior estimates based on two above samples.
    target_in_trial_est <- as.numeric(nextBest@derive(TDsamples = target_in_trial_samples))
    target_trial_end_est <- as.numeric(nextBest@derive(TDsamples = target_trial_end_samples))

    # Get eligible doses that do not exceed maximum possible dose.
    is_dose_eligible <- data@doseGrid <= doselimit
    doses_eligible <- data@doseGrid[is_dose_eligible]

    # Find the next doses in the doseGrid. The next dose is the dose at level
    # closest and below the target_in_trial_est (or target_trial_end_est, respectively).
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    next_best_level <- h_find_interval(target_in_trial_est, doses_eligible)
    next_best <- doses_eligible[next_best_level]

    next_best_level1 <- h_find_interval(target_trial_end_est, doses_eligible)
    next_best1 <- doses_eligible[next_best_level1]

    # 95% credibility interval.
    ci_td_eot <- as.numeric(quantile(target_trial_end_samples, probs = c(0.025, 0.975)))
    td_eot_ratio <- ci_td_eot[2] / ci_td_eot[1]

    # Build plot.
    p <- h_next_best_tdsamples_plot(
      target_in_trial_samples = target_in_trial_samples,
      target_trial_end_samples = target_trial_end_samples,
      target_in_trial_est = target_in_trial_est,
      target_trial_end_est = target_trial_end_est,
      nextBest = nextBest,
      dose_grid_range = range(data@doseGrid),
      doselimit = doselimit,
      next_best_dose = next_best
    )

    list(
      nextdose = next_best,
      targetDuringTrial = nextBest@targetDuringTrial,
      TDtargetDuringTrialEstimate = target_in_trial_est,
      targetEndOfTrial = nextBest@targetEndOfTrial,
      TDtargetEndOfTrialEstimate = target_trial_end_est,
      TDtargetEndOfTrialAtDoseGrid = next_best1,
      CITDEOT = ci_td_eot,
      ratioTDEOT = td_eot_ratio,
      plot = p
    )
  }
)

## NextBestTD ----

#' @describeIn nextBest find the next best dose based only on the DLT responses
#'   and for [`LogisticIndepBeta`] model class object without DLT samples.
#'
#' @param SIM (`flag`)\cr is this method used in simulations? Default as `FALSE`.
#'   If this flag is `TRUE` and target dose estimates (during trial and end-of-trial)
#'   are outside of the dose gride range, the information message is printed by
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
  definition = function(nextBest, doselimit, model, data, SIM = FALSE, ...) {
    doselimit <- ifelse(missing(doselimit) || length(doselimit) == 0, Inf, doselimit)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@targetDuringTrial
    prob_target_eot <- nextBest@targetEndOfTrial

    # Target dose estimates, i.e. the dose with probability of the occurrence of
    # a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt <- dose(x = prob_target_drt, model)
    dose_target_eot <- dose(x = prob_target_eot, model)

    # Get eligible doses that do not exceed maximum possible dose.
    is_dose_eligible <- data@doseGrid <= doselimit
    doses_eligible <- data@doseGrid[is_dose_eligible]

    # Find the next best doses in the doseGrid. The next best dose is the dose
    # at level closest and below the target dose estimate.
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
    next_dose_drt <- doses_eligible[next_dose_lev_drt]

    next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
    next_dose_eot <- doses_eligible[next_dose_lev_eot]

    # Find the variance of the log of the dose_target_eot.
    M <- matrix(
      c(
        -1 / (model@phi2),
        -(log(prob_target_eot / (1 - prob_target_eot)) - model@phi1) / (model@phi2)^2
      ),
      nrow = 1
    )
    var_eta <- as.vector(M %*% model@Pcov %*% t(M))

    # 95% credibility interval.
    ci_td_eot <- exp(log(dose_target_eot) + c(-1, 1) * 1.96 * sqrt(var_eta))
    td_eot_ratio <- ci_td_eot[2] / ci_td_eot[1]

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

    dose_grid_range <- c(
      data@doseGrid[ifelse(data@placebo && data@nGrid >= 2, 2, 1)],
      data@doseGrid[data@nGrid]
    )
    if (!h_in_range(dose_target_drt, range = dose_grid_range, bounds_closed = TRUE) && !SIM) {
      print(paste("TD", prob_target_drt * 100, "=", dose_target_drt, "not within dose grid"))
    }
    if (!h_in_range(dose_target_eot, range = dose_grid_range, bounds_closed = TRUE) && !SIM) {
      print(paste("TD", prob_target_eot * 100, "=", dose_target_eot, "not within dose grid"))
    }

    list(
      nextdose = next_dose_drt,
      targetDuringTrial = prob_target_drt,
      TDtargetDuringTrialEstimate = dose_target_drt,
      TDtargetEndOfTrialatdoseGrid = next_dose_eot,
      targetEndOfTrial = prob_target_eot,
      TDtargetEndOfTrialEstimate = dose_target_eot,
      CITDEOT = ci_td_eot,
      ratioTDEOT = td_eot_ratio,
      plot = p
    )
  }
)

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

# nolint end

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
      increments@basisLevel == "last",
      tail(
        data@xLevel,
        1
      ),
      max(data@xLevel)
    )

    max_next_dose_level <- min(
      length(data@doseGrid),
      basis_dose_level + increments@maxLevels
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

# nolint start

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
              data@part1Ladder[lastDoseLevel + increments@dltStart]
            } else {
              ## otherwise
              if (increments@cleanStart > 0) {
                ## if we want to start part 2 higher than
                ## the last part 1 dose, use usual increments
                callNextMethod(increments, data, ...)
              } else {
                ## otherwise
                data@part1Ladder[lastDoseLevel + increments@cleanStart]
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
          vec = increments@DLTintervals
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
          vec = increments@DLTintervals
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
##' @example examples/Rules-method-maxDose-IncrementMin.R
setMethod("maxDose",
  signature =
    signature(
      increments = "IncrementMin",
      data = "Data"
    ),
  def =
    function(increments, data, ...) {

      ## apply the multiple increment rules
      individualResults <-
        sapply(increments@IncrementsList,
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
      e1@stopList <- c(
        e1@stopList,
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
      e2@stopList <- c(
        e1,
        e2@stopList
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
      e1@stopList <- c(
        e1@stopList,
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
      e2@stopList <- c(
        e1,
        e2@stopList
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
##' @param model The model input, an object of class \code{\linkS4class{Model}}
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
          lapply(stopping@stopList,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stopList,
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
          lapply(stopping@stopList,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stopList,
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
          lapply(stopping@stopList,
            stopTrial,
            dose = dose,
            model = model,
            data = data,
            ...
          )
        } else {
          lapply(stopping@stopList,
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
      model = "Model",
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
      model = "Model",
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

# nolint end

# stopTrial-StoppingMTDCV ----

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


# stopTrial-StoppingLowestDoseHSRBeta ----

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

# nolint start

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
      if (stopping@scale == "relative") {

        ## If there is an 'Emax' parameter, target biomarker level will
        ## be relative to 'Emax', otherwise will be relative to the
        ## maximum biomarker level achieved in the given dose range.
        if ("Emax" %in% names(samples@data)) {

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


## --------------------------------------------------
## Determine the size of the next cohort
## --------------------------------------------------

##' Determine the size of the next cohort
##'
##' This function determines the size of the next cohort.
##'
##' @param cohortSize The rule, an object of class
##' \code{\linkS4class{CohortSize}}
##' @param dose the next dose
##' @param data The data input, an object of class \code{\linkS4class{Data}}
##' @param \dots additional arguments
##'
##' @return the size as integer value
##'
##' @export
##' @keywords methods
setGeneric("size",
  def =
    function(cohortSize, dose, data, ...) {
      ## if the recommended next dose is NA,
      ## don't check and return 0
      if (is.na(dose)) {
        return(0L)
      }

      ## there should be no default method,
      ## therefore just forward to next method!
      standardGeneric("size")
    },
  valueClass = "integer"
)

## --------------------------------------------------
## The dose range method
## --------------------------------------------------

##' @describeIn size Determine the cohort size based on the range into which the
##' next dose falls into
##'
##' @example examples/Rules-method-size-CohortSizeRange.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeRange",
      dose = "ANY",
      data = "Data"
    ),
  def =
    function(cohortSize, dose, data, ...) {

      ## determine in which interval the next dose is
      interval <-
        findInterval(
          x = dose,
          vec = cohortSize@intervals
        )

      ## so the cohort size is
      ret <- cohortSize@cohortSize[interval]

      return(ret)
    }
)

## --------------------------------------------------
## The DLT range method
## --------------------------------------------------

##' @describeIn size Determine the cohort size based on the number of DLTs so
##' far
##'
##' @example examples/Rules-method-size-CohortSizeDLT.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeDLT",
      dose = "ANY",
      data = "Data"
    ),
  def =
    function(cohortSize, dose, data, ...) {

      ## determine how many DLTs have occurred so far
      dltHappened <- sum(data@y)

      ## determine in which interval this is
      interval <-
        findInterval(
          x = dltHappened,
          vec = cohortSize@DLTintervals
        )

      ## so the cohort size is
      ret <- cohortSize@cohortSize[interval]

      return(ret)
    }
)

## --------------------------------------------------
## Size based on maximum of multiple cohort size rules
## --------------------------------------------------

##' @describeIn size Size based on maximum of multiple cohort size rules
##' @example examples/Rules-method-size-CohortSizeMax.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeMax",
      dose = "ANY",
      data = "Data"
    ),
  def =
    function(cohortSize, dose, data, ...) {
      ## evaluate the individual cohort size rules
      ## in the list
      individualResults <-
        sapply(cohortSize@cohortSizeList,
          size,
          dose = dose,
          data = data,
          ...
        )

      ## summarize to obtain overall result
      overallResult <- max(individualResults)

      return(overallResult)
    }
)

## --------------------------------------------------
## Size based on minimum of multiple cohort size rules
## --------------------------------------------------

##' @describeIn size Size based on minimum of multiple cohort size rules
##' @example examples/Rules-method-size-CohortSizeMin.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeMin",
      dose = "ANY",
      data = "Data"
    ),
  def =
    function(cohortSize, dose, data, ...) {
      ## evaluate the individual cohort size rules
      ## in the list
      individualResults <-
        sapply(cohortSize@cohortSizeList,
          size,
          dose = dose,
          data = data,
          ...
        )

      ## summarize to obtain overall result
      overallResult <- min(individualResults)

      return(overallResult)
    }
)

## --------------------------------------------------
## Constant cohort size
## --------------------------------------------------

##' @describeIn size Constant cohort size
##' @example examples/Rules-method-size-CohortSizeConst.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeConst",
      dose = "ANY",
      data = "Data"
    ),
  def =
    function(cohortSize, dose, data, ...) {
      return(cohortSize@size)
    }
)


## --------------------------------------------------
## Cohort size based on the parts
## --------------------------------------------------

##' @describeIn size Cohort size based on the parts
##' @example examples/Rules-method-size-CohortSizeParts.R
setMethod("size",
  signature =
    signature(
      cohortSize = "CohortSizeParts",
      dose = "ANY",
      data = "DataParts"
    ),
  def =
    function(cohortSize, dose, data, ...) {
      return(cohortSize@sizes[data@nextPart])
    }
)

## ------------------------------------------------------------------------------------
## the nextBest method based on DLE and efficacy responses without DLE and efficacy samples
## -------------------------------------------------------------------------- ----------
##' @describeIn nextBest for slots \code{nextBest},\code{doselimit}, \code{data} and \code{SIM}. This is
##' a function to find the next best dose based on the 'NextBestMaxGain'
##' class rule. This a method based on the DLE responses and efficacy responses without DLE and
##' efficacy samples.
##'
##' @param Effmodel the efficacy model of \code{\linkS4class{ModelEff}} class object
##'
##' @importFrom ggplot2 ggplot xlab ylab xlim aes geom_vline
##' geom_text
##'
##' @example examples/Rules-method-nextbest_MaxGain.R
##'
##' @importFrom ggplot2 scale_colour_manual
##' @export
##' @keywords methods
setMethod("nextBest",
  signature =
    signature(
      nextBest = "NextBestMaxGain",
      doselimit = "numeric",
      samples = "missing",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(nextBest, doselimit, model, data, Effmodel, SIM = FALSE, ...) {
      stopifnot(is(Effmodel, "ModelEff"))

      DuringTrialtargetprob <- nextBest@DLEDuringTrialtarget
      EndOfTrialtargetprob <- nextBest@DLEEndOfTrialtarget

      ## Find the TDtarget Estimate for During Trial and End of trial


      TDtargetEndOfTrialEstimate <- dose(x = EndOfTrialtargetprob, model = model)


      TDtargetDuringTrialEstimate <- dose(x = DuringTrialtargetprob, model = model)

      ## Get all prob of DLE at all dose levels
      probDLE <- prob(
        dose = data@doseGrid,
        model = model
      )

      ## Define gain function
      Gainfun <- function(DOSE) {
        -gain(DOSE, model_dle = model, model_eff = Effmodel)
      }

      # if(data@placebo) {
      # n <- length(data@doseGrid)
      # LowestDose <- sort(data@doseGrid)[2]} else {
      LowestDose <- min(data@doseGrid)
      # }
      ## Find the dose which gives the maximum gain
      Gstar <- (optim(LowestDose, Gainfun, method = "L-BFGS-B", lower = LowestDose, upper = max(data@doseGrid))$par)
      ## Find the maximum gain value

      MaxGain <- -(optim(LowestDose, Gainfun, method = "L-BFGS-B", lower = LowestDose, upper = max(data@doseGrid))$value)
      ## be sure which doses are ok with respect to maximum
      ## possible dose

      dosesOK <- which(data@doseGrid <= doselimit)

      ## For placebo design, if safety allow, exclude placebo from
      ## the recommended next doses
      if (data@placebo & (length(dosesOK) > 1L)) {
        dosesOK <- dosesOK[-1]
      }

      ## FIND the next dose which is the minimum between TDtargetDuringTrial and Gstar
      nextdose <- min(TDtargetDuringTrialEstimate, Gstar)

      ## Find the dose level in doseGrid closest below nextdose

      index <- suppressWarnings(max(which((signif(nextdose, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


      ret <- data@doseGrid[dosesOK][index]

      ## Find the dose level in doseGrid closest below TDtargetEndOfTrial

      indexE <- suppressWarnings(max(which((signif(TDtargetEndOfTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


      retE <- data@doseGrid[indexE]

      ## Find the dose level in doseGrid closest below TDtargetDuringTrial

      indexD <- suppressWarnings(max(which((signif(TDtargetDuringTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


      retD <- data@doseGrid[indexD]

      ## Find the dose level in doseGrid closest below Gstar

      Gstarindex <- suppressWarnings(max(which((signif(Gstar, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


      Gstarret <- data@doseGrid[Gstarindex]

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

      deltaG <- matrix(c(dgphi1, dgphi2, dgtheta1, dgtheta2), 4, 1)


      ## Find the variance of the log Gstar
      ## First find the covariance matrix of all the parameters, phi1, phi2, theta1 and theta2
      ## such that phi1 and phi2 and independent of theta1 and theta2
      emptyMatrix <- matrix(0, 2, 2)
      covBETA <- cbind(rbind(model@Pcov, emptyMatrix), rbind(emptyMatrix, Effmodel@Pcov))
      varlogGstar <- t(deltaG) %*% covBETA %*% deltaG



      ## Find the upper and lower limit of the 95% credibility interval of Gstar
      CIGstar <- c()
      CIGstar[2] <- exp(logGstar + 1.96 * sqrt(varlogGstar))
      CIGstar[1] <- exp(logGstar - 1.96 * sqrt(varlogGstar))

      ## The ratio of the upper to the lower 95% credibility interval
      ratioGstar <- as.numeric(CIGstar[2] / CIGstar[1])



      ## Find the variance of the log of the TDtargetEndOfTrial(eta)
      M1 <- matrix(c(-1 / (model@phi2), -(log(EndOfTrialtargetprob / (1 - EndOfTrialtargetprob)) - model@phi1) / (model@phi2)^2), 1, 2)
      M2 <- model@Pcov

      varEta <- M1 %*% M2 %*% t(M1)

      ## Find the upper and lower limit of the 95% credibility interval of
      ## TDtargetEndOfTrial
      CITDEOT <- c()
      CITDEOT[2] <- exp(log(TDtargetEndOfTrialEstimate) + 1.96 * sqrt(varEta))
      CITDEOT[1] <- exp(log(TDtargetEndOfTrialEstimate) - 1.96 * sqrt(varEta))

      ## The ratio of the upper to the lower 95% credibility interval
      ratioTDEOT <- as.numeric(CITDEOT[2] / CITDEOT[1])

      plotData <- data.frame(
        dose = rep(data@doseGrid, 3),
        values = c(
          prob(
            dose = data@doseGrid,
            model = model
          ),
          efficacy(
            dose = data@doseGrid,
            model = Effmodel
          ),
          gain(
            dose = data@doseGrid,
            model_dle = model,
            model_eff = Effmodel
          )
        )
      )
      gdata <- with(
        plotData,
        data.frame(
          x = dose,
          y = values,
          group = c(
            rep("p(DLE)", length(data@doseGrid)),
            rep("Expected Efficacy", length(data@doseGrid)),
            rep("Gain", length(data@doseGrid))
          ),
          Type = factor("Estimate", levels = "Estimate")
        )
      )

      plot1 <- ggplot(data = gdata, aes(x = x, y = y)) +
        geom_line(aes(group = group, color = group), size = 1.5) +
        ggplot2::scale_colour_manual(name = "curves", values = c("blue", "green3", "red")) +
        xlab("Dose Level") +
        xlim(c(0, max(data@doseGrid))) +
        ylab(paste("Values")) +
        ylim(c(min(gdata$y), max(gdata$y)))



      if ((signif(TDtargetEndOfTrialEstimate, 4) < LowestDose) | (signif(TDtargetEndOfTrialEstimate, 4) > max(data@doseGrid))) {
        if (SIM == FALSE) {
          plot1 <- plot1
          print(paste(paste("Estimated TD", EndOfTrialtargetprob * 100), paste("=", paste(TDtargetEndOfTrialEstimate, " not within dose Grid"))))
        } else {
          plot1 <- plot1
        }
      } else {
        plot1 <- plot1 + geom_point(data = data.frame(x = TDtargetEndOfTrialEstimate, y = EndOfTrialtargetprob), aes(x = x, y = y), colour = "violet", shape = 16, size = 8) +
          annotate("text", label = paste(paste("TD", EndOfTrialtargetprob * 100), "Estimate"), x = TDtargetEndOfTrialEstimate - 3, y = 0.2, size = 5, colour = "violet")
      }



      if ((signif(Gstar, 4) < LowestDose) | (signif(Gstar, 4) > max(data@doseGrid))) {
        if (SIM == FALSE) {
          plot1 <- plot1
          print(paste("Estimated Gstar=", paste(Gstar, " not within dose Grid")))
        } else {
          plot1 <- plot1
        }
      } else {
        plot1 <- plot1 +
          geom_point(data = data.frame(x = Gstar, y = MaxGain), aes(x = x, y = y), colour = "green3", shape = 17, size = 8) +
          annotate("text", label = "Max Gain Estimate", x = Gstar, y = MaxGain - 0.1, size = 5, colour = "green3")
      }

      mylabel <- format(DuringTrialtargetprob, digits = 2)


      if ((signif(TDtargetDuringTrialEstimate, 4) < LowestDose) | (signif(TDtargetDuringTrialEstimate, 4) > max(data@doseGrid))) {
        if (SIM == FALSE) {
          plot1 <- plot1
          print(paste(paste("Estimated TD", DuringTrialtargetprob * 100), paste("=", paste(TDtargetDuringTrialEstimate, " not within dose Grid"))))
        } else {
          plot1 <- plot1
        }
      } else {
        plot1 <- plot1 +
          geom_point(data = data.frame(x = signif(TDtargetDuringTrialEstimate, 4), y = DuringTrialtargetprob), aes(x = x, y = y), colour = "orange", shape = 15, size = 8) +
          annotate("text",
            label = paste(paste("TD", DuringTrialtargetprob * 100), "Estimate"), x = TDtargetDuringTrialEstimate + 25,
            y = DuringTrialtargetprob + 0.01, size = 5, colour = "orange"
          )
      }


      if (doselimit > max(data@doseGrid)) {
        maxdoselimit <- max(data@doseGrid)
      } else {
        maxdoselimit <- doselimit
      }

      plot1 <- plot1 +
        geom_vline(xintercept = maxdoselimit, colour = "brown", lwd = 1.1) +
        annotate("text", label = "Max", x = maxdoselimit - 2, y = max(gdata$y), size = 5, colour = "brown")


      plot1 <- plot1 +
        geom_vline(xintercept = ret, colour = "purple", lwd = 1.1) +
        annotate("text", label = "Next", x = ret + 1, y = max(gdata$y) - 0.05, size = 5, color = "purple")


      ## return next best dose and plot
      return(list(
        nextdose = ret,
        DLEDuringTrialtarget = DuringTrialtargetprob,
        TDtargetDuringTrialEstimate = TDtargetDuringTrialEstimate,
        TDtargetDuringTrialAtDoseGrid = retD,
        DLEEndOfTrialtarget = EndOfTrialtargetprob,
        TDtargetEndOfTrialEstimate = TDtargetEndOfTrialEstimate,
        TDtargetEndOfTrialAtDoseGrid = retE,
        GstarEstimate = Gstar,
        GstarAtDoseGrid = Gstarret,
        CITDEOT = CITDEOT,
        ratioTDEOT = ratioTDEOT,
        CIGstar = CIGstar,
        ratioGstar = ratioGstar,
        plot = plot1
      ))
    }
)
## =====================================================================================
## the nextBest method based on DLE and efficacy responses with DLE and efficacy samples
## -------------------------------------------------------------------------- ----------
##' @describeIn nextBest for slots \code{nextBest},\code{doselimit}, \code{data} and \code{SIM}. This is
##' a function to find the next best dose based on the 'NextBestMaxGainSamples'
##' class rule. This a method based on the DLE responses and efficacy responses with DLE and
##' efficacy samples. Effmodel must be of class \code{\linkS4class{Effloglog}} or
##' \code{\linkS4class{EffFlexi}}.
##' @param Effsamples the efficacy samples of \code{\linkS4class{Samples}} class object
##'
##' @importFrom ggplot2 ggplot geom_histogram xlab ylab xlim aes geom_vline
##' geom_text
##' @example examples/Rules-method-nextbest_MaxGainSamples.R
setMethod("nextBest",
  signature =
    signature(
      nextBest = "NextBestMaxGainSamples",
      doselimit = "numeric",
      samples = "Samples",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(nextBest, doselimit, samples, model, data, Effmodel, Effsamples, SIM = FALSE, ...) {
      if (is(Effmodel, "Effloglog")) {

        ## first get the probDLE samples
        points <- data@doseGrid
        probDLESamples <- matrix(
          nrow = sampleSize(samples@options),
          ncol = length(points)
        )

        ## evaluate the probs, for all gain samples.
        for (i in seq_along(points))
        {
          ## Now we want to evaluate for the
          ## following dose:
          probDLESamples[, i] <- prob(
            dose = points[i],
            model = model,
            samples = samples
          )
        }
        probDLE <- apply(probDLESamples, 2, FUN = nextBest@TDderive)

        DuringTrialtargetprob <- nextBest@DLEDuringTrialtarget
        EndOfTrialtargetprob <- nextBest@DLEEndOfTrialtarget

        ## Find the TDtarget samples for During Trial and End of trial


        TDtargetEndOfTrialSamples <- dose(
          x = EndOfTrialtargetprob,
          model = model,
          samples = samples
        )


        TDtargetDuringTrialSamples <- dose(
          x = DuringTrialtargetprob,
          model = model,
          samples = samples
        )



        ## Find the TDtarget Estimate for During Trial and End of trial


        TDtargetEndOfTrialEstimate <- as.numeric(nextBest@TDderive(TDtargetEndOfTrialSamples))
        ## Ensure the estimate is within dose range
        # TDtargetEndOfTrialEstimate <- min(TDtargetEndOfTrialEstimate,max(data@doseGrid))


        TDtargetDuringTrialEstimate <- as.numeric(nextBest@TDderive(TDtargetDuringTrialSamples))

        ## Ensure the estimate is within dose range
        # TDtargetDuringTrialEstimate <- min(TDtargetDuringTrialEstimate,max(data@doseGrid))


        ## we have to get samples from the gain values at all dose levels

        ExpEffSamples <- matrix(
          nrow = sampleSize(Effsamples@options),
          ncol = length(points)
        )

        ## evaluate the probs, for all gain samples.
        for (i in seq_along(points))
        {
          ## Now we want to evaluate for the
          ## following dose:
          ExpEffSamples[, i] <- efficacy(
            dose = points[i],
            model = Effmodel,
            samples = Effsamples
          )
        }


        ExpEff <- apply(ExpEffSamples, 2, FUN = nextBest@Gstarderive)


        GainSamples <- matrix(
          nrow = sampleSize(samples@options),
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

        ## Obtain the Gstar estimate which is the nth percentile of the Gstar samples
        Gstar <- as.numeric(nextBest@Gstarderive(GstarSamples))
        ## Ensure the estimate is within dose range

        # Gstar <- min(Gstar,max(data@doseGrid))

        gainvalues <- apply(GainSamples, 2, FUN = nextBest@Gstarderive)

        dosesOK <- which(data@doseGrid <= doselimit)

        ## For placebo design, if safety allow, exclude placebo from
        ## the recommended next doses
        if (data@placebo & (length(dosesOK) > 1L)) {
          dosesOK <- dosesOK[-1]
        }

        ## FIND the next dose which is the minimum between TDtargetDuringTrial and Gstar
        nextdose <- min(TDtargetDuringTrialEstimate, Gstar)

        ## Find the dose level in doseGrid closest below nextdose

        index <- suppressWarnings(max(which((signif(nextdose, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        ret <- data@doseGrid[dosesOK][index]

        ## Find the dose level in doseGrid closest below TDtargetEndOfTrial

        indexE <- suppressWarnings(max(which((signif(TDtargetEndOfTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        retE <- data@doseGrid[indexE]

        ## Find the dose level in doseGrid closest below TDtargetDuringTrial

        indexD <- suppressWarnings(max(which((signif(TDtargetDuringTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        retD <- data@doseGrid[indexD]

        ## Find the dose level in doseGrid closest below Gstar

        Gstarindex <- suppressWarnings(max(which((signif(Gstar, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        Gstarret <- data@doseGrid[Gstarindex]

        ## Find the 95% credibility interval of Gstar and its ratio of the upper to the lower limit
        CIGstar <- as.numeric(quantile(GstarSamples, probs = c(0.025, 0.975)))
        ratioGstar <- as.numeric(CIGstar[2] / CIGstar[1])

        ## Find the 95% credibility interval of TDtargetEndOfTrial and its ratio of the upper to the lower limit
        CITDEOT <- as.numeric(quantile(TDtargetEndOfTrialSamples, probs = c(0.025, 0.975)))
        ratioTDEOT <- as.numeric(CITDEOT[2] / CITDEOT[1])


        plotData <- data.frame(
          dose = rep(data@doseGrid, 3),
          values = c(
            probDLE,
            ExpEff,
            gainvalues
          )
        )

        gdata <- with(
          plotData,
          data.frame(
            x = dose,
            y = values,
            group = c(
              rep("p(DLE)", length(data@doseGrid)),
              rep("Expected Efficacy", length(data@doseGrid)),
              rep("Gain", length(data@doseGrid))
            ),
            Type = factor("Estimate", levels = "Estimate")
          )
        )


        ## produce plot
        plot1 <- ggplot() +
          geom_histogram(
            data =
              data.frame(x = GstarSamples),
            aes(x = x),
            fill = "darkgreen", colour = "green3", binwidth = 25
          ) +
          xlab("Gstar") +
          xlim(c(0, max(data@doseGrid))) +
          ylab("Posterior density")

        # if(data@placebo) {
        # n <- length(data@doseGrid)
        # LowestDose <- sort(data@doseGrid)[2]} else {
        LowestDose <- min(data@doseGrid)
        # }


        if (signif(TDtargetDuringTrialEstimate, 4) < LowestDose | signif(TDtargetDuringTrialEstimate, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste(paste("Estimated TD", DuringTrialtargetprob * 100), paste("=", paste(TDtargetDuringTrialEstimate, " not within dose Grid"))))
          } else {
            plot1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = TDtargetDuringTrialEstimate, colour = "orange", lwd = 1.1) +
            annotate("text",
              label = paste(paste("TD", DuringTrialtargetprob * 100), "Estimate"),
              x = TDtargetDuringTrialEstimate, y = 0, hjust = -0.1, vjust = -20, size = 5, colour = "orange"
            )
        }

        if (signif(TDtargetEndOfTrialEstimate, 4) < LowestDose | signif(TDtargetEndOfTrialEstimate, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste(paste("Estimated TD", EndOfTrialtargetprob * 100), paste("=", paste(TDtargetEndOfTrialEstimate, " not within dose Grid"))))
          } else {
            plot1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = TDtargetEndOfTrialEstimate, colour = "violet", lwd = 1.1) +
            annotate("text",
              label = paste(paste("TD", EndOfTrialtargetprob * 100), "Estimate"),
              x = TDtargetEndOfTrialEstimate, y = 0, hjust = -0.1, vjust = -25, size = 5, colour = "violet"
            )
        }

        if (signif(Gstar, 4) < LowestDose | signif(Gstar, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste("Estimated Gstar=", paste(Gstar, " not within dose Grid")))
          } else {
            plot1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = Gstar, colour = "green", lwd = 1.1) +
            annotate("text",
              label = " Gstar Estimate",
              x = Gstar, y = 0, hjust = -0.1, vjust = -25, size = 5, colour = "green"
            )
        }


        if (doselimit > max(data@doseGrid)) {
          maxdoselimit <- max(data@doseGrid)
        } else {
          maxdoselimit <- doselimit
        }

        plot1 <- plot1 +
          geom_vline(xintercept = maxdoselimit, colour = "red", lwd = 1.1) +
          geom_text(
            data =
              data.frame(x = maxdoselimit),
            aes(x, 0,
              label = "Max", hjust = +1, vjust = -35
            ),
            colour = "red"
          )

        plot1 <- plot1 +
          geom_vline(xintercept = ret, colour = "blue", lwd = 1.1) +
          geom_text(
            data =
              data.frame(x = ret),
            aes(x, 0,
              label = "Next", hjust = 0.1, vjust = -30
            ),
            colour = "blue"
          )



        ## return next best dose and plot
        return(list(
          nextdose = ret,
          DLEDuringTrialtarget = DuringTrialtargetprob,
          TDtargetDuringTrialEstimate = TDtargetDuringTrialEstimate,
          TDtargetDuringTrialAtDoseGrid = retD,
          DLEEndOfTrialtarget = EndOfTrialtargetprob,
          TDtargetEndOfTrialEstimate = TDtargetEndOfTrialEstimate,
          TDtargetEndOfTrialAtDoseGrid = retE,
          GstarEstimate = Gstar,
          GstarAtDoseGrid = Gstarret,
          CITDEOT = CITDEOT,
          ratioTDEOT = ratioTDEOT,
          CIGstar = CIGstar,
          ratioGstar = ratioGstar,
          plot = plot1
        ))
      } else if (is(Effmodel, "EffFlexi")) {

        ## first get the probDLE samples
        points <- data@doseGrid
        probDLESamples <- matrix(
          nrow = sampleSize(samples@options),
          ncol = length(points)
        )

        ## evaluate the probs, for all gain samples.
        for (i in seq_along(points))
        {
          ## Now we want to evaluate for the
          ## following dose:
          probDLESamples[, i] <- prob(
            dose = points[i],
            model = model,
            samples = samples
          )
        }
        probDLE <- apply(probDLESamples, 2, FUN = nextBest@TDderive)

        DuringTrialtargetprob <- nextBest@DLEDuringTrialtarget
        EndOfTrialtargetprob <- nextBest@DLEEndOfTrialtarget

        ## Find the TDtarget samples for During Trial and End of trial


        TDtargetEndOfTrialSamples <- dose(
          x = EndOfTrialtargetprob,
          model = model,
          samples = samples
        )


        TDtargetDuringTrialSamples <- dose(
          x = DuringTrialtargetprob,
          model = model,
          samples = samples
        )



        ## Find the TDtarget Estimate for During Trial and End of trial


        TDtargetEndOfTrialEstimate <- as.numeric(nextBest@TDderive(TDtargetEndOfTrialSamples))
        ## Ensure the estimate is within dose range
        # TDtargetEndOfTrialEstimate <- min(TDtargetEndOfTrialEstimate,max(data@doseGrid))


        TDtargetDuringTrialEstimate <- as.numeric(nextBest@TDderive(TDtargetDuringTrialSamples))

        ## Ensure the estimate is within dose range
        # TDtargetDuringTrialEstimate <- min(TDtargetDuringTrialEstimate,max(data@doseGrid))


        ## we have to get samples from the gain values at all dose levels

        ExpEffsamples <- Effsamples@data$ExpEff

        ExpEff <- apply(ExpEffsamples, 2, FUN = nextBest@Gstarderive)


        GainSamples <- matrix(
          nrow = sampleSize(samples@options),
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

        ## Obtain the Gstar estimate which is the 50th percentile of the Gstar samples
        Gstar <- as.numeric(nextBest@Gstarderive(GstarSamples))
        ## Ensure the estimate is within dose range

        # Gstar <- min(Gstar,max(data@doseGrid))

        gainvalues <- apply(GainSamples, 2, FUN = nextBest@Gstarderive)

        dosesOK <- which(data@doseGrid <= doselimit)

        ## For placebo design, if safety allow, exclude placebo from
        ## the recommended next doses
        if (data@placebo & (length(dosesOK) > 1L)) {
          dosesOK <- dosesOK[-1]
        }

        ## FIND the next dose which is the minimum between TDtargetDuringTrial and Gstar
        nextdose <- min(TDtargetDuringTrialEstimate, Gstar)

        ## Find the dose level in doseGrid closest below nextdose

        index <- suppressWarnings(max(which((signif(nextdose, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        ret <- data@doseGrid[dosesOK][index]

        ## Find the dose level in doseGrid closest below TDtargetEndOfTrial

        indexE <- suppressWarnings(max(which((signif(TDtargetEndOfTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        retE <- data@doseGrid[indexE]

        ## Find the dose level in doseGrid closest below TDtargetDuringTrial

        indexD <- suppressWarnings(max(which((signif(TDtargetDuringTrialEstimate, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        retD <- data@doseGrid[indexD]

        ## Find the dose level in doseGrid closest below Gstar

        Gstarindex <- suppressWarnings(max(which((signif(Gstar, digits = 4) - data@doseGrid[dosesOK]) >= 0)))


        Gstarret <- data@doseGrid[Gstarindex]

        ## Find the 95% credibility interval of Gstar and its ratio of the upper to the lower limit
        CIGstar <- as.numeric(quantile(GstarSamples, probs = c(0.025, 0.975)))
        ratioGstar <- as.numeric(CIGstar[2] / CIGstar[1])

        ## Find the 95% credibility interval of TDtargetEndOfTrial and its ratio of the upper to the lower limit
        CITDEOT <- as.numeric(quantile(TDtargetEndOfTrialSamples, probs = c(0.025, 0.975)))
        ratioTDEOT <- as.numeric(CITDEOT[2] / CITDEOT[1])


        plotData <- data.frame(
          dose = rep(data@doseGrid, 3),
          values = c(
            probDLE,
            ExpEff,
            gainvalues
          )
        )

        gdata <- with(
          plotData,
          data.frame(
            x = dose,
            y = values,
            group = c(
              rep("p(DLE)", length(data@doseGrid)),
              rep("Expected Efficacy", length(data@doseGrid)),
              rep("Gain", length(data@doseGrid))
            ),
            Type = factor("Estimate", levels = "Estimate")
          )
        )


        ## produce plot
        plot1 <- ggplot() +
          geom_histogram(
            data =
              data.frame(x = GstarSamples),
            aes(x = x),
            fill = "darkgreen", colour = "green3", binwidth = 25
          ) +
          xlab("Gstar") +
          xlim(c(0, max(data@doseGrid))) +
          ylab("Posterior density")

        # if(data@placebo) {
        # n <- length(data@doseGrid)
        # LowestDose <- sort(data@doseGrid)[2]} else {
        LowestDose <- min(data@doseGrid)
        # }


        if (signif(TDtargetDuringTrialEstimate, 4) < LowestDose | signif(TDtargetDuringTrialEstimate, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste(paste("Estimated TD", DuringTrialtargetprob * 100), paste("=", paste(TDtargetDuringTrialEstimate, " not within dose Grid"))))
          } else {
            plo1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = TDtargetDuringTrialEstimate, colour = "orange", lwd = 1.1) +
            annotate("text",
              label = paste(paste("TD", DuringTrialtargetprob * 100), "Estimate"),
              x = TDtargetDuringTrialEstimate, y = 0, hjust = -0.1, vjust = -20, size = 5, colour = "orange"
            )
        }

        if (signif(TDtargetEndOfTrialEstimate, 4) < LowestDose | signif(TDtargetEndOfTrialEstimate, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste(paste("Estimated TD", EndOfTrialtargetprob * 100), paste("=", paste(TDtargetEndOfTrialEstimate, " not within dose Grid"))))
          } else {
            plot1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = TDtargetEndOfTrialEstimate, colour = "violet", lwd = 1.1) +
            annotate("text",
              label = paste(paste("TD", EndOfTrialtargetprob * 100), "Estimate"),
              x = TDtargetEndOfTrialEstimate, y = 0, hjust = -0.1, vjust = -25, size = 5, colour = "violet"
            )
        }

        if (signif(Gstar, 4) < LowestDose | signif(Gstar, 4) > max(data@doseGrid)) {
          if (SIM == FALSE) {
            plot1 <- plot1
            print(paste("Estimated Gstar=", paste(Gstar, " not within dose Grid")))
          } else {
            plot1 <- plot1
          }
        } else {
          plot1 <- plot1 +
            geom_vline(xintercept = Gstar, colour = "green", lwd = 1.1) +
            annotate("text",
              label = " Gstar Estimate",
              x = Gstar, y = 0, hjust = +0.6, vjust = -25, size = 5, colour = "green"
            )
        }


        if (doselimit > max(data@doseGrid)) {
          maxdoselimit <- max(data@doseGrid)
        } else {
          maxdoselimit <- doselimit
        }

        plot1 <- plot1 +
          geom_vline(xintercept = maxdoselimit, colour = "red", lwd = 1.1) +
          geom_text(
            data =
              data.frame(x = maxdoselimit),
            aes(x, 0,
              label = "Max", hjust = +1, vjust = -35
            ),
            colour = "red"
          )

        plot1 <- plot1 +
          geom_vline(xintercept = ret, colour = "blue", lwd = 1.1) +
          geom_text(
            data =
              data.frame(x = ret),
            aes(x, 0,
              label = "Next", hjust = 0.1, vjust = -30
            ),
            colour = "blue"
          )



        ## return next best dose and plot
        return(list(
          nextdose = ret,
          DLEDuringTrialtarget = DuringTrialtargetprob,
          TDtargetDuringTrialEstimate = TDtargetDuringTrialEstimate,
          TDtargetDuringTrialAtDoseGrid = retD,
          DLEEndOfTrialtarget = EndOfTrialtargetprob,
          TDtargetEndOfTrialEstimate = TDtargetEndOfTrialEstimate,
          TDtargetEndOfTrialAtDoseGrid = retE,
          GstarEstimate = Gstar,
          GstarAtDoseGrid = Gstarret,
          CITDEOT = CITDEOT,
          ratioTDEOT = ratioTDEOT,
          CIGstar = CIGstar,
          ratioGstar = ratioGstar,
          plot = plot1
        ))
      } else {
        stop("Effmodel needs to be of class Effloglog or EffFlexi")
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
setMethod("stopTrial",
  signature =
    signature(
      stopping = "StoppingTDCIRatio",
      dose = "ANY",
      samples = "Samples",
      model = "ModelTox",
      data = "ANY"
    ),
  def =
    function(stopping, dose, samples, model, data, ...) {
      targetEndOfTrial <- stopping@targetEndOfTrial
      ## check id targetEndOfTrial is a probability
      stopifnot(is.probability(targetEndOfTrial))

      ## find the TDtarget End of Trial samples
      TDtargetEndOfTrialSamples <- dose(
        x = targetEndOfTrial,
        model = model,
        samples = samples
      )

      ## Find the upper and lower limit of the 95% credibility interval
      CI <- quantile(TDtargetEndOfTrialSamples, probs = c(0.025, 0.975))

      ## The ratio of the upper to the lower 95% credibility interval
      ratio <- as.numeric(CI[2] / CI[1])


      ## so can we stop?
      doStop <- ratio <= stopping@targetRatio
      ## generate messgae
      text <- paste(
        "95% CI is (", round(CI[1], 4), ",", round(CI[2], 4), "), Ratio =", round(ratio, 4), "is ", ifelse(doStop, "is less than or equal to", "greater than"),
        "targetRatio =", stopping@targetRatio
      )
      ## return both
      return(structure(doStop,
        messgae = text
      ))
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
      targetEndOfTrial <- stopping@targetEndOfTrial

      ## check if targetEndOfTrial is a probability
      stopifnot(is.probability(targetEndOfTrial))

      ## find the TDtarget End of Trial
      TDtargetEndOfTrial <- dose(
        x = targetEndOfTrial,
        model = model
      )

      ## Find the variance of the log of the TDtargetEndOfTrial(eta)
      M1 <- matrix(c(-1 / (model@phi2), -(log(targetEndOfTrial / (1 - targetEndOfTrial)) - model@phi1) / (model@phi2)^2), 1, 2)
      M2 <- model@Pcov

      varEta <- M1 %*% M2 %*% t(M1)

      ## Find the upper and lower limit of the 95% credibility interval
      CI <- c()
      CI[2] <- exp(log(TDtargetEndOfTrial) + 1.96 * sqrt(varEta))
      CI[1] <- exp(log(TDtargetEndOfTrial) - 1.96 * sqrt(varEta))

      ## The ratio of the upper to the lower 95% credibility interval
      ratio <- as.numeric(CI[2] / CI[1])


      ## so can we stop?
      doStop <- ratio <= stopping@targetRatio
      ## generate messgae
      text <- paste(
        "95% CI is (", round(CI[1], 4), ",", round(CI[2], 4), "), Ratio =", round(ratio, 4), "is ", ifelse(doStop, "is less than or equal to", "greater than"),
        "targetRatio =", stopping@targetRatio
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
      stopping = "StoppingGstarCIRatio",
      dose = "ANY",
      samples = "Samples",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(stopping, dose, samples, model, data, TDderive, Effmodel, Effsamples, Gstarderive, ...) {
      targetEndOfTrial <- stopping@targetEndOfTrial

      ## checks
      stopifnot(is.probability(targetEndOfTrial))
      stopifnot(is(Effmodel, "ModelEff"))
      stopifnot(is(Effsamples, "Samples"))
      stopifnot(is.function(TDderive))
      stopifnot(is.function(Gstarderive))

      ## find the TDtarget End of Trial samples
      TDtargetEndOfTrialSamples <- dose(
        x = targetEndOfTrial,
        model = model,
        samples = samples
      )
      ## Find the TDtarget End of trial estimate
      TDtargetEndOfTrialEstimate <- TDderive(TDtargetEndOfTrialSamples)

      ## Find the gain value samples then the GstarSamples
      points <- data@doseGrid

      GainSamples <- matrix(
        nrow = sampleSize(samples@options),
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
      doStop <- ratio <= stopping@targetRatio
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
        "targetRatio =", stopping@targetRatio
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
      stopping = "StoppingGstarCIRatio",
      dose = "ANY",
      samples = "missing",
      model = "ModelTox",
      data = "DataDual"
    ),
  def =
    function(stopping, dose, model, data, Effmodel, ...) {
      targetEndOfTrial <- stopping@targetEndOfTrial

      ## checks
      stopifnot(is.probability(targetEndOfTrial))
      stopifnot(is(Effmodel, "ModelEff"))


      ## find the TDtarget End of Trial
      TDtargetEndOfTrial <- dose(
        x = targetEndOfTrial,
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
      varlogGstar <- t(deltaG) %*% covBETA %*% deltaG



      ## Find the upper and lower limit of the 95% credibility interval of Gstar
      CIGstar <- c()
      CIGstar[2] <- exp(logGstar + 1.96 * sqrt(varlogGstar))
      CIGstar[1] <- exp(logGstar - 1.96 * sqrt(varlogGstar))

      ## The ratio of the upper to the lower 95% credibility interval
      ratioGstar <- as.numeric(CIGstar[2] / CIGstar[1])

      ## Find the variance of the log of the TDtargetEndOfTrial(eta)
      M1 <- matrix(c(-1 / (model@phi2), -(log(targetEndOfTrial / (1 - targetEndOfTrial)) - model@phi1) / (model@phi2)^2), 1, 2)
      M2 <- model@Pcov

      varEta <- M1 %*% M2 %*% t(M1)

      ## Find the upper and lower limit of the 95% credibility interval of
      ## TDtargetEndOfTrial
      CITDEOT <- c()
      CITDEOT[2] <- exp(log(TDtargetEndOfTrial) + 1.96 * sqrt(varEta))
      CITDEOT[1] <- exp(log(TDtargetEndOfTrial) - 1.96 * sqrt(varEta))
      ## The ratio of the upper to the lower 95% credibility interval
      ratioTDEOT <- as.numeric(CITDEOT[2] / CITDEOT[1])

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
      doStop <- ratio <= stopping@targetRatio
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
        "targetRatio =", stopping@targetRatio
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
##' (`patientGap`, `patientFollow`, `patientFollowMin`)
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
          vec = safetyWindow@sizeIntervals
        )

      ## so the safety window length is
      patientGap <- head(c(
        0, safetyWindow@patientGap[[interval]],
        rep(tail(safetyWindow@patientGap[[interval]], 1), 100)
      ), size)
      patientFollow <- safetyWindow@patientFollow
      patientFollowMin <- safetyWindow@patientFollowMin

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
        0, safetyWindow@patientGap,
        rep(tail(safetyWindow@patientGap, 1), 100)
      ), size)
      patientFollow <- safetyWindow@patientFollow
      patientFollowMin <- safetyWindow@patientFollowMin

      ret <- list(
        patientGap = patientGap,
        patientFollow = patientFollow,
        patientFollowMin = patientFollowMin
      )

      return(ret)
    }
)

# nolint end
