#' @include checkmate.R
#' @include Model-methods.R
#' @include Samples-class.R
#' @include Rules-class.R
#' @include helpers.R
#' @include helpers_rules.R
#' @include helpers_broom.R
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

#' @describeIn nextBest find the next best dose based on the EWOC rule.
#'
#' @aliases nextBest-NextBestEWOC
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestEWOC.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestEWOC",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
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
      nextBest@overdose,
      bounds_closed = c(FALSE, TRUE)
    ))

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    ) &
      (prob_overdose <= nextBest@max_overdose_prob)

    next_dose <- if (any(is_dose_eligible)) {
      # Take the highest eligible dose.
      next_best_level <- sum(is_dose_eligible)
      data@doseGrid[is_dose_eligible][next_best_level]
    } else {
      NA_real_
    }

    # Build plot for the overdosing probability.
    p <- ggplot() +
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
        yintercept = nextBest@max_overdose_prob * 100,
        lwd = 1.1,
        lty = 2,
        colour = "black"
      ) +
      ylim(c(0, 100)) +
      ylab("Overdose probability [%]")

    list(
      value = next_dose,
      plot = p,
      singlePlots = list(overdose = p),
      probs = cbind(
        dose = data@doseGrid,
        overdose = prob_overdose
      )
    )
  }
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
    dose_target_samples <- dose(x = nextBest@target, model, samples, ...)
    dose_target <- nextBest@derive(dose_target_samples)

    # Round to the next possible grid point.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )
    next_dose_level <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level]

    # Create a plot.
    p <- ggplot(
      data = data.frame(x = dose_target_samples),
      aes(.data$x)
    ) +
      geom_density(colour = "grey50") +
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
      nextBest@overdose,
      bounds_closed = c(FALSE, TRUE)
    ))
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))

    # Eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    ) &
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
      NA_real_
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
      coord_cartesian(ylim = c(0, 100)) +
      ylab(paste("Target probability [%]"))

    if (is.finite(doselimit)) {
      p1 <- p1 +
        geom_vline(xintercept = doselimit, lwd = 1.1, lty = 2, colour = "black")
    }

    if (any(is_dose_eligible)) {
      p1 <- p1 +
        geom_vline(
          xintercept = data@doseGrid[sum(is_dose_eligible)],
          lwd = 1.1,
          lty = 2,
          colour = "red"
        ) +
        geom_point(
          data = data.frame(
            x = next_dose,
            y = prob_target[is_dose_eligible][next_best_level] * 100 + 0.03
          ),
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
      geom_hline(
        yintercept = nextBest@max_overdose_prob * 100,
        lwd = 1.1,
        lty = 2,
        colour = "black"
      ) +
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
setMethod(
  "nextBest",
  signature = signature(
    nextBest = "NextBestNCRMLoss",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )
    # Compute probabilities to be in target and overdose tox interval.
    prob_underdosing <- colMeans(prob_samples < nextBest@target[1])
    prob_target <- colMeans(h_in_range(prob_samples, nextBest@target))
    prob_overdose <- colMeans(h_in_range(
      prob_samples,
      nextBest@overdose,
      bounds_closed = c(FALSE, TRUE)
    ))
    prob_mean <- colMeans(prob_samples)
    prob_sd <- apply(prob_samples, 2, stats::sd)

    is_unacceptable_specified <- any(nextBest@unacceptable != c(1, 1))

    prob_mat <- if (!is_unacceptable_specified) {
      cbind(
        underdosing = prob_underdosing,
        target = prob_target,
        overdose = prob_overdose
      )
    } else {
      prob_unacceptable <- colMeans(
        h_in_range(
          prob_samples,
          nextBest@unacceptable,
          bounds_closed = c(FALSE, TRUE)
        )
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
    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    ) &
      (prob_overdose < nextBest@max_overdose_prob)

    # Next best dose is the dose with the minimum loss function.
    next_dose <- if (any(is_dose_eligible)) {
      next_best_level <- which.min(posterior_loss[is_dose_eligible])
      data@doseGrid[is_dose_eligible][next_best_level]
    } else {
      NA_real_
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
      ifelse(
        (last_level == data@nGrid) || (nPatients[last_level + 1L] > 0),
        0L,
        1L
      )
    } else {
      # Rate is too high, deescalate it, unless an edge case of 1/3, where the decision
      # depends on the num. of patients: if >3, then deescalate it, otherwise stay.
      ifelse(
        (dlt_rate_last_level == 1 / 3) && (nPatients[last_level] <= 3L),
        0L,
        -1L
      )
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
          min(which(h_in_range(
            x,
            nextBest@target * diff(rng) + rng[1] + c(0, 1e-10),
            bounds_closed = c(FALSE, TRUE)
          )))
        })
        prob_target <- as.vector(table(factor(
          target_levels,
          levels = 1:data@nGrid
        )))
        prob_target / nrow(biom_samples)
      }
    } else {
      colMeans(h_in_range(biom_samples, nextBest@target))
    }

    # Now, compute probabilities to be in overdose tox interval, then flag
    # eligible grid doses after accounting for maximum possible dose and discarding overdoses.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples
    )
    prob_overdose <- colMeans(h_in_range(
      prob_samples,
      nextBest@overdose,
      bounds_closed = c(FALSE, TRUE)
    ))

    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    ) &
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
      NA_real_
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
      p1 <- p1 +
        geom_vline(xintercept = doselimit, lwd = 1.1, lty = 2, colour = "black")
    }

    if (any(is_dose_eligible)) {
      p1 <- p1 +
        geom_vline(
          xintercept = data@doseGrid[sum(is_dose_eligible)],
          lwd = 1.1,
          lty = 2,
          colour = "red"
        ) +
        geom_point(
          data = data.frame(
            x = next_dose,
            y = prob_target[is_dose_eligible][next_dose_level] * 100 + 0.03
          ),
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
      geom_hline(
        yintercept = nextBest@max_overdose_prob * 100,
        lwd = 1.1,
        lty = 2,
        colour = "black"
      ) +
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
    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )
    dlt_prob <- colMeans(prob_samples)

    # Determine the dose with the closest distance.
    dose_target <- data@doseGrid[which.min(abs(dlt_prob - nextBest@target))]

    # Determine next dose.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )
    next_dose_level_eligible <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level_eligible]

    # Create a plot.
    p <- ggplot(
      data = data.frame(x = data@doseGrid, y = dlt_prob),
      aes(.data$x, .data$y)
    ) +
      geom_line(colour = "grey50") +
      geom_point(fill = "grey50", colour = "grey50") +
      coord_cartesian(xlim = range(data@doseGrid)) +
      scale_x_continuous(
        labels = as.character(data@doseGrid),
        breaks = data@doseGrid,
        guide = guide_axis(check.overlap = TRUE)
      ) +
      geom_hline(yintercept = nextBest@target, linetype = "dotted") +
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
      xlab("Dose") +
      ylab("Posterior toxicity probability")

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

    list(
      value = next_dose,
      probs = cbind(dose = data@doseGrid, dlt_prob = dlt_prob),
      plot = p
    )
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
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )

    criterion <- colMeans(h_info_theory_dist(
      prob_samples,
      nextBest@target,
      nextBest@asymmetry
    ))

    is_dose_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo,
      levels = TRUE
    )
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
  definition = function(
    nextBest,
    doselimit = Inf,
    model,
    data,
    in_sim = FALSE,
    ...
  ) {
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Target dose estimates, i.e. the dose with probability of the occurrence of
    # a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt <- dose(x = prob_target_drt, model, ...)
    dose_target_eot <- dose(x = prob_target_eot, model, ...)

    # Find the next best doses in the doseGrid. The next best dose is the dose
    # at level closest and below the target dose estimate.
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )

    next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
    next_dose_drt <- doses_eligible[next_dose_lev_drt]

    next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
    next_dose_eot <- doses_eligible[next_dose_lev_eot]

    # Find the variance of the log of the dose_target_eot.
    mat <- matrix(
      c(
        -1 / (model@phi2),
        -(log(prob_target_eot / (1 - prob_target_eot)) - model@phi1) /
          (model@phi2)^2
      ),
      nrow = 1
    )
    var_dose_target_eot <- as.vector(mat %*% model@Pcov %*% t(mat))

    # 95% credibility interval.
    ci_dose_target_eot <- exp(
      log(dose_target_eot) + c(-1, 1) * 1.96 * sqrt(var_dose_target_eot)
    )
    cir_dose_target_eot <- ci_dose_target_eot[2] / ci_dose_target_eot[1]

    # Build plot.
    p <- h_next_best_td_plot(
      prob_target_drt = prob_target_drt,
      dose_target_drt = dose_target_drt,
      prob_target_eot = prob_target_eot,
      dose_target_eot = dose_target_eot,
      data = data,
      prob_dlt = prob(dose = data@doseGrid, model = model, ...),
      doselimit = doselimit,
      next_dose = next_dose_drt
    )

    if (
      !h_in_range(
        dose_target_drt,
        range = dose_grid_range(data),
        bounds_closed = TRUE
      ) &&
        !in_sim
    ) {
      warning(paste(
        "TD",
        prob_target_drt * 100,
        "=",
        dose_target_drt,
        "not within dose grid"
      ))
    }
    if (
      !h_in_range(
        dose_target_eot,
        range = dose_grid_range(data),
        bounds_closed = TRUE
      ) &&
        !in_sim
    ) {
      warning(paste(
        "TD",
        prob_target_eot * 100,
        "=",
        dose_target_eot,
        "not within dose grid"
      ))
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
  definition = function(
    nextBest,
    doselimit = Inf,
    samples,
    model,
    data,
    in_sim,
    ...
  ) {
    # Generate target dose samples, i.e. the doses with probability of the
    # occurrence of a DLT that equals to the nextBest@prob_target_drt
    # (or nextBest@prob_target_eot, respectively).
    dose_target_drt_samples <- dose(
      x = nextBest@prob_target_drt,
      model,
      samples,
      ...
    )
    dose_target_eot_samples <- dose(
      x = nextBest@prob_target_eot,
      model,
      samples,
      ...
    )

    # Derive the prior/posterior estimates based on two above samples.
    dose_target_drt <- nextBest@derive(dose_target_drt_samples)
    dose_target_eot <- nextBest@derive(dose_target_eot_samples)

    # Find the next doses in the doseGrid. The next dose is the dose at level
    # closest and below the dose_target_drt (or dose_target_eot, respectively).
    # h_find_interval assumes that elements in doses_eligible are strictly increasing.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )

    next_dose_lev_drt <- h_find_interval(dose_target_drt, doses_eligible)
    next_dose_drt <- doses_eligible[next_dose_lev_drt]

    next_dose_lev_eot <- h_find_interval(dose_target_eot, doses_eligible)
    next_dose_eot <- doses_eligible[next_dose_lev_eot]

    # 95% credibility interval.
    ci_dose_target_eot <- as.numeric(quantile(
      dose_target_eot_samples,
      probs = c(0.025, 0.975)
    ))
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
  definition = function(
    nextBest,
    doselimit = Inf,
    model,
    data,
    model_eff,
    in_sim = FALSE,
    ...
  ) {
    assert_class(model_eff, "Effloglog")
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Target dose estimates, i.e. the dose with probability of the occurrence of
    # a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt <- dose(x = prob_target_drt, model, ...)
    dose_target_eot <- dose(x = prob_target_eot, model, ...)

    # Find the dose which gives the maximum gain.
    dosegrid_range <- dose_grid_range(data)
    opt <- optim(
      par = dosegrid_range[1],
      fn = function(DOSE) {
        -gain(DOSE, model_dle = model, model_eff = model_eff, ...)
      },
      method = "L-BFGS-B",
      lower = dosegrid_range[1],
      upper = dosegrid_range[2]
    )
    dose_mg <- opt$par # this is G*. # no lintr
    max_gain <- -opt$value

    # Print info message if dose target is outside of the range.
    if (
      !h_in_range(
        dose_target_drt,
        range = dose_grid_range(data),
        bounds_closed = FALSE
      ) &&
        !in_sim
    ) {
      print(paste(
        "Estimated TD",
        prob_target_drt * 100,
        "=",
        dose_target_drt,
        "not within dose grid"
      ))
    }
    if (
      !h_in_range(
        dose_target_eot,
        range = dose_grid_range(data),
        bounds_closed = FALSE
      ) &&
        !in_sim
    ) {
      print(paste(
        "Estimated TD",
        prob_target_eot * 100,
        "=",
        dose_target_eot,
        "not within dose grid"
      ))
    }
    if (
      !h_in_range(
        dose_mg,
        range = dose_grid_range(data),
        bounds_closed = FALSE
      ) &&
        !in_sim
    ) {
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
  definition = function(
    nextBest,
    doselimit = Inf,
    samples,
    model,
    data,
    model_eff,
    samples_eff,
    in_sim = FALSE,
    ...
  ) {
    assert_true(
      test_class(model_eff, "Effloglog") || test_class(model_eff, "EffFlexi")
    )
    assert_class(samples_eff, "Samples")
    assert_flag(in_sim)

    # 'drt' - during the trial, 'eot' end of trial.
    prob_target_drt <- nextBest@prob_target_drt
    prob_target_eot <- nextBest@prob_target_eot

    # Generate target dose samples, i.e. the doses with probability of the
    # occurrence of a DLT that equals to the prob_target_drt or prob_target_eot.
    dose_target_drt_samples <- dose(
      x = prob_target_drt,
      model,
      samples = samples,
      ...
    )
    dose_target_eot_samples <- dose(
      x = prob_target_eot,
      model,
      samples = samples,
      ...
    )

    # Derive the prior/posterior estimates based on two above samples.
    dose_target_drt <- nextBest@derive(dose_target_drt_samples)
    dose_target_eot <- nextBest@derive(dose_target_eot_samples)

    # Gain samples.
    gain_samples <- sapply(
      data@doseGrid,
      gain,
      model,
      samples,
      model_eff,
      samples_eff,
      ...
    )
    # For every sample, get the dose (from the dose grid) that gives the maximum gain value.
    dose_lev_mg_samples <- apply(gain_samples, 1, which.max)
    dose_mg_samples <- data@doseGrid[dose_lev_mg_samples]
    # Maximum gain dose estimate is the nth percentile of the maximum gain dose samples.
    dose_mg <- nextBest@mg_derive(dose_mg_samples)
    gain_values <- apply(gain_samples, 2, FUN = nextBest@mg_derive)

    # Print info message if dose target is outside of the range.
    dosegrid_range <- dose_grid_range(data)
    if (
      !h_in_range(
        dose_target_drt,
        range = dosegrid_range,
        bounds_closed = FALSE
      ) &&
        !in_sim
    ) {
      print(paste(
        "Estimated TD",
        prob_target_drt * 100,
        "=",
        dose_target_drt,
        "not within dose grid"
      ))
    }
    if (
      !h_in_range(
        dose_target_eot,
        range = dosegrid_range,
        bounds_closed = FALSE
      ) &&
        !in_sim
    ) {
      print(paste(
        "Estimated TD",
        prob_target_eot * 100,
        "=",
        dose_target_eot,
        "not within dose grid"
      ))
    }
    if (
      !h_in_range(dose_mg, range = dosegrid_range, bounds_closed = FALSE) &&
        !in_sim
    ) {
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

    ci_dose_target_eot <- as.numeric(quantile(
      dose_target_eot,
      probs = c(0.025, 0.975)
    ))
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
      dose_grid_range = dosegrid_range
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

## NextBestProbMTDLTE ----

#' @describeIn nextBest find the next best dose based with the highest
#'  probability of having a toxicity rate less or equal to the target toxicity
#'  level.
#'
#' @aliases nextBest-NextBestProbMTDLTE
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestProbMTDLTE.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestProbMTDLTE",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )

    # Determine the maximum dose level with a toxicity probability below or
    # equal to the target and calculate how often a dose is selected as MTD
    # across iterations.
    # The first element of the vector is the relative frequency that no
    # dose in the grid is below or equal to the target, the
    # second element that the 1st dose of the grid is the MTD, etc..
    prob_mtd_lte <- prop.table(
      table(factor(
        rowSums(prob_samples <= nextBest@target),
        levels = 0:data@nGrid
      ))
    )

    allocation_crit <- as.vector(prob_mtd_lte)
    names(allocation_crit) <- as.character(c(0, data@doseGrid))

    # In case that placebo is used, placebo and the portion that is not assigned
    # to any dose of the grid are merged.
    if (data@placebo) {
      allocation_crit[1] <- sum(allocation_crit[1:2])
      allocation_crit <- allocation_crit[-2]
    }

    # Handling of the portion that is not assigned to an active dose of
    # the dose grid. The portion is added to the minimum active dose
    # of the dose grid.
    allocation_crit[2] <- sum(allocation_crit[1:2])
    allocation_crit <- allocation_crit[-1]

    # Determine the dose with the highest relative frequency.
    allocation_crit_dose <- as.numeric(names(allocation_crit))
    dose_target <- allocation_crit_dose[which.max(allocation_crit)]

    # Determine next dose.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )
    next_dose_level_eligible <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level_eligible]

    # Create a plot.
    plt_data <- if (data@placebo && (data@doseGrid[1] == next_dose)) {
      data.frame(
        x = as.factor(data@doseGrid),
        y = c(0, as.numeric(allocation_crit)) * 100
      )
    } else {
      data.frame(
        x = as.factor(allocation_crit_dose),
        y = as.numeric(allocation_crit) * 100
      )
    }

    p <- ggplot(
      data = plt_data
    ) +
      geom_col(aes(x, y), fill = "grey75") +
      scale_x_discrete(drop = FALSE, guide = guide_axis(check.overlap = TRUE)) +
      geom_vline(
        xintercept = as.factor(dose_target),
        lwd = 1.1,
        colour = "black"
      ) +
      geom_text(
        data = data.frame(x = as.factor(dose_target)),
        aes(.data$x, 0),
        label = "Est",
        vjust = -0.5,
        hjust = -0.5,
        colour = "black",
        angle = 90
      ) +
      xlab("Dose") +
      ylab(paste("Allocation criterion [%]"))

    if (is.finite(doselimit)) {
      doselimit_level <- if (sum(allocation_crit_dose == doselimit) > 0) {
        which(allocation_crit_dose == doselimit)
      } else {
        ifelse(
          test = data@placebo && (data@doseGrid[1] == next_dose),
          yes = 1.5,
          no = sum(allocation_crit_dose < doselimit) + 0.5
        )
      }

      p <- p +
        geom_vline(
          xintercept = doselimit_level,
          colour = "red",
          lwd = 1.1
        ) +
        geom_text(
          data = data.frame(x = doselimit_level),
          aes(.data$x, 0),
          label = "Max",
          vjust = -0.5,
          hjust = -1.5,
          colour = "red",
          angle = 90
        )
    }

    p <- p +
      geom_vline(
        xintercept = as.factor(next_dose),
        colour = "blue",
        lwd = 1.1
      ) +
      geom_text(
        data = data.frame(x = as.factor(next_dose)),
        aes(.data$x, 0),
        label = "Next",
        vjust = -0.5,
        hjust = -2.5,
        colour = "blue",
        angle = 90
      )

    list(
      value = next_dose,
      allocation = cbind(
        dose = allocation_crit_dose,
        allocation = allocation_crit
      ),
      plot = p
    )
  }
)

## NextBestProbMTDMinDist ----

#' @describeIn nextBest find the next best dose based with the highest
#'  probability of having a toxicity rate with minimum distance to the
#'  target toxicity level.
#'
#' @aliases nextBest-NextBestProbMTDMinDist
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestProbMtdMinDist.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestProbMTDMinDist",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit, samples, model, data, ...) {
    # Matrix with samples from the dose-tox curve at the dose grid points.
    prob_samples <- sapply(
      data@doseGrid,
      prob,
      model = model,
      samples = samples,
      ...
    )

    # Determine which dose level has the minimum distance to target.
    dose_min_mtd_dist <- apply(
      prob_samples,
      1,
      function(x) which.min(abs(x - nextBest@target))
    )

    allocation_crit <- prop.table(
      table(factor(dose_min_mtd_dist, levels = 1:data@nGrid))
    )
    names(allocation_crit) <- as.character(data@doseGrid)

    # In case that placebo is used, placebo and the first non-placebo dose
    # of the grid are merged.
    if (data@placebo) {
      allocation_crit[2] <- sum(allocation_crit[1:2])
      allocation_crit <- allocation_crit[-1]
    }

    # Determine the dose with the highest relative frequency.
    allocation_crit_dose <- as.numeric(names(allocation_crit))
    dose_target <- allocation_crit_dose[which.max(allocation_crit)]

    # Determine next dose.
    doses_eligible <- h_next_best_eligible_doses(
      data@doseGrid,
      doselimit,
      data@placebo
    )
    next_dose_level_eligible <- which.min(abs(doses_eligible - dose_target))
    next_dose <- doses_eligible[next_dose_level_eligible]

    # Create a plot.
    plt_data <- if (data@placebo && data@doseGrid[1] == next_dose) {
      data.frame(
        x = as.factor(data@doseGrid),
        y = c(0, as.numeric(allocation_crit)) * 100
      )
    } else {
      data.frame(
        x = as.factor(allocation_crit_dose),
        y = as.numeric(allocation_crit) * 100
      )
    }

    p <- ggplot(
      data = plt_data
    ) +
      geom_col(aes(x, y), fill = "grey75") +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      geom_vline(
        xintercept = as.factor(dose_target),
        lwd = 1.1,
        colour = "black"
      ) +
      geom_text(
        data = data.frame(x = as.factor(dose_target)),
        aes(.data$x, 0),
        label = "Est",
        vjust = -0.5,
        hjust = -0.5,
        colour = "black",
        angle = 90
      ) +
      xlab("Dose") +
      ylab(paste("Allocation criterion [%]"))

    if (is.finite(doselimit)) {
      doselimit_level <- if (any(allocation_crit_dose == doselimit)) {
        which(allocation_crit_dose == doselimit)
      } else {
        ifelse(
          test = data@placebo && data@doseGrid[1] == next_dose,
          yes = 1.5,
          no = sum(allocation_crit_dose < doselimit) + 0.5
        )
      }

      p <- p +
        geom_vline(
          xintercept = doselimit_level,
          colour = "red",
          lwd = 1.1
        ) +
        geom_text(
          data = data.frame(x = doselimit_level),
          aes(.data$x, 0),
          label = "Max",
          vjust = -0.5,
          hjust = -1.5,
          colour = "red",
          angle = 90
        )
    }

    p <- p +
      geom_vline(
        xintercept = as.factor(next_dose),
        colour = "blue",
        lwd = 1.1
      ) +
      geom_text(
        data = data.frame(x = as.factor(next_dose)),
        aes(.data$x, 0),
        label = "Next",
        vjust = -0.5,
        hjust = -2.5,
        colour = "blue",
        angle = 90
      )

    list(
      value = next_dose,
      allocation = cbind(
        dose = allocation_crit_dose,
        allocation = allocation_crit
      ),
      plot = p
    )
  }
)

## NextBestOrdinal ----

#' @describeIn nextBest find the next best dose for ordinal CRM models.
#'
#' @aliases nextBest-NextBestOrdinal
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestOrdinal.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestOrdinal",
    doselimit = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "Data"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
    stop(
      paste0(
        "NextBestOrdinal objects can only be used with LogisticLogNormalOrdinal ",
        "models and DataOrdinal data objects. In this case, the model is a '",
        class(model),
        "' object and the data is in a ",
        class(data),
        " object."
      )
    )
  }
)

#' @describeIn nextBest find the next best dose for ordinal CRM models.
#'
#' @aliases nextBest-NextBestOrdinal
#'
#' @export
#' @example examples/Rules-method-nextBest-NextBestOrdinal.R
#'
setMethod(
  f = "nextBest",
  signature = signature(
    nextBest = "NextBestOrdinal",
    doselimit = "numeric",
    samples = "Samples",
    model = "LogisticLogNormalOrdinal",
    data = "DataOrdinal"
  ),
  definition = function(nextBest, doselimit = Inf, samples, model, data, ...) {
    nextBest(
      nextBest = nextBest@rule,
      doselimit = doselimit,
      samples = h_convert_ordinal_samples(samples, nextBest@grade),
      model = h_convert_ordinal_model(model, nextBest@grade),
      data = h_convert_ordinal_data(data, nextBest@grade),
      ...
    )
  }
)

# maxDose ----

## generic ----

#' Determine the Maximum Possible Next Dose
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function determines the upper limit of the next dose based on the
#' `increments`and the `data`.
#'
#' @param increments (`Increments`)\cr the rule for the next best dose.
#' @param data (`Data`)\cr input data.
#' @param ... additional arguments without method dispatch.
#'
#' @return A `number`, the maximum possible next dose.
#'
#' @export
#'
setGeneric(
  name = "maxDose",
  def = function(increments, data, ...) {
    standardGeneric("maxDose")
  },
  valueClass = "numeric"
)

## IncrementsRelative ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   relative increments.
#'
#' @aliases maxDose-IncrementsRelative
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsRelative.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsRelative",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    if (data@nObs == 0L) {
      # In this case we return Inf, because there is no restriction
      # from this stopping rule because we cannot reference any
      # previous dose. In practice this does not matter because
      # there is a starting dose fixed externally anyway.
      return(Inf)
    }
    last_dose <- data@x[data@nObs]
    # Determine in which interval the `last_dose` is.
    assert_true(last_dose >= head(increments@intervals, 1))
    last_dose_interval <- findInterval(
      x = last_dose,
      vec = increments@intervals
    )
    (1 + increments@increments[last_dose_interval]) * last_dose
  }
)

## IncrementsRelativeDLT ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   relative increments determined by DLTs so far.
#'
#' @aliases maxDose-IncrementsRelativeDLT
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsRelativeDLT.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsRelativeDLT",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    dlt_count <- sum(data@y)
    # Determine in which interval the `dlt_count` is.
    assert_true(dlt_count >= increments@intervals[1])
    dlt_count_interval <- findInterval(
      x = dlt_count,
      vec = increments@intervals
    )
    (1 + increments@increments[dlt_count_interval]) * data@x[data@nObs]
  }
)

## IncrementsRelativeDLTCurrent ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   relative increments determined by DLTs in the current cohort.
#'
#' @aliases maxDose-IncrementsRelativeDLTCurrent
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsRelativeDLTCurrent.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsRelativeDLTCurrent",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    last_dose <- data@x[data@nObs]

    # Determine how many DLTs have occurred in the last cohort.
    last_cohort <- data@cohort[data@nObs]
    last_cohort_indices <- which(data@cohort == last_cohort)
    dlt_count_lcohort <- sum(data@y[last_cohort_indices])

    # Determine in which interval the `dlt_count_lcohort` is.
    assert_true(dlt_count_lcohort >= increments@intervals[1])
    dlt_count_lcohort_int <- findInterval(
      x = dlt_count_lcohort,
      vec = increments@intervals
    )
    (1 + increments@increments[dlt_count_lcohort_int]) * last_dose
  }
)

## IncrementsRelativeParts ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   relative increments as well as part 1 and beginning of part 2.
#'
#' @aliases maxDose-IncrementsRelativeParts
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsRelativeParts.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsRelativeParts",
    data = "DataParts"
  ),
  definition = function(increments, data, ...) {
    all_in_part1 <- all(data@part == 1L)
    incrmnt <- if (all_in_part1) {
      part2_started <- data@nextPart == 2L
      if (part2_started) {
        any_dlt <- any(data@y == 1L)
        if (any_dlt) {
          increments@dlt_start
        } else if (increments@clean_start <= 0L) {
          increments@clean_start
        }
      } else {
        1L
      }
    }

    if (is.null(incrmnt)) {
      callNextMethod(increments, data, ...)
    } else {
      max_dose_lev_part1 <- match_within_tolerance(
        max(data@x),
        data@part1Ladder
      )
      new_max_dose_level <- max_dose_lev_part1 + incrmnt
      assert_true(new_max_dose_level >= 0L)
      assert_true(new_max_dose_level <= length(data@part1Ladder))
      data@part1Ladder[new_max_dose_level]
    }
  }
)

## IncrementsDoseLevels ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   the number of dose grid levels. That is, the max dose is determined as
#'   the one which level is equal to: base dose level + level increment.
#'   The base dose level is the level of the last dose in grid or the level
#'   of the maximum dose applied, which is defined in `increments` object.
#'   Find out more in [`IncrementsDoseLevels`].
#'
#' @aliases maxDose-IncrementsDoseLevels
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsDoseLevels.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsDoseLevels",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    # Determine what is the basis level for increment,
    # i.e. the last dose or the max dose applied.
    basis_dose_level <- ifelse(
      increments@basis_level == "last",
      data@xLevel[data@nObs],
      max(data@xLevel)
    )
    max_dose_level <- min(basis_dose_level + increments@levels, data@nGrid)
    data@doseGrid[max_dose_level]
  }
)

## IncrementsHSRBeta ----

#' @describeIn maxDose determine the maximum possible next dose for escalation.
#'
#' @aliases maxDose-IncrementsHSRBeta
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsHSRBeta.R
#'
setMethod(
  f = "maxDose",
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
    max(
      data@doseGrid[data@doseGrid < dose_tox],
      data@doseGrid[data@placebo + 1]
    )
  }
)

## IncrementsMin ----

#' @describeIn maxDose determine the maximum possible next dose based on
#'   multiple increment rules, taking the minimum across individual increments.
#'
#' @aliases maxDose-IncrementsMin
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsMin.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsMin",
    data = "Data"
  ),
  definition = function(increments, data, ...) {
    individual_results <- sapply(
      increments@increments_list,
      maxDose,
      data = data,
      ...
    )
    min(individual_results)
  }
)

#' @describeIn maxDose determine the maximum possible next dose based on
#'   multiple increment rules, taking the minimum across individual increments.
#'
#' @aliases maxDose-IncrementsMin
#'
#' @export
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsMin",
    data = "DataOrdinal"
  ),
  definition = function(increments, data, ...) {
    individual_results <- sapply(
      increments@increments_list,
      maxDose,
      data = data,
      ...
    )
    min(individual_results)
  }
)

## IncrementsOrdinal ----

#' @describeIn maxDose determine the maximum possible next dose in an ordinal
#' CRM trial
#'
#' @aliases maxDose-IncrementsOrdinal
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsOrdinal.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsOrdinal",
    data = "DataOrdinal"
  ),
  definition = function(increments, data, ...) {
    maxDose(
      increments = increments@rule,
      data = h_convert_ordinal_data(
        data,
        increments@grade,
        ...
      )
    )
  }
)

## IncrementsMaxToxProb ----

#' @describeIn maxDose determine the maximum possible next dose based on the
#' probability of toxicity
#' @param model (`GeneralModel`)\cr The model on which probabilities will be based
#' @param samples (`Samples`)\cr The MCMC samples to which `model` will be applied
#'
#' @aliases maxDose-IncrementsMaxToxProb
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsMaxToxProb.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsMaxToxProb",
    data = "DataOrdinal"
  ),
  definition = function(increments, data, model, samples, ...) {
    assert_class(samples, "Samples")
    assert_true(length(increments@prob) == length(data@yCategories) - 1)
    nm <- utils::tail(names(data@yCategories), -1)
    assert_set_equal(names(increments@prob), nm)

    probs <- dplyr::bind_rows(
      lapply(
        seq_along(increments@prob),
        function(g) {
          fitted_probs <- fit(samples, model, data, grade = g, ...)
          safe_fitted_probs <- dplyr::filter(
            fitted_probs,
            middle < increments@prob[nm[g]]
          )
          highest_safe_fitted_prob <- utils::tail(safe_fitted_probs, 1)
        }
      )
    )
    min(probs$dose)
  }
)
#' @describeIn maxDose determine the maximum possible next dose based on the
#' probability of toxicity
#' @param model (`GeneralModel`)\cr The model on which probabilities will be based
#' @param samples (`Samples`)\cr The MCMC samples to which `model` will be applied
#'
#' @aliases maxDose-IncrementsMaxToxProb
#'
#' @export
#' @example examples/Rules-method-maxDose-IncrementsMaxToxProb.R
#'
setMethod(
  f = "maxDose",
  signature = signature(
    increments = "IncrementsMaxToxProb",
    data = "Data"
  ),
  definition = function(increments, data, model, samples, ...) {
    assert_class(samples, "Samples")
    assert_true(length(increments@prob) == 1)

    fitted_prob <- fit(samples, model, data, ...)
    safe_fitted_prob <- dplyr::filter(fitted_prob, middle < increments@prob)
    highest_safe_fitted_prob <- utils::tail(safe_fitted_prob, 1)
    highest_safe_fitted_prob$dose
  }
)

## tidy-IncrementsMaxToxProb ----

#' @rdname tidy
#' @aliases tidy-IncrementsMaxToxProb
#' @example examples/Rules-method-tidyIncrementsMaxToxProb.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsMaxToxProb"),
  definition = function(x, ...) {
    grades <- names(x@prob)
    if (is.null(grades)) {
      grades <- "1"
    }
    tibble(
      Grade = grades,
      Prob = x@prob
    ) %>%
      h_tidy_class(x)
  }
)

# and-Stopping-Stopping ----

#' Combine Two Stopping Rules with AND
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining two atomic stopping rules.
#'
#' @param e1 (`Stopping`)\cr first stopping rule object.
#' @param e2 (`Stopping`)\cr second stopping rule object.
#'
#' @return The [`StoppingAll`] object.
#'
#' @aliases and-Stopping-Stopping
#' @example examples/Rules-method-and-stopping-stopping.R
#' @export
#'
setMethod(
  f = "&",
  signature = signature(
    e1 = "Stopping",
    e2 = "Stopping"
  ),
  definition = function(e1, e2) {
    StoppingAll(list(e1, e2))
  }
)

# and-StoppingAll-Stopping ----

#' Combine a Stopping List and an Atomic Stopping Rule with AND
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining a stopping list and an atomic stopping rule.
#'
#' @param e1 (`StoppingAll`)\cr stopping list object.
#' @param e2 (`Stopping`)\cr stopping rule object.
#'
#' @return The modified [`StoppingAll`] object.
#'
#' @aliases and-StoppingAll-Stopping
#' @example examples/Rules-method-and-stoppingAll-stopping.R
#' @export
#'
setMethod(
  f = "&",
  signature = signature(
    e1 = "StoppingAll",
    e2 = "Stopping"
  ),
  definition = function(e1, e2) {
    e1@stop_list <- c(
      e1@stop_list,
      e2
    )
    e1
  }
)

# and-Stopping-StoppingAll ----

#' Combine an Atomic Stopping Rule and a Stopping List with AND
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining an atomic stopping rule and a stopping list.
#'
#' @param e1 (`Stopping`)\cr stopping rule object.
#' @param e2 (`StoppingAll`)\cr stopping list object.
#'
#' @return The modified [`StoppingAll`] object.
#'
#' @aliases and-Stopping-StoppingAll
#' @example examples/Rules-method-and-stopping-stoppingAll.R
#' @export
#'
setMethod(
  f = "&",
  signature = signature(
    e1 = "Stopping",
    e2 = "StoppingAll"
  ),
  definition = function(e1, e2) {
    e2@stop_list <- c(
      e1,
      e2@stop_list
    )
    e2
  }
)

# or-Stopping-Stopping ----

#' Combine Two Stopping Rules with OR
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining two atomic stopping rules.
#'
#' @param e1 (`Stopping`)\cr first stopping rule object.
#' @param e2 (`Stopping`)\cr second stopping rule object.
#'
#' @return The [`StoppingAny`] object.
#'
#' @aliases |,Stopping,Stopping-method
#' @name or-Stopping-Stopping
#' @example examples/Rules-method-or-stopping-stopping.R
#' @export
#'
setMethod(
  f = "|",
  signature = signature(
    e1 = "Stopping",
    e2 = "Stopping"
  ),
  definition = function(e1, e2) {
    StoppingAny(list(e1, e2))
  }
)

# or-StoppingAny-Stopping ----

#' Combine a Stopping List and an Atomic Stopping Rule with OR
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining a stopping list and an atomic stopping rule.
#'
#' @param e1 (`StoppingAny`)\cr stopping list object.
#' @param e2 (`Stopping`)\cr stopping rule object.
#'
#' @return The modified [`StoppingAny`] object.
#'
#' @aliases |,StoppingAny,Stopping-method
#' @name or-StoppingAny-Stopping
#' @example examples/Rules-method-or-stoppingAny-stopping.R
#' @export
#'
setMethod(
  f = "|",
  signature = signature(
    e1 = "StoppingAny",
    e2 = "Stopping"
  ),
  definition = function(e1, e2) {
    e1@stop_list <- c(
      e1@stop_list,
      e2
    )
    e1
  }
)

# or-Stopping-StoppingAny ----

#' Combine an Atomic Stopping Rule and a Stopping List with OR
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The method combining an atomic stopping rule and a stopping list.
#'
#' @param e1 (`Stopping`)\cr stopping rule object.
#' @param e2 (`StoppingAny`)\cr stopping list object.
#'
#' @return The modified [`StoppingAny`] object.
#'
#' @aliases |,Stopping,StoppingAny-method
#' @name or-Stopping-StoppingAny
#' @example examples/Rules-method-or-stopping-stoppingAny.R
#' @export
#'
setMethod(
  f = "|",
  signature = signature(
    e1 = "Stopping",
    e2 = "StoppingAny"
  ),
  definition = function(e1, e2) {
    e2@stop_list <- c(
      e1,
      e2@stop_list
    )
    e2
  }
)

# Stopping ----

## stopTrial ----

#' Stop the trial?
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function returns whether to stop the trial.
#'
#' @param stopping (`Stopping`)\cr the rule for stopping the trial.
#' @param dose the recommended next best dose.
#' @param samples (`Samples`)\cr the mcmc samples.
#' @param model (`GeneralModel`)\cr the model.
#' @param data (`Data`)\cr input data.
#' @param ... additional arguments without method dispatch.
#'
#' @return logical value: `TRUE` if the trial can be stopped, `FALSE`
#' otherwise. It should have an attribute `message` which gives the reason
#' for the decision.
#'
#' @export
#' @example examples/Rules-method-CombiningStoppingRulesAndOr.R
setGeneric(
  name = "stopTrial",
  def = function(stopping, dose, samples, model, data, ...) {
    standardGeneric("stopTrial")
  },
  valueClass = "logical"
)

## stopTrial-StoppingMissingDose ----

#' @describeIn stopTrial Stop based on value returned by next best dose.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @aliases stopTrial-StoppingMissingDose
#' @example examples/Rules-method-stopTrial-StoppingMissingDose.R
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMissingDose",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    do_stop <- is.na(dose) || (data@placebo && dose == min(data@doseGrid))

    msg <- paste(
      "Next dose is",
      ifelse(
        do_stop,
        paste(
          ifelse(
            data@placebo && dose == min(data@doseGrid),
            "placebo dose",
            "NA"
          ),
          ", i.e., no active dose is safe enough according to the NextBest rule."
        ),
        "available at the dose grid."
      )
    )

    structure(do_stop, message = msg, report_label = stopping@report_label)
  }
)

## stopTrial-StoppingList ----

#' @describeIn stopTrial Stop based on multiple stopping rules.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingList
#' @example examples/Rules-method-stopTrial-StoppingList.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingList",
    dose = "ANY",
    samples = "ANY",
    model = "ANY",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Evaluate the individual stopping rules in the list.
    individual_results <-
      if (missing(samples)) {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          model = model,
          data = data,
          ...
        )
      } else {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          samples = samples,
          model = model,
          data = data,
          ...
        )
      }

    # Summarize to obtain overall result.
    overall_result <- stopping@summary(as.logical(individual_results))

    # Retrieve individual text messages, but let them in the list structure.
    overall_text <- lapply(individual_results, attr, "message")

    structure(
      overall_result,
      message = overall_text,
      individual = individual_results
    )
  }
)

## stopTrial-StoppingAll ----

#' @describeIn stopTrial Stop based on fulfillment of all multiple stopping
#'   rules.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingAll
#' @example examples/Rules-method-stopTrial-StoppingAll.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingAll",
    dose = "ANY",
    samples = "ANY",
    model = "ANY",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Evaluate the individual stopping rules in the list.
    individual_results <-
      if (missing(samples)) {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          model = model,
          data = data,
          ...
        )
      } else {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          samples = samples,
          model = model,
          data = data,
          ...
        )
      }

    # Summarize to obtain overall result.
    overall_result <- all(as.logical(individual_results))

    # Retrieve individual text messages, but let them in the list structure.
    overall_text <- lapply(individual_results, attr, "message")

    structure(
      overall_result,
      message = overall_text,
      individual = individual_results,
      report_label = stopping@report_label
    )
  }
)


## stopTrial-StoppingAny ----

#' @describeIn stopTrial Stop based on fulfillment of any stopping rule.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingAny
#' @example examples/Rules-method-stopTrial-StoppingAny.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingAny",
    dose = "ANY",
    samples = "ANY",
    model = "ANY",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Evaluate the individual stopping rules in the list.
    individual_results <-
      if (missing(samples)) {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          model = model,
          data = data,
          ...
        )
      } else {
        lapply(
          stopping@stop_list,
          stopTrial,
          dose = dose,
          samples = samples,
          model = model,
          data = data,
          ...
        )
      }

    # Summarize to obtain overall result.
    overall_result <- any(as.logical(individual_results))

    # Retrieve individual text messages, but let them in the list structure.
    overall_text <- lapply(individual_results, attr, "message")

    structure(
      overall_result,
      message = overall_text,
      individual = individual_results,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingCohortsNearDose ----

#' @describeIn stopTrial Stop based on number of cohorts near to next best
#'   dose.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingCohortsNearDose
#' @example examples/Rules-method-stopTrial-StoppingCohortsNearDose.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingCohortsNearDose",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Determine the range where the cohorts must lie in.
    lower <- (100 - stopping@percentage) / 100 * dose
    upper <- (100 + stopping@percentage) / 100 * dose

    # Which patients lie there?
    index_patients <- which((data@x >= lower) & (data@x <= upper))

    # How many cohorts?
    n_cohorts <- length(unique(data@cohort[index_patients]))

    # So can we stop?
    do_stop <- n_cohorts >= stopping@nCohorts

    # Generate message.
    text <- paste(
      n_cohorts,
      " cohorts lie within ",
      stopping@percentage,
      "% of the next best dose ",
      dose,
      ". This ",
      ifelse(do_stop, "reached", "is below"),
      " the required ",
      stopping@nCohorts,
      " cohorts",
      sep = ""
    )

    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingPatientsNearDose ----

#' @describeIn stopTrial Stop based on number of patients near to next best
#'   dose.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingPatientsNearDose
#' @example examples/Rules-method-stopTrial-StoppingPatientsNearDose.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingPatientsNearDose",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Determine the range where the cohorts must lie in.
    lower <- (100 - stopping@percentage) / 100 * dose
    upper <- (100 + stopping@percentage) / 100 * dose

    # How many patients lie there?
    n_patients <- ifelse(
      is.na(dose),
      0,
      sum((data@x >= lower) & (data@x <= upper))
    )

    # So can we stop?
    do_stop <- n_patients >= stopping@nPatients

    # Generate message.
    text <- paste(
      n_patients,
      " patients lie within ",
      stopping@percentage,
      "% of the next best dose ",
      dose,
      ". This ",
      ifelse(do_stop, "reached", "is below"),
      " the required ",
      stopping@nPatients,
      " patients",
      sep = ""
    )

    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingMinCohorts ----

#' @describeIn stopTrial Stop based on minimum number of cohorts.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingMinCohorts
#' @example examples/Rules-method-stopTrial-StoppingMinCohorts.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMinCohorts",
    dose = "ANY",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Determine number of cohorts.
    n_cohorts <- length(unique(data@cohort))

    # So can we stop?
    do_stop <- n_cohorts >= stopping@nCohorts

    # Generate message.
    text <-
      paste(
        "Number of cohorts is",
        n_cohorts,
        "and thus",
        ifelse(do_stop, "reached", "below"),
        "the prespecified minimum number",
        stopping@nCohorts
      )

    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingMinPatients ----

#' @describeIn stopTrial Stop based on minimum number of patients.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingMinPatients
#' @example examples/Rules-method-stopTrial-StoppingMinPatients.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMinPatients",
    dose = "ANY",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # So can we stop?
    do_stop <- data@nObs >= stopping@nPatients

    # Generate message.
    text <-
      paste(
        "Number of patients is",
        data@nObs,
        "and thus",
        ifelse(do_stop, "reached", "below"),
        "the prespecified minimum number",
        stopping@nPatients
      )

    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingTargetProb ----

#' @describeIn stopTrial Stop based on probability of target tox interval
#'
#' @aliases stopTrial-StoppingTargetProb
#' @example examples/Rules-method-stopTrial-StoppingTargetProb.R
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingTargetProb",
    dose = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Compute probability to be in target interval.
    prob_target <- ifelse(
      is.na(dose),
      0,
      mean(
        prob(dose = dose, model, samples, ...) >= stopping@target[1] &
          prob(dose = dose, model, samples, ...) <= stopping@target[2]
      )
    )

    do_stop <- prob_target >= stopping@prob

    msg <- paste(
      "Probability for target toxicity is",
      round(prob_target * 100),
      "% for dose",
      dose,
      "and thus",
      ifelse(do_stop, "above", "below"),
      "the required",
      round(stopping@prob * 100),
      "%"
    )

    structure(
      do_stop,
      message = msg,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingMTDdistribution ----

#' @describeIn stopTrial Stop based on MTD distribution.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingMTDdistribution
#' @example examples/Rules-method-stopTrial-StoppingMTDdistribution.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMTDdistribution",
    dose = "numeric",
    samples = "Samples",
    model = "GeneralModel",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # First, generate the MTD samples.
    # Add prior data and samples to the function environment so that they can
    # be used.
    mtd_samples <- dose(
      x = stopping@target,
      model,
      samples,
      ...
    )

    # What is the absolute threshold?
    abs_thresh <- stopping@thresh * dose

    # What is the probability to be above this dose?
    prob <- ifelse(
      is.na(abs_thresh),
      0,
      mean(mtd_samples > abs_thresh)
    )

    # So can we stop?
    do_stop <- prob >= stopping@prob

    # Generate message.
    text <-
      paste(
        "Probability of MTD above",
        round(stopping@thresh * 100),
        "% of current dose",
        dose,
        "is",
        round(prob * 100),
        "% and thus",
        ifelse(do_stop, "greater than or equal to", "strictly less than"),
        "the required",
        round(stopping@prob * 100),
        "%"
      )

    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingMTDCV ----

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
  f = "stopTrial",
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
      samples,
      ...
    )
    # CV of MTD expressed as percentage, derived based on MTD posterior samples.
    mtd_cv <- (mad(mtd_samples) / median(mtd_samples)) * 100
    do_stop <- mtd_cv <= stopping@thresh_cv

    msg <- paste(
      "CV of MTD is",
      round(mtd_cv),
      "% and thus",
      ifelse(do_stop, "below", "above"),
      "the required precision threshold of",
      round(stopping@thresh_cv),
      "%"
    )

    structure(
      do_stop,
      message = msg,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingLowestDoseHSRBeta ----

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
  f = "stopTrial",
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
        pbeta(
          stopping@target,
          x + stopping@a,
          n - x + stopping@b,
          lower.tail = FALSE
        )
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
        stopping@a,
        ",",
        stopping@b,
        ") prior is ",
        round(tox_prob_first_dose * 100),
        "% and thus ",
        ifelse(do_stop, "above", "below"),
        " the required ",
        round(stopping@prob * 100),
        "% threshold.",
        sep = ""
      )
    }

    structure(
      do_stop,
      message = msg,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingTargetBiomarker ----

#' @describeIn stopTrial Stop based on probability of targeting biomarker
#'
#' @aliases stopTrial-StoppingTargetBiomarker
#' @example examples/Rules-method-stopTrial-StoppingTargetBiomarker.R
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingTargetBiomarker",
    dose = "numeric",
    samples = "Samples",
    model = "DualEndpoint",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    # Compute the target biomarker prob at this dose.
    # Get the biomarker level samples at the dose grid points.
    biom_level_samples <- biomarker(
      xLevel = seq_len(data@nGrid),
      model,
      samples,
      ...
    )

    # If target is relative to maximum.
    if (stopping@is_relative) {
      # If there is an 'Emax' parameter, target biomarker level will
      # be relative to 'Emax', otherwise will be relative to the
      # maximum biomarker level achieved in the given dose range.
      if ("Emax" %in% names(samples)) {
        # For each sample, look which dose is maximizing the
        # simultaneous probability to be in the target biomarker
        # range and below overdose toxicity.
        prob_target <- numeric(ncol(biom_level_samples))
        prob_target <- sapply(
          seq(1, ncol(biom_level_samples)),
          function(x) {
            sum(
              biom_level_samples[, x] >=
                stopping@target[1] * samples@data$Emax &
                biom_level_samples[, x] <=
                  stopping@target[2] * samples@data$Emax
            ) /
              nrow(biom_level_samples)
          }
        )
      } else {
        # For each sample, look which was the minimum dose giving
        # relative target level.
        targetIndex <- apply(
          biom_level_samples,
          1L,
          function(x) {
            rnx <- range(x)
            min(which(
              (x >= stopping@target[1] * diff(rnx) + rnx[1]) &
                (x <= stopping@target[2] * diff(rnx) + rnx[1] + 1e-10)
            ))
          }
        )
        prob_target <- numeric(ncol(biom_level_samples))
        tab <- table(targetIndex)
        prob_target[as.numeric(names(tab))] <- tab
        prob_target <- prob_target / nrow(biom_level_samples)
      }
    } else {
      # Otherwise the target is absolute.
      # For each sample, look which dose is maximizing the
      # simultaneous probability to be in the target biomarker
      # range and below overdose toxicity.
      prob_target <- numeric(ncol(biom_level_samples))
      prob_target <- sapply(
        seq(1, ncol(biom_level_samples)),
        function(x) {
          sum(
            biom_level_samples[, x] >= stopping@target[1] &
              biom_level_samples[, x] <= stopping@target[2]
          ) /
            nrow(biom_level_samples)
        }
      )
    }

    prob_target <- ifelse(
      is.na(dose),
      0,
      prob_target[which(data@doseGrid == dose)]
    )

    do_stop <- prob_target >= stopping@prob

    msg <- paste(
      "Probability for target biomarker is",
      round(prob_target * 100),
      "% for dose",
      dose,
      "and thus",
      ifelse(do_stop, "above", "below"),
      "the required",
      round(stopping@prob * 100),
      "%"
    )

    structure(
      do_stop,
      message = msg,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingSpecificDose ----

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

    attr(result, "report_label") <- stopping@report_label

    result
  }
)

## stopTrial-StoppingHighestDose ----

#' @describeIn stopTrial Stop when the highest dose is reached.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingHighestDose
#' @example examples/Rules-method-stopTrial-StoppingHighestDose.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingHighestDose",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "Data"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    is_highest_dose <- ifelse(
      is.na(dose),
      FALSE,
      (dose == data@doseGrid[data@nGrid])
    )
    structure(
      is_highest_dose,
      message = paste(
        "Next best dose is",
        dose,
        "and thus",
        ifelse(is_highest_dose, "the", "not the"),
        "highest dose"
      ),
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingOrdinal ----

#' @describeIn stopTrial Stop based on value returned by next best dose.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @aliases stopTrial-StoppingOrdinal
#' @example examples/Rules-method-stopTrial-StoppingOrdinal.R
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingOrdinal",
    dose = "numeric",
    samples = "ANY",
    model = "LogisticLogNormalOrdinal",
    data = "DataOrdinal"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    stopTrial(
      stopping = stopping@rule,
      dose = dose,
      samples = h_convert_ordinal_samples(samples, stopping@grade),
      model = h_convert_ordinal_model(model, stopping@grade),
      data = h_convert_ordinal_data(data, stopping@grade),
      ...
    )
  }
)

## stopTrial-StoppingOrdinal ----

#' @describeIn stopTrial Stop based on value returned by next best dose.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @aliases stopTrial-StoppingOrdinal
#' @example examples/Rules-method-stopTrial-StoppingOrdinal.R
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingOrdinal",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, ...) {
    stop(
      paste0(
        "StoppingOrdinal objects can only be used with LogisticLogNormalOrdinal ",
        "models and DataOrdinal data objects. In this case, the model is a '",
        class(model),
        "' object and the data is in a ",
        class(data),
        " object."
      )
    )
  }
)

## stopTrial-StoppingExternal ----

#' @describeIn stopTrial Stop based on an external flag.
#'
#' @description `r lifecycle::badge("experimental")`
#' @param external (`flag`)\cr whether to stop based on the external
#'   result or not.
#'
#' @aliases stopTrial-StoppingExternal
#' @example examples/Rules-method-stopTrial-StoppingExternal.R
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingExternal",
    dose = "numeric",
    samples = "ANY",
    model = "ANY",
    data = "ANY"
  ),
  definition = function(stopping, dose, samples, model, data, external, ...) {
    assert_flag(external)

    msg <- paste(
      "Based on external result",
      ifelse(external, "stop", "continue")
    )

    structure(
      external,
      message = msg,
      report_label = stopping@report_label
    )
  }
)


## stopTrial-StoppingTDCIRatio ----

#' @describeIn stopTrial Stop based on [`StoppingTDCIRatio`] class when
#'   reaching the target ratio of the upper to the lower 95% credibility
#'   interval of the estimate (TDtargetEndOfTrial). This is a stopping rule
#'   which incorporates only DLE responses and DLE samples are given.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingTDCIRatio
#' @example examples/Rules-method-stopTrialCITDsamples.R
#' @export
#'
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
      samples = samples,
      ...
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
      "target_ratio = ",
      stopping@target_ratio
    )
    structure(do_stop, message = text, report_label = stopping@report_label)
  }
)

## stopTrial-StoppingTDCIRatio ----

#' @describeIn stopTrial Stop based on [`StoppingTDCIRatio`] class when
#'   reaching the target ratio of the upper to the lower 95% credibility
#'   interval of the estimate (TDtargetEndOfTrial). This is a stopping rule
#'   which incorporates only DLE responses and no DLE samples are involved.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingTDCIRatio
#' @example examples/Rules-method-stopTrialCITD.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingTDCIRatio",
    dose = "ANY",
    samples = "missing",
    model = "ModelTox",
    data = "ANY"
  ),
  definition = function(stopping, dose, model, data, ...) {
    assert_probability(stopping@prob_target)

    prob_target <- stopping@prob_target
    dose_target_samples <- dose(x = prob_target, model = model, ...)
    # Find the variance of the log of the dose_target_samples (eta).
    m1 <- matrix(
      c(
        -1 / (model@phi2),
        -(log(prob_target / (1 - prob_target)) - model@phi1) / (model@phi2)^2
      ),
      1,
      2
    )
    m2 <- model@Pcov
    var_eta <- as.vector(m1 %*% m2 %*% t(m1))

    # Find the upper and lower limit of the 95% credibility interval.
    ci <- exp(log(dose_target_samples) + c(-1, 1) * 1.96 * sqrt(var_eta))
    ratio <- ci[2] / ci[1]

    # So can we stop?
    do_stop <- ratio <= stopping@target_ratio
    # Generate message.
    text <- paste(
      "95% CI is (",
      round(ci[1], 4),
      ",",
      round(ci[2], 4),
      "), Ratio =",
      round(ratio, 4),
      "is ",
      ifelse(do_stop, "is less than or equal to", "greater than"),
      "target_ratio =",
      stopping@target_ratio
    )
    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

## stopTrial-StoppingMaxGainCIRatio ----

#' @describeIn stopTrial Stop based on reaching the target ratio of the upper
#'   to the lower 95% credibility interval of the estimate (the minimum of
#'   Gstar and TDtargetEndOfTrial). This is a stopping rule which incorporates
#'   DLE and efficacy responses and DLE and efficacy samples are also used.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param TDderive (`function`)\cr the function which derives from the input,
#'   a vector of the posterior samples called `TDsamples` of the dose which has
#'   the probability of the occurrence of DLE equals to either the
#'   targetDuringTrial or targetEndOfTrial, the final next best
#'   TDtargetDuringTrial (the dose with probability of the occurrence of DLE
#'   equals to the targetDuringTrial) and TDtargetEndOfTrial estimate.
#' @param Effmodel (`ModelEff`)\cr the efficacy model.
#' @param Effsamples (`Samples`)\cr the efficacy samples.
#' @param Gstarderive (`function`)\cr the function which derives from the input,
#'   a vector of the posterior Gstar (the dose which gives the maximum gain
#'   value) samples called `Gstarsamples`, the final next best Gstar estimate.
#'
#' @aliases stopTrial-StoppingMaxGainCIRatio
#' @example examples/Rules-method-stopTrialCIMaxGainSamples.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMaxGainCIRatio",
    dose = "ANY",
    samples = "Samples",
    model = "ModelTox",
    data = "DataDual"
  ),
  definition = function(
    stopping,
    dose,
    samples,
    model,
    data,
    TDderive,
    Effmodel,
    Effsamples,
    Gstarderive,
    ...
  ) {
    prob_target <- stopping@prob_target

    # Checks.
    assert_probability(prob_target)
    stopifnot(is(Effmodel, "ModelEff"))
    stopifnot(is(Effsamples, "Samples"))
    stopifnot(is.function(TDderive))
    stopifnot(is.function(Gstarderive))

    # Find the TDtarget End of Trial samples.
    td_target_end_of_trial_samples <- dose(
      x = prob_target,
      model = model,
      samples = samples,
      ...
    )
    # Find the TDtarget End of trial estimate.
    td_target_end_of_trial_estimate <- TDderive(td_target_end_of_trial_samples)

    # Find the gain value samples then the GstarSamples.
    points <- data@doseGrid

    gain_samples <- matrix(
      nrow = size(samples),
      ncol = length(points)
    )

    # Evaluate the probs, for all gain samples.
    for (i in seq_along(points)) {
      # Now we want to evaluate for the following dose.
      gain_samples[, i] <- gain(
        dose = points[i],
        model,
        samples,
        Effmodel,
        Effsamples,
        ...
      )
    }

    # Find the maximum gain value samples.
    max_gain_samples <- apply(gain_samples, 1, max)

    # Obtain Gstar samples, samples for the dose level which gives the maximum
    # gain value.
    index_g <- apply(gain_samples, 1, which.max)
    gstar_samples <- data@doseGrid[index_g]

    # Find the Gstar estimate.
    gstar <- Gstarderive(gstar_samples)
    # Find the 95% credibility interval of Gstar and its ratio of the upper to
    # the lower limit.
    ci_gstar <- quantile(gstar_samples, probs = c(0.025, 0.975))
    ratio_gstar <- as.numeric(ci_gstar[2] / ci_gstar[1])

    # Find the 95% credibility interval of TDtargetEndOfTrial and its ratio of
    # the upper to the lower limit.
    ci_tdeot <- quantile(
      td_target_end_of_trial_samples,
      probs = c(0.025, 0.975)
    )
    ratio_tdeot <- as.numeric(ci_tdeot[2] / ci_tdeot[1])

    # Find which is smaller (TDtargetEndOfTrialEstimate or Gstar).
    if (td_target_end_of_trial_estimate <= gstar) {
      # Find the upper and lower limit of the 95% credibility interval and its
      # ratio of the smaller.
      ci <- ci_tdeot
      ratio <- ratio_tdeot
      choose_td <- TRUE
    } else {
      ci <- ci_gstar
      ratio <- ratio_gstar
      choose_td <- FALSE
    }

    # So can we stop?
    do_stop <- ratio <= stopping@target_ratio
    # Generate message.
    text1 <- paste(
      "Gstar estimate is",
      round(gstar, 4),
      "with 95% CI (",
      round(ci_gstar[1], 4),
      ",",
      round(ci_gstar[2], 4),
      ") and its ratio =",
      round(ratio_gstar, 4)
    )
    text2 <- paste(
      "TDtargetEndOfTrial estimate is ",
      round(td_target_end_of_trial_estimate, 4),
      "with 95% CI (",
      round(ci_tdeot[1], 4),
      ",",
      round(ci_tdeot[2], 4),
      ") and its ratio=",
      round(ratio_tdeot, 4)
    )
    text3 <- paste(
      ifelse(choose_td, "TDtargetEndOfTrial estimate", "Gstar estimate"),
      "is smaller with ratio =",
      round(ratio, 4),
      " which is ",
      ifelse(do_stop, "is less than or equal to", "greater than"),
      "target_ratio =",
      stopping@target_ratio
    )
    text <- c(text1, text2, text3)
    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

#' @describeIn stopTrial Stop based on reaching the target ratio of the upper
#'   to the lower 95% credibility interval of the estimate (the minimum of
#'   Gstar and TDtargetEndOfTrial). This is a stopping rule which incorporates
#'   DLE and efficacy responses without DLE and efficacy samples involved.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @aliases stopTrial-StoppingMaxGainCIRatio
#' @example examples/Rules-method-stopTrialCIMaxGain.R
#' @export
#'
setMethod(
  f = "stopTrial",
  signature = signature(
    stopping = "StoppingMaxGainCIRatio",
    dose = "ANY",
    samples = "missing",
    model = "ModelTox",
    data = "DataDual"
  ),
  definition = function(stopping, dose, model, data, Effmodel, ...) {
    prob_target <- stopping@prob_target

    # Checks.
    assert_probability(prob_target)
    stopifnot(is(Effmodel, "ModelEff"))

    # Find the TDtarget End of Trial.
    td_target_end_of_trial <- dose(
      x = prob_target,
      model = model,
      ...
    )

    # Find the dose with maximum gain value.
    gainfun <- function(dose_val) {
      -gain(dose_val, model_dle = model, model_eff = Effmodel, ...)
    }

    lowest_dose <- min(data@doseGrid)

    gstar <- (optim(
      lowest_dose,
      gainfun,
      method = "L-BFGS-B",
      lower = lowest_dose,
      upper = max(data@doseGrid)
    )$par)
    max_gain <- -(optim(
      lowest_dose,
      gainfun,
      method = "L-BFGS-B",
      lower = lowest_dose,
      upper = max(data@doseGrid)
    )$value)
    if (data@placebo) {
      log_gstar <- log(gstar + Effmodel@const)
    } else {
      log_gstar <- log(gstar)
    }

    # From paper (Yeung et. al 2015).
    mean_eff_gstar <- Effmodel@theta1 + Effmodel@theta2 * log(log_gstar)

    denom <- (model@phi2) * (mean_eff_gstar) * (1 + log_gstar * model@phi2)

    dgphi1 <- -(mean_eff_gstar * log_gstar * model@phi2 - Effmodel@theta2) /
      denom

    dgphi2 <- -((mean_eff_gstar) *
      log_gstar +
      mean_eff_gstar * (log_gstar)^2 * model@phi2 -
      Effmodel@theta2 * log_gstar) /
      denom

    dgtheta1 <- -(log_gstar * model@phi2) / denom

    dgtheta2 <- -(log_gstar *
      exp(model@phi1 + model@phi2 * log_gstar) *
      model@phi2 *
      log(log_gstar) -
      1 -
      exp(model@phi1 + model@phi2 * log_gstar)) /
      denom

    delta_g <- matrix(c(dgphi1, dgphi2, dgtheta1, dgtheta2), 4, 1)

    # Find the variance of the log Gstar.
    # First find the covariance matrix of all the parameters, phi1, phi2,
    # theta1 and theta2 such that phi1 and phi2 are independent of theta1 and
    # theta2.
    empty_matrix <- matrix(0, 2, 2)
    cov_beta <- cbind(
      rbind(model@Pcov, empty_matrix),
      rbind(empty_matrix, Effmodel@Pcov)
    )
    var_log_gstar <- as.vector(t(delta_g) %*% cov_beta %*% delta_g)

    # Find the upper and lower limit of the 95% credibility interval of Gstar.
    ci_gstar <- exp(log_gstar + c(-1, 1) * 1.96 * sqrt(var_log_gstar))

    # The ratio of the upper to the lower 95% credibility interval.
    ratio_gstar <- ci_gstar[2] / ci_gstar[1]

    # Find the variance of the log of the TDtargetEndOfTrial (eta).
    m1 <- matrix(
      c(
        -1 / (model@phi2),
        -(log(prob_target / (1 - prob_target)) - model@phi1) / (model@phi2)^2
      ),
      1,
      2
    )
    m2 <- model@Pcov

    var_eta <- as.vector(m1 %*% m2 %*% t(m1))

    # Find the upper and lower limit of the 95% credibility interval of
    # TDtargetEndOfTrial.
    ci_tdeot <- exp(
      log(td_target_end_of_trial) + c(-1, 1) * 1.96 * sqrt(var_eta)
    )

    # The ratio of the upper to the lower 95% credibility interval.
    ratio_tdeot <- ci_tdeot[2] / ci_tdeot[1]

    if (gstar <= td_target_end_of_trial) {
      choose_td <- FALSE
      ci <- c()
      ci[2] <- ci_gstar[2]
      ci[1] <- ci_gstar[1]
      ratio <- ratio_gstar
    } else {
      choose_td <- TRUE
      ci <- c()
      ci[2] <- ci_tdeot[2]
      ci[1] <- ci_tdeot[1]
      ratio <- ratio_tdeot
    }
    # So can we stop?
    do_stop <- ratio <= stopping@target_ratio
    # Generate message.
    text1 <- paste(
      "Gstar estimate is",
      round(gstar, 4),
      "with 95% CI (",
      round(ci_gstar[1], 4),
      ",",
      round(ci_gstar[2], 4),
      ") and its ratio =",
      round(ratio_gstar, 4)
    )
    text2 <- paste(
      "TDtargetEndOfTrial estimate is ",
      round(td_target_end_of_trial, 4),
      "with 95% CI (",
      round(ci_tdeot[1], 4),
      ",",
      round(ci_tdeot[2], 4),
      ") and its ratio=",
      round(ratio_tdeot, 4)
    )
    text3 <- paste(
      ifelse(choose_td, "TDtargetEndOfTrial estimate", "Gstar estimate"),
      "is smaller with ratio =",
      round(ratio, 4),
      "which is ",
      ifelse(do_stop, "is less than or equal to", "greater than"),
      "target_ratio =",
      stopping@target_ratio
    )
    text <- c(text1, text2, text3)
    # Return both.
    structure(
      do_stop,
      message = text,
      report_label = stopping@report_label
    )
  }
)

# maxSize ----

## generic ----

#' "MAX" Combination of Cohort Size Rules
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function combines cohort size rules by taking the maximum of all sizes.
#'
#' @param ... Objects of class [`CohortSize`].
#'
#' @return The combination as an object of class [`CohortSizeMax`].
#'
#' @seealso [minSize()]
#' @export
#'
setGeneric(
  name = "maxSize",
  def = function(...) {
    # There should be no default method, therefore just forward to next method.
    standardGeneric("maxSize")
  },
  valueClass = "CohortSizeMax"
)

## maxSize-CohortSize ----

#' @describeIn maxSize The method combining cohort size rules by taking maximum.
#'
#' @aliases maxSize-CohortSize
#' @example examples/Rules-method-maxSize.R
#' @export
#'
setMethod(
  f = "maxSize",
  signature = "CohortSize",
  definition = function(...) {
    CohortSizeMax(list(...))
  }
)

# minSize ----

## generic ----

#' "MIN" Combination of Cohort Size Rules
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function combines cohort size rules by taking the minimum of all sizes.
#'
#' @param ... Objects of class [`CohortSize`].
#'
#' @return The combination as an object of class [`CohortSizeMin`].
#'
#' @seealso [maxSize()]
#' @export
#'
setGeneric(
  name = "minSize",
  def = function(...) {
    # There should be no default method, therefore just forward to next method.
    standardGeneric("minSize")
  },
  valueClass = "CohortSizeMin"
)

## minSize-CohortSize ----

#' @describeIn minSize The method combining cohort size rules by taking minimum.
#'
#' @aliases minSize-CohortSize
#' @example examples/Rules-method-minSize.R
#' @export
#'
setMethod(
  f = "minSize",
  signature = "CohortSize",
  definition = function(...) {
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

## size-CohortSizeDLT ----

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
    interval <- findInterval(x = dlt_happened, vec = object@intervals)
    object@cohort_size[interval]
  }
)

## size-CohortSizeMax ----

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
    assert_multi_class(data, c("Data", "DataOrdinal"))

    # Evaluate the individual cohort size rules in the list.
    individual_results <- sapply(
      object@cohort_sizes,
      size,
      dose = dose,
      data = data
    )
    # The overall result.
    max(individual_results)
  }
)

## size-CohortSizeMin ----

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
    assert_multi_class(data, c("Data", "DataOrdinal"))

    # Evaluate the individual cohort size rules in the list.
    individual_results <- sapply(
      object@cohort_sizes,
      size,
      dose = dose,
      data = data
    )
    # The overall result.
    min(individual_results)
  }
)

## size-CohortSizeConst ----

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

## size-CohortSizeParts ----

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
      0L
    } else {
      assert_class(data, "DataParts")
      object@cohort_sizes[data@nextPart]
    }
  }
)

## size-CohortSizeOrdinal ----

#' @describeIn size Determines the size of the next cohort in a ordinal CRM trial.
#'
#' @param dose (`numeric`) the next dose.
#' @param data the data input, an object of class [`DataOrdinal`].
#'
#' @aliases size-CohortSizeOrdinal
#' @example examples/Rules-method-size-CohortSizeOrdinal.R
#'
setMethod(
  f = "size",
  signature = signature(
    object = "CohortSizeOrdinal"
  ),
  definition = function(object, dose, data, ...) {
    # Validate
    assert_numeric(dose, len = 1, lower = 0)
    assert_class(data, "DataOrdinal")
    # Execute

    size(
      object@rule,
      dose = dose,
      data = h_convert_ordinal_data(data, object@grade),
      ...
    )
  }
)

# windowLength ----

## generic ----

#' Determine the Safety Window Length of the Next Cohort
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function determines the safety window length of the next cohort.
#'
#' @param safetyWindow (`SafetyWindow`)\cr the rule, an object of class
#'   [`SafetyWindow`].
#' @param size (`integer`)\cr the next cohort size.
#' @param data (`DataDA`)\cr the data input, an object of class [`DataDA`].
#' @param ... additional arguments without method dispatch.
#'
#' @return The `windowLength` as a list of safety window parameters
#'   (`gap`, `follow`, `follow_min`).
#'
#' @export
#'
setGeneric(
  name = "windowLength",
  def = function(safetyWindow, size, ...) {
    # There should be no default method, therefore just forward to next method.
    standardGeneric("windowLength")
  },
  valueClass = "list"
)

## windowLength-SafetyWindowSize ----

#' @describeIn windowLength Determine safety window length based on the cohort
#'   size.
#'
#' @aliases windowLength-SafetyWindowSize
#' @example examples/Rules-method-windowLength-SafetyWindowSize.R
#' @export
#'
setMethod(
  f = "windowLength",
  signature = signature(
    safetyWindow = "SafetyWindowSize",
    size = "ANY"
  ),
  definition = function(safetyWindow, size, data, ...) {
    # Determine in which interval the next size is.
    interval <-
      findInterval(
        x = size,
        vec = safetyWindow@size
      )

    # So the safety window length is.
    patient_gap <- head(
      c(
        0,
        safetyWindow@gap[[interval]],
        rep(tail(safetyWindow@gap[[interval]], 1), 100)
      ),
      size
    )
    patient_follow <- safetyWindow@follow
    patient_follow_min <- safetyWindow@follow_min

    ret <- list(
      patientGap = patient_gap,
      patientFollow = patient_follow,
      patientFollowMin = patient_follow_min
    )

    ret
  }
)

## windowLength-SafetyWindowConst ----

#' @describeIn windowLength Constant safety window length.
#'
#' @aliases windowLength-SafetyWindowConst
#' @example examples/Rules-method-windowLength-SafetyWindowConst.R
#' @export
#'
setMethod(
  f = "windowLength",
  signature = signature(
    safetyWindow = "SafetyWindowConst",
    size = "ANY"
  ),
  definition = function(safetyWindow, size, ...) {
    # First element should be 0.
    patient_gap <- head(
      c(
        0,
        safetyWindow@gap,
        rep(tail(safetyWindow@gap, 1), 100)
      ),
      size
    )
    patient_follow <- safetyWindow@follow
    patient_follow_min <- safetyWindow@follow_min

    ret <- list(
      patientGap = patient_gap,
      patientFollow = patient_follow,
      patientFollowMin = patient_follow_min
    )

    ret
  }
)

# tidy ----

## tidy-IncrementsRelative ----

#' @rdname tidy
#' @aliases tidy-IncrementsRelative
#' @example examples/Rules-method-tidyIncrementsRelative.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsRelative"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      dplyr::bind_cols() %>%
      h_range_to_minmax(.data$intervals) %>%
      dplyr::filter(max > 0) %>%
      tibble::add_column(increment = x@increments) %>%
      h_tidy_class(x)
  }
)

## tidy-CohortSizeDLT ----

#' @rdname tidy
#' @aliases tidy-CohortSizeDLT
#' @example examples/Rules-method-tidyCohortSizeDLT.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CohortSizeDLT"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      dplyr::bind_cols() %>%
      h_range_to_minmax(.data$intervals) %>%
      dplyr::filter(max > 0) %>%
      tibble::add_column(cohort_size = x@cohort_size) %>%
      h_tidy_class(x)
  }
)

## tidy-CohortSizeMin ----

#' @rdname tidy
#' @aliases tidy-CohortSizeMin
#' @example examples/Rules-method-tidyCohortSizeMin.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CohortSizeMin"),
  definition = function(x, ...) {
    callNextMethod() %>% h_tidy_class(x)
  }
)

## tidy-CohortSizeMax ----

#' @rdname tidy
#' @aliases tidy-CohortSizeMax
#' @example examples/Rules-method-tidyCohortSizeMax.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CohortSizeMax"),
  definition = function(x, ...) {
    callNextMethod() %>% h_tidy_class(x)
  }
)

## tidy-CohortSizeRange ----

#' @rdname tidy
#' @aliases tidy-CohortSizeRange
#' @example examples/Rules-method-tidyCohortSizeRange.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CohortSizeRange"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      dplyr::bind_cols() %>%
      h_range_to_minmax(.data$intervals) %>%
      dplyr::filter(max > 0) %>%
      tibble::add_column(cohort_size = x@cohort_size) %>%
      h_tidy_class(x)
  }
)

## tidy-CohortSizeParts ----

#' @rdname tidy
#' @aliases tidy-CohortSizeParts
#' @example examples/Rules-method-tidyCohortSizeParts.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CohortSizeParts"),
  definition = function(x, ...) {
    tibble::tibble(
      part = seq_along(x@cohort_sizes),
      cohort_size = x@cohort_sizes
    ) %>%
      h_tidy_class(x)
  }
)

## tidy-IncrementsMin ----

#' @rdname tidy
#' @aliases tidy-IncrementsMin
#' @example examples/Rules-method-tidyIncrementsMin.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsMin"),
  definition = function(x, ...) {
    callNextMethod() %>% h_tidy_class(x)
  }
)

## tidy-IncrementsRelative ----

#' @rdname tidy
#' @aliases tidy-IncrementsRelative
#' @example examples/Rules-method-tidyIncrementsRelative.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsRelative"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      h_range_to_minmax(.data$intervals) %>%
      dplyr::filter(dplyr::row_number() > 1) %>%
      tibble::add_column(increment = x@increments) %>%
      h_tidy_class(x)
  }
)

## tidy-IncrementsRelativeDLT ----

#' @rdname tidy
#' @aliases tidy-IncrementsRelativeDLT
#' @example examples/Rules-method-tidyIncrementsRelativeDLT.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsRelativeDLT"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      h_range_to_minmax(.data$intervals) %>%
      dplyr::filter(dplyr::row_number() > 1) %>%
      tibble::add_column(increment = x@increments) %>%
      h_tidy_class(x)
  }
)

## tidy-IncrementsRelative ----

#' @rdname tidy
#' @aliases tidy-IncrementsRelativeParts
#' @example examples/Rules-method-tidyIncrementsRelativeParts.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "IncrementsRelativeParts"),
  definition = function(x, ...) {
    slot_names <- slotNames(x)
    rv <- list()
    for (nm in slot_names) {
      if (!is.function(slot(x, nm))) {
        rv[[nm]] <- h_tidy_slot(x, nm, ...)
      }
    }
    # Column bind of all list elements have the same number of rows.
    if (length(rv) > 1 & length(unique(sapply(rv, nrow))) == 1) {
      rv <- rv %>% dplyr::bind_cols()
    }
    rv <- rv %>% h_tidy_class(x)
    if (length(rv) == 1) {
      rv[[names(rv)[1]]] %>% h_tidy_class(x)
    } else {
      rv
    }
  }
)

## tidy-NextBestNCRM ----

#' @rdname tidy
#' @aliases tidy-NextBestNCRM
#' @example examples/Rules-method-tidyNextBestNCRM.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "NextBestNCRM"),
  definition = function(x, ...) {
    h_tidy_all_slots(x) %>%
      dplyr::bind_cols() %>%
      h_range_to_minmax(.data$target, range_min = 0, range_max = 1) %>%
      add_column(max_prob = c(NA, NA, x@max_overdose_prob)) %>%
      add_column(Range = c("Underdose", "Target", "Overdose"), .before = 1) %>%
      h_tidy_class(x)
  }
)

## tidy-NextBestNCRMLoss ----

#' @rdname tidy
#' @aliases tidy-NextBestNCRMLoss
#' @example examples/Rules-method-tidyNextBestNCRMLoss.R
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "NextBestNCRMLoss"),
  definition = function(x, ...) {
    tibble(
      Range = "Underdose",
      Lower = 0,
      Upper = x@target[1]
    ) %>%
      dplyr::bind_rows(
        lapply(
          c("target", "overdose", "unacceptable"),
          function(nm, obj) {
            tibble::tibble(
              Range = stringr::str_to_sentence(nm),
              Lower = slot(obj, nm)[1],
              Upper = slot(obj, nm)[2]
            )
          },
          obj = x
        ) %>%
          dplyr::bind_rows()
      ) %>%
      add_column(LossCoefficient = x@losses) %>%
      add_column(MaxOverdoseProb = x@max_overdose_prob) %>%
      h_tidy_class(x)
  }
)
