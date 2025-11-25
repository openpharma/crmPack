#' @include Simulations-class.R
#' @include helpers.R
NULL

# h_plot_simulation_trajectory ----

#' Helper Function to Create Trajectory Plot
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Creates a trajectory plot showing dose level statistics across patients.
#'
#' @param sim_doses (`list`)\cr list of simulated doses per trial.
#' @param max_patients (`integer`)\cr maximum number of patients.
#' @param has_placebo (`flag`)\cr whether the design includes placebo.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
h_plot_simulation_trajectory <- function(sim_doses, max_patients, has_placebo) {
  # Create matrix of simulated dose trajectories.
  sim_doses_mat <- matrix(
    data = NA,
    nrow = length(sim_doses),
    ncol = max_patients
  )

  for (i in seq_along(sim_doses)) {
    sim_doses_mat[i, seq_along(sim_doses[[i]])] <- sim_doses[[i]]
  }

  # Extract statistics.
  stats <- c(
    "Minimum",
    "Lower Quartile",
    "Median",
    "Upper Quartile",
    "Maximum"
  )
  traj_df <- data.frame(
    patient = rep(seq_len(max_patients), each = 5L),
    Statistic = factor(
      rep(stats, max_patients),
      levels = stats
    ),
    traj = c(apply(sim_doses_mat, 2L, quantile, na.rm = TRUE))
  )

  # Create plot title.
  my_title <- if (has_placebo) {
    "Patient (placebo were excluded)"
  } else {
    "Patient"
  }

  # Create and return plot.
  ggplot() +
    geom_step(
      aes(
        x = patient,
        y = traj,
        group = Statistic,
        linetype = Statistic
      ),
      linewidth = 1.2,
      colour = "blue",
      data = traj_df
    ) +
    xlab(my_title) +
    ylab("Dose Level")
}

# h_plot_doses_tried ----

#' Helper Function to Create Doses Tried Plot
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Creates a bar plot showing average proportions of doses tested.
#'
#' @param sim_doses (`list`)\cr list of simulated doses per trial.
#' @param dose_grid (`numeric`)\cr dose grid.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
h_plot_doses_tried <- function(sim_doses, dose_grid) {
  # Get the dose distributions by trial.
  dose_distributions <- sapply(
    sim_doses,
    function(s) {
      if (length(s) > 0) {
        prop.table(table(factor(s, levels = dose_grid)))
      } else {
        rep(0, length(dose_grid))
      }
    }
  )

  # Derive the average dose distribution across trial simulations.
  average_dose_dist <- rowMeans(dose_distributions)

  # Get in data frame shape.
  dat <- data.frame(
    dose = as.numeric(names(average_dose_dist)),
    perc = average_dose_dist * 100
  )

  # Create and return plot.
  ggplot() +
    geom_bar(
      data = dat,
      aes(x = dose, y = perc),
      stat = "identity",
      position = "identity",
      width = min(diff(dose_grid)) / 2
    ) +
    xlab("Dose level") +
    ylab("Average proportion [%]")
}

# plot-GeneralSimulations ----

#' Plot Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the simulations with plots.
#'
#' This plot method can be applied to [`GeneralSimulations`] objects in order
#' to summarize them graphically. Possible `type`s of plots at the moment are:
#' \describe{
#'   \item{trajectory}{Summary of the trajectory of the simulated trials}
#'   \item{dosesTried}{Average proportions of the doses tested in patients}
#' }
#' You can specify one or both of these in the `type` argument.
#'
#' @param x (`GeneralSimulations`)\cr the object we want to plot from.
#' @param y (`missing`)\cr not used.
#' @param type (`character`)\cr the type of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot` object if a single plot is
#'   asked for, otherwise a `gtable` object.
#'
#' @aliases plot-GeneralSimulations-missing
#' @example examples/Simulations-method-plotSIMsingle.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "GeneralSimulations",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c("trajectory", "dosesTried"),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Summary of the trajectories.
    if ("trajectory" %in% type) {
      # If design with placebo, then exclude placebo patients.
      if (x@data[[1]]@placebo) {
        pl <- x@data[[1]]@doseGrid[1]
        sim_doses <- lapply(
          x@data,
          function(y) {
            y@x[y@x != pl]
          }
        )
      } else {
        sim_doses <- lapply(
          x@data,
          slot,
          "x"
        )
      }

      max_patients <- max(sapply(sim_doses, length))

      # Create trajectory plot.
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_plot_simulation_trajectory(
          sim_doses = sim_doses,
          max_patients = max_patients,
          has_placebo = x@data[[1]]@placebo
        )
    }

    # Average distribution of the doses tried.
    if ("dosesTried" %in% type) {
      # Get the doses tried.
      sim_doses <- lapply(
        x@data,
        slot,
        "x"
      )

      # Create doses tried plot.
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_plot_doses_tried(
          sim_doses = sim_doses,
          dose_grid = x@data[[1]]@doseGrid
        )
    }

    # Return plot(s).
    if (identical(length(plot_list), 1L)) {
      # Just return single plot.
      plot_list[[1L]]
    } else {
      # Otherwise arrange them.
      do.call(gridExtra::arrangeGrob, plot_list)
    }
  }
)

# plot-DualSimulations ----

#' Plot Dual-Endpoint Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This plot method can be applied to [`DualSimulations`] objects in order to
#' summarize them graphically. In addition to the standard plot types, there is:
#' \describe{
#'   \item{sigma2W}{Plot a boxplot of the final biomarker variance estimates in
#'     the simulated trials}
#'   \item{rho}{Plot a boxplot of the final correlation estimates in the
#'     simulated trials}
#' }
#'
#' @param x (`DualSimulations`)\cr the object we want to plot from.
#' @param y (`missing`)\cr not used.
#' @param type (`character`)\cr the type of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot` object if a single plot is asked for,
#'   otherwise a `gtable` object.
#'
#' @aliases plot-DualSimulations-missing
#' @example examples/Simulations-method-plot-DualSimulations.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "DualSimulations",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c("trajectory", "dosesTried", "sigma2W", "rho"),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Subtract the specific plot types for dual-endpoint simulation results.
    type_reduced <- setdiff(type, c("sigma2W", "rho"))

    # Are there more plots from general?
    more_from_general <- (length(type_reduced) > 0)

    # If so, then produce these plots.
    if (more_from_general) {
      gen_plot <- callNextMethod(x = x, y = y, type = type_reduced)
    }

    # Now to the specific dual-endpoint plots.

    # Biomarker variance estimates boxplot.
    if ("sigma2W" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        ggplot(
          data = data.frame(y = x@sigma2w_est),
          aes(x = factor(0), y = y)
        ) +
        geom_boxplot() +
        coord_flip() +
        scale_x_discrete(breaks = NULL) +
        xlab("") +
        ylab("Biomarker variance estimates")
    }

    # Correlation estimates boxplot.
    if ("rho" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        ggplot(
          data = data.frame(y = x@rho_est),
          aes(x = factor(0), y = y)
        ) +
        geom_boxplot() +
        coord_flip() +
        scale_x_discrete(breaks = NULL) +
        xlab("") +
        ylab("Correlation estimates")
    }

    # Return plot(s).
    if (identical(length(plot_list), 0L)) {
      gen_plot
    } else if (identical(length(plot_list), 1L)) {
      if (more_from_general) {
        gridExtra::arrangeGrob(gen_plot, plot_list[[1L]])
      } else {
        plot_list[[1L]]
      }
    } else {
      ret <- do.call(gridExtra::arrangeGrob, plot_list)
      if (more_from_general) {
        gridExtra::arrangeGrob(gen_plot, ret)
      } else {
        ret
      }
    }
  }
)

# summary-GeneralSimulations ----

#' Summarize the Simulations, Relative to a Given Truth
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize simulations relative to a given true dose-toxicity curve.
#'
#' @param object (`GeneralSimulations`)\cr the object we want to summarize.
#' @param truth (`function`)\cr a function which takes as input a dose (vector)
#'   and returns the true probability (vector) for toxicity.
#' @param target (`numeric`)\cr the target toxicity interval (default: 20-35%)
#'   used for the computations.
#' @param ... additional arguments can be supplied here for `truth`.
#'
#' @return An object of class [`GeneralSimulationsSummary`].
#'
#' @aliases summary-GeneralSimulations
#' @export
#'
setMethod(
  f = "summary",
  signature = signature(object = "GeneralSimulations"),
  def = function(object, truth, target = c(0.2, 0.35), ...) {
    # Validate arguments.
    assert_function(truth)
    assert_numeric(target, len = 2, lower = 0, upper = 1)
    assert_true(target[1] < target[2])

    # Extract dose grid.
    dose_grid <- object@data[[1]]@doseGrid

    # Evaluate true toxicity at dose grid.
    true_tox <- truth(dose_grid, ...)

    # Find dose interval corresponding to target tox interval.
    target_dose_interval <- sapply(
      target,
      function(t) {
        # We have to be careful because it might be that in the range of the
        # dose grid, no doses can be found that match the target interval
        # boundaries! In that case we want to return NA.
        r <- try(
          uniroot(
            f = function(x) {
              truth(x, ...) - t
            },
            interval = range(dose_grid)
          )$root,
          silent = TRUE
        )
        if (inherits(r, "try-error")) {
          NA_real_
        } else {
          r
        }
      }
    )

    # What are the levels above target interval?
    x_above_target <- which(true_tox > target[2])

    # Proportion of DLTs in a trial.
    if (object@data[[1]]@placebo) {
      if (sum(object@data[[1]]@x == dose_grid[1])) {
        prop_dlts <- sapply(
          object@data,
          function(d) {
            tapply(
              d@y,
              factor(d@x == d@doseGrid[1], labels = c("ACTV", "PLCB")),
              mean
            )
          }
        )
      } else {
        prop_dlts <- sapply(
          object@data,
          function(d) {
            c("ACTV" = mean(d@y), "PLCB" = NA)
          }
        )
      }
    } else {
      prop_dlts <- sapply(
        object@data,
        function(d) {
          mean(d@y)
        }
      )
    }

    # Mean toxicity risk.
    if (object@data[[1]]@placebo) {
      mean_tox_risk <- sapply(
        object@data,
        function(d) {
          mean(true_tox[d@xLevel[d@xLevel != 1]])
        }
      )
    } else {
      mean_tox_risk <- sapply(
        object@data,
        function(d) {
          mean(true_tox[d@xLevel])
        }
      )
    }

    # Doses selected for MTD.
    dose_selected <- object@doses

    # Replace NA by 0.
    dose_selected[is.na(dose_selected)] <- 0

    # Dose most often selected as MTD.
    dose_most_selected <- as.numeric(names(which.max(table(dose_selected))))
    x_most_selected <- match_within_tolerance(
      dose_most_selected,
      table = dose_grid
    )

    # Observed toxicity rate at dose most often selected.
    # Note: this does not seem very useful!
    # Reason: In case of a fine grid, few patients if any will have been
    # treated at this dose.
    tmp <- sapply(
      object@data,
      function(d) {
        which_at_this_dose <- which(d@x == dose_most_selected)
        n_at_this_dose <- length(which_at_this_dose)
        n_dlt_at_this_dose <- sum(d@y[which_at_this_dose])
        c(
          nAtThisDose = n_at_this_dose,
          nDLTatThisDose = n_dlt_at_this_dose
        )
      }
    )

    obs_tox_rate_at_dose_most_selected <-
      mean(tmp["nDLTatThisDose", ]) / mean(tmp["nAtThisDose", ])

    # Number of patients overall.
    if (object@data[[1]]@placebo) {
      n_obs <- sapply(
        object@data,
        function(x) {
          data.frame(
            n.ACTV = sum(x@xLevel != 1L),
            n.PLCB = sum(x@xLevel == 1L)
          )
        }
      )
      n_obs <- matrix(unlist(n_obs), dim(n_obs))
    } else {
      n_obs <- sapply(
        object@data,
        slot,
        "nObs"
      )
    }

    # Number of patients treated above target tox interval.
    n_above_target <- sapply(
      object@data,
      function(d) {
        sum(d@xLevel %in% x_above_target)
      }
    )

    # Proportion of trials selecting target MTD.
    tox_at_doses <- truth(dose_selected, ...)
    prop_at_target <- mean(
      (tox_at_doses > target[1]) & (tox_at_doses < target[2])
    )

    # Give back an object of class GeneralSimulationsSummary.
    .GeneralSimulationsSummary(
      target = target,
      target_dose_interval = target_dose_interval,
      nsim = length(object@data),
      prop_dlts = prop_dlts,
      mean_tox_risk = mean_tox_risk,
      dose_selected = dose_selected,
      dose_most_selected = dose_most_selected,
      obs_tox_rate_at_dose_most_selected = obs_tox_rate_at_dose_most_selected,
      n_obs = n_obs,
      n_above_target = n_above_target,
      tox_at_doses_selected = tox_at_doses,
      prop_at_target = prop_at_target,
      dose_grid = dose_grid,
      placebo = object@data[[1]]@placebo
    )
  }
)

# summary-Simulations ----

#' Summarize Model-Based Design Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the model-based design simulations, relative to a given truth.
#'
#' @param object (`Simulations`)\cr the object we want to summarize.
#' @param truth (`function`)\cr a function which takes as input a dose (vector)
#'   and returns the true probability (vector) for toxicity.
#' @param target (`numeric`)\cr the target toxicity interval (default: 20-35%)
#'   used for the computations.
#' @param ... additional arguments can be supplied here for `truth`.
#'
#' @return An object of class [`SimulationsSummary`].
#'
#' @aliases summary-Simulations
#' @example examples/Simulations-method-summary.R
#' @export
#'
setMethod(
  f = "summary",
  signature = signature(object = "Simulations"),
  def = function(object, truth, target = c(0.2, 0.35), ...) {
    # Call the parent method.
    start <- callNextMethod(
      object = object,
      truth = truth,
      target = target,
      ...
    )

    dose_grid <- object@data[[1]]@doseGrid

    # Dose level most often selected as MTD.
    x_most_selected <-
      match_within_tolerance(start@dose_most_selected, table = dose_grid)

    # Fitted toxicity rate at dose most often selected.
    fit_at_dose_most_selected <-
      sapply(
        object@fit,
        function(f) {
          f$middle[x_most_selected]
        }
      )

    # Mean fitted toxicity (average, lower and upper quantiles)
    # at each dose level (this is required for plotting).
    mean_fit_matrix <- sapply(
      object@fit,
      "[[",
      "middle"
    )
    mean_fit <- list(
      truth = truth(dose_grid, ...),
      average = rowMeans(mean_fit_matrix),
      lower = apply(
        mean_fit_matrix,
        1L,
        quantile,
        0.025
      ),
      upper = apply(
        mean_fit_matrix,
        1L,
        quantile,
        0.975
      )
    )

    # Give back an object of class SimulationsSummary.
    .SimulationsSummary(
      start,
      stop_report = object@stop_report,
      additional_stats = object@additional_stats,
      fit_at_dose_most_selected = fit_at_dose_most_selected,
      mean_fit = mean_fit
    )
  }
)

# summary-DualSimulations ----

#' Summarize Dual-Endpoint Design Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the dual-endpoint design simulations, relative to given true
#' dose-toxicity and dose-biomarker curves.
#'
#' @param object (`DualSimulations`)\cr the object we want to summarize.
#' @param trueTox (`function`)\cr a function which takes as input a dose
#'   (vector) and returns the true probability (vector) for toxicity.
#' @param trueBiomarker (`function`)\cr a function which takes as input a dose
#'   (vector) and returns the true biomarker level (vector).
#' @param target (`numeric`)\cr the target toxicity interval (default: 20-35%)
#'   used for the computations.
#' @param ... additional arguments can be supplied here for `trueTox` and
#'   `trueBiomarker`.
#'
#' @return An object of class [`DualSimulationsSummary`].
#'
#' @aliases summary-DualSimulations
#' @example examples/Simulations-method-summary-DualSimulations.R
#' @export
#'
setMethod(
  f = "summary",
  signature = signature(object = "DualSimulations"),
  def = function(object, trueTox, trueBiomarker, target = c(0.2, 0.35), ...) {
    # Call the parent method.
    start <- callNextMethod(
      object = object,
      truth = trueTox,
      target = target,
      ...
    )

    dose_grid <- object@data[[1]]@doseGrid

    # Dose level most often selected as MTD.
    x_most_selected <-
      match_within_tolerance(start@dose_most_selected, table = dose_grid)

    # Fitted biomarker level at dose most often selected.
    biomarker_fit_at_dose_most_selected <-
      sapply(
        object@fit_biomarker,
        function(f) {
          f$middleBiomarker[x_most_selected]
        }
      )

    # Mean fitted biomarker curve (average, lower and upper quantiles)
    # at each dose level (this is required for plotting).
    mean_biomarker_fit_matrix <- sapply(
      object@fit_biomarker,
      "[[",
      "middleBiomarker"
    )
    mean_biomarker_fit <- list(
      truth = trueBiomarker(dose_grid, ...),
      average = rowMeans(mean_biomarker_fit_matrix),
      lower = apply(
        mean_biomarker_fit_matrix,
        1L,
        quantile,
        0.025
      ),
      upper = apply(
        mean_biomarker_fit_matrix,
        1L,
        quantile,
        0.975
      )
    )

    # Give back an object of class DualSimulationsSummary.
    .DualSimulationsSummary(
      start,
      biomarker_fit_at_dose_most_selected = biomarker_fit_at_dose_most_selected,
      mean_biomarker_fit = mean_biomarker_fit
    )
  }
)

# Report-class ----

#' @name Report
#' @field object The object from which to report
#' @field df the data frame to which columns are sequentially added
#' @field dfNames the names to which strings are sequentially added
Report <-
  setRefClass(
    "Report",
    fields = list(
      object = "ANY",
      df = "data.frame",
      dfNames = "character"
    ),
    methods = list(
      dfSave = function(res, name) {
        df <<- cbind(df, res)
        dfNames <<- c(dfNames, name)
        return(res)
      },
      report = function(
        slotName,
        description,
        percent = TRUE,
        digits = 0,
        quantiles = c(0.1, 0.9),
        subset = NULL,
        doSum = FALSE
      ) {
        vals <- slot(object, name = slotName)
        if (!is.null(subset)) {
          vals <- vals[subset, ]
        }
        if (doSum) {
          vals <- apply(vals, 2, sum)
        }
        if (percent) {
          unit <- " %"
          vals <- vals * 100
        } else {
          unit <- ""
        }

        res <- paste(
          round(mean(vals), digits),
          unit,
          " (",
          paste(
            round(
              quantile(vals, quantiles, na.rm = TRUE),
              digits
            ),
            unit,
            collapse = ", ",
            sep = ""
          ),
          ")",
          sep = ""
        )

        # Print result to the buffer.
        cat(
          description,
          ":",
          "mean",
          dfSave(res, slotName),
          "\n"
        )
      }
    )
  )

# show-GeneralSimulationsSummary ----

#' Show the Summary of the Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Display a summary of general simulation results.
#'
#' @param object (`GeneralSimulationsSummary`)\cr the object we want to print.
#'
#' @return Invisibly returns a data frame of the results with one row and
#'   appropriate column names.
#'
#' @aliases show-GeneralSimulationsSummary
#' @export
#'
setMethod(
  f = "show",
  signature = signature(object = "GeneralSimulationsSummary"),
  def = function(object) {
    r <- Report$new(
      object = object,
      df = as.data.frame(matrix(
        nrow = 1,
        ncol = 0
      )),
      dfNames = character()
    )

    cat(
      "Summary of",
      r$dfSave(object@nsim, "nsim"),
      "simulations\n\n"
    )

    cat(
      "Target toxicity interval was",
      r$dfSave(
        paste(round(object@target * 100), collapse = ", "),
        "target"
      ),
      "%\n"
    )
    cat(
      "Target dose interval corresponding to this was",
      r$dfSave(
        paste(round(object@target_dose_interval, 1), collapse = ", "),
        "target_dose_interval"
      ),
      "\n"
    )
    cat(
      "Intervals are corresponding to",
      "10 and 90 % quantiles\n\n"
    )

    if (object@placebo) {
      r$report(
        "n_obs",
        "Number of patients on placebo",
        percent = FALSE,
        subset = 2
      )
      r$report(
        "n_obs",
        "Number of patients on active",
        percent = FALSE,
        subset = 1
      )
      r$report(
        "n_obs",
        "Number of patients overall",
        percent = FALSE,
        doSum = TRUE
      )
    } else {
      r$report("n_obs", "Number of patients overall", percent = FALSE)
    }
    r$report(
      "n_above_target",
      "Number of patients treated above target tox interval",
      percent = FALSE
    )

    if (object@placebo) {
      r$report(
        "prop_dlts",
        "Proportions of DLTs in the trials for patients on placebo",
        subset = 2
      )
      r$report(
        "prop_dlts",
        "Proportions of DLTs in the trials for patients on active",
        subset = 1
      )
    } else {
      r$report(
        "prop_dlts",
        "Proportions of DLTs in the trials"
      )
    }
    r$report(
      "mean_tox_risk",
      "Mean toxicity risks for the patients on active"
    )
    r$report(
      "dose_selected",
      "Doses selected as MTD",
      percent = FALSE,
      digits = 1
    )
    r$report(
      "tox_at_doses_selected",
      "True toxicity at doses selected"
    )
    cat(
      "Proportion of trials selecting target MTD:",
      r$dfSave(
        object@prop_at_target * 100,
        "prop_at_target"
      ),
      "%\n"
    )
    cat(
      "Dose most often selected as MTD:",
      r$dfSave(
        object@dose_most_selected,
        "dose_most_selected"
      ),
      "\n"
    )
    cat(
      "Observed toxicity rate at dose most often selected:",
      r$dfSave(
        round(object@obs_tox_rate_at_dose_most_selected * 100),
        "obs_tox_rate_at_dose_most_selected"
      ),
      "%\n"
    )

    # Finally assign names to the df and return it invisibly.
    names(r$df) <- r$dfNames
    invisible(r$df)
  }
)

# show-SimulationsSummary ----

#' Show the Summary of Model-Based Design Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Display a summary of model-based design simulation results.
#'
#' @param object (`SimulationsSummary`)\cr the object we want to print.
#'
#' @return Invisibly returns a data frame of the results with one row and
#'   appropriate column names.
#'
#' @aliases show-SimulationsSummary
#' @example examples/Simulations-method-show-SimulationsSummary.R
#' @export
#'
setMethod(
  f = "show",
  signature = signature(object = "SimulationsSummary"),
  def = function(object) {
    # Call the parent method.
    df <- callNextMethod(object)
    df_names <- names(df)

    # Start report object.
    r <- Report$new(
      object = object,
      df = df,
      dfNames = df_names
    )

    # Add one reporting line.
    r$report(
      "fit_at_dose_most_selected",
      "Fitted toxicity rate at dose most often selected"
    )

    # Report results of additional statistics summary.
    if (length(unlist(object@additional_stats)) > 0) {
      param_names <- h_summarize_add_stats(
        stats_list = object@additional_stats
      )[[1]]
      averages <- h_summarize_add_stats(stats_list = object@additional_stats)[[
        2
      ]]

      for (i in seq_along(param_names)) {
        cat(param_names[i], ":", round(averages[[i]], 2), "\n")
      }
    }

    # Report individual stopping rules with non-NA labels.
    stop_pct_to_print <- h_calc_report_label_percentage(object@stop_report)

    if (length(stop_pct_to_print) > 0) {
      cat(
        "Stop reason triggered:\n",
        paste(
          names(stop_pct_to_print),
          ": ",
          round(stop_pct_to_print, 2),
          "%\n"
        )
      )
    }

    # And return the updated information.
    names(r$df) <- r$dfNames
    invisible(r$df)
  }
)

# show-DualSimulationsSummary ----

#' Show the Summary of Dual-Endpoint Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Display a summary of dual-endpoint simulation results.
#'
#' @param object (`DualSimulationsSummary`)\cr the object we want to print.
#'
#' @return Invisibly returns a data frame of the results with one row and
#'   appropriate column names.
#'
#' @aliases show-DualSimulationsSummary
#' @example examples/Simulations-method-show-DualSimulationsSummary.R
#' @export
#'
setMethod(
  f = "show",
  signature = signature(object = "DualSimulationsSummary"),
  def = function(object) {
    # Call the parent method.
    df <- callNextMethod(object)
    df_names <- names(df)

    # Start report object.
    r <- Report$new(
      object = object,
      df = df,
      dfNames = df_names
    )

    # Add one reporting line.
    r$report(
      "biomarker_fit_at_dose_most_selected",
      "Fitted biomarker level at dose most often selected",
      percent = FALSE,
      digits = 1
    )

    # And return the updated information.
    names(r$df) <- r$dfNames
    invisible(r$df)
  }
)

# plot-GeneralSimulationsSummary-missing ----

#' Plot General Simulation Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Graphical display of the general simulation summary.
#'
#' This plot method can be applied to [`GeneralSimulationsSummary`] objects in
#' order to summarize them graphically. Possible `type`s of plots at the moment
#' are:
#' \describe{
#'   \item{nObs}{Distribution of the number of patients in the simulated trials}
#'   \item{doseSelected}{Distribution of the final selected doses in the trials.
#'     Note that this can include zero entries, meaning that the trial was
#'     stopped because all doses in the dose grid appeared too toxic.}
#'   \item{propDLTs}{Distribution of the proportion of patients with DLTs in the
#'     trials}
#'   \item{nAboveTarget}{Distribution of the number of patients treated at doses
#'     which are above the target toxicity interval (as specified by the
#'     `truth` and `target` arguments to [`summary,GeneralSimulations-method`])}
#' }
#' You can specify any subset of these in the `type` argument.
#'
#' @param x (`GeneralSimulationsSummary`)\cr the object we want to plot from.
#' @param y (`missing`)\cr not used.
#' @param type (`character`)\cr the types of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot` object if a single plot is
#'   asked for, otherwise a `gtable` object.
#'
#' @aliases plot-GeneralSimulationsSummary-missing
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "GeneralSimulationsSummary",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "nObs",
      "doseSelected",
      "propDLTs",
      "nAboveTarget"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Distribution of overall sample size.
    if (x@placebo) {
      if ("nObs" %in% type) {
        plot_list[[plot_index <- plot_index + 1L]] <-
          h_barplot_percentages(
            x = x@n_obs[2, ],
            description = "Number of patients on active in total"
          )
      }
    } else {
      if ("nObs" %in% type) {
        plot_list[[plot_index <- plot_index + 1L]] <-
          h_barplot_percentages(
            x = x@n_obs,
            description = "Number of patients in total"
          )
      }
    }

    # Distribution of final MTD estimate.
    if ("doseSelected" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@dose_selected,
          description = "MTD estimate"
        )
    }

    # Distribution of proportion of DLTs.
    if (x@placebo) {
      if ("propDLTs" %in% type) {
        plot_list[[plot_index <- plot_index + 1L]] <-
          h_barplot_percentages(
            x = x@prop_dlts[1, ] * 100,
            description = "Proportion of DLTs [%] on active",
            xaxisround = 1
          )
      }
    } else {
      if ("propDLTs" %in% type) {
        plot_list[[plot_index <- plot_index + 1L]] <-
          h_barplot_percentages(
            x = x@prop_dlts * 100,
            description = "Proportion of DLTs [%]",
            xaxisround = 1
          )
      }
    }

    # Distribution of number of patients treated at too much tox.
    if ("nAboveTarget" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@n_above_target,
          description = "Number of patients above target"
        )
    }

    # First combine these small plots.
    if (length(plot_list)) {
      ret <-
        # If there is only one plot.
        if (identical(length(plot_list), 1L)) {
          # Just use that.
          plot_list[[1L]]
        } else {
          # Multiple plots in this case.
          do.call(
            gridExtra::arrangeGrob,
            plot_list
          )
        }
    }

    # Then return.
    ret
  }
)

# plot-SimulationsSummary ----

#' Plot Model-Based Design Simulation Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Graphical display of the simulation summary.
#'
#' This plot method can be applied to [`SimulationsSummary`] objects in order
#' to summarize them graphically. Possible `type` of plots at the moment are
#' those listed in [`plot,GeneralSimulationsSummary,missing-method`] plus:
#'
#' \describe{
#'   \item{meanFit}{Plot showing the average fitted dose-toxicity curve across
#'     the trials, together with 95% credible intervals, and comparison with the
#'     assumed truth (as specified by the `truth` argument to
#'     [`summary,Simulations-method`])}
#' }
#'
#' You can specify any subset of these in the `type` argument.
#'
#' @param x (`SimulationsSummary`)\cr the object we want to plot from.
#' @param y (`missing`)\cr not used.
#' @param type (`character`)\cr the types of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot` object if a single plot is
#'   asked for, otherwise a `gtable` object.
#'
#' @aliases plot-SimulationsSummary-missing
#' @example examples/Simulations-method-plot-SimulationsSummary.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "SimulationsSummary",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "nObs",
      "doseSelected",
      "propDLTs",
      "nAboveTarget",
      "meanFit"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Subtract the specific plot types for model-based designs.
    type_reduced <- setdiff(
      type,
      "meanFit"
    )

    # Are there more plots from general?
    more_from_general <- (length(type_reduced) > 0)

    # If so, then produce these plots.
    if (more_from_general) {
      ret <- callNextMethod(x = x, y = y, type = type_reduced)
    }

    # Is the meanFit plot requested?
    if ("meanFit" %in% type) {
      # Which types of lines do we have?
      linetype <- c(
        "True toxicity",
        "Average estimated toxicity",
        "95% interval for estimated toxicity"
      )

      # Create the data frame, with true tox, average estimated tox, and 95%
      # (lower, upper) estimated tox (in percentage) stacked below each other.
      dat <- data.frame(
        dose = rep(x@dose_grid, 4L),
        group = rep(1:4, each = length(x@dose_grid)),
        linetype = factor(
          rep(linetype[c(1, 2, 3, 3)], each = length(x@dose_grid)),
          levels = linetype
        ),
        lines = unlist(x@mean_fit) * 100
      )

      # Linetypes for the plot.
      lt <- c(
        "True toxicity" = 1,
        "Average estimated toxicity" = 1,
        "95% interval for estimated toxicity" = 2
      )

      # Colour for the plot.
      col <- c(
        "True toxicity" = 1,
        "Average estimated toxicity" = 2,
        "95% interval for estimated toxicity" = 2
      )

      # Now create and save the plot.
      this_plot <- ggplot() +
        geom_line(
          aes(
            x = dose,
            y = lines,
            group = group,
            linetype = linetype,
            col = linetype
          ),
          data = dat
        )

      this_plot <- this_plot +
        scale_linetype_manual(values = lt) +
        scale_colour_manual(values = col) +
        xlab("Dose level") +
        ylab("Probability of DLT [%]")

      # Add this plot to the bottom.
      ret <-
        if (more_from_general) {
          gridExtra::arrangeGrob(ret, this_plot)
        } else {
          this_plot
        }
    }

    # Then finally plot everything.
    ret
  }
)

# plot-DualSimulationsSummary ----

#' Plot Dual-Endpoint Design Simulation Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Graphical display of dual-endpoint simulation summary.
#'
#' This plot method can be applied to [`DualSimulationsSummary`] objects in
#' order to summarize them graphically. Possible `type` of plots at the moment
#' are those listed in [`plot,SimulationsSummary,missing-method`] plus:
#'
#' \describe{
#'   \item{meanBiomarkerFit}{Plot showing the average fitted dose-biomarker
#'     curve across the trials, together with 95% credible intervals, and
#'     comparison with the assumed truth (as specified by the `trueBiomarker`
#'     argument to [`summary,DualSimulations-method`])}
#' }
#'
#' You can specify any subset of these in the `type` argument.
#'
#' @param x (`DualSimulationsSummary`)\cr the object we want to plot from.
#' @param y (`missing`)\cr not used.
#' @param type (`character`)\cr the types of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot` object if a single plot is
#'   asked for, otherwise a `gtable` object.
#'
#' @aliases plot-DualSimulationsSummary-missing
#' @example examples/Simulations-method-plot-DualSimulationsSummary.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "DualSimulationsSummary",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "nObs",
      "doseSelected",
      "propDLTs",
      "nAboveTarget",
      "meanFit",
      "meanBiomarkerFit"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Subtract the specific plot types for dual-endpoint designs.
    type_reduced <- setdiff(
      type,
      "meanBiomarkerFit"
    )

    # Are there more plots from general?
    more_from_general <- (length(type_reduced) > 0)

    # If so, then produce these plots.
    if (more_from_general) {
      ret <- callNextMethod(x = x, y = y, type = type_reduced)
    }

    # Is the meanBiomarkerFit plot requested?
    if ("meanBiomarkerFit" %in% type) {
      # Which types of lines do we have?
      linetype <- c(
        "True biomarker",
        "Average estimated biomarker",
        "95% interval for estimated biomarker"
      )

      # Create the data frame, with true biomarker, average estimated
      # biomarker, and 95% (lower, upper) estimated biomarker stacked below
      # each other.
      dat <- data.frame(
        dose = rep(x@dose_grid, 4L),
        group = rep(1:4, each = length(x@dose_grid)),
        linetype = factor(
          rep(linetype[c(1, 2, 3, 3)], each = length(x@dose_grid)),
          levels = linetype
        ),
        lines = unlist(x@mean_biomarker_fit)
      )

      # Linetypes for the plot.
      lt <- c(
        "True biomarker" = 1,
        "Average estimated biomarker" = 1,
        "95% interval for estimated biomarker" = 2
      )

      # Colour for the plot.
      col <- c(
        "True biomarker" = 1,
        "Average estimated biomarker" = 2,
        "95% interval for estimated biomarker" = 2
      )

      # Now create and save the plot.
      this_plot <- ggplot() +
        geom_line(
          aes(
            x = dose,
            y = lines,
            group = group,
            linetype = linetype,
            col = linetype
          ),
          data = dat
        )

      this_plot <- this_plot +
        scale_linetype_manual(values = lt) +
        scale_colour_manual(values = col) +
        xlab("Dose level") +
        ylab("Biomarker level")

      # Add this plot to the bottom.
      ret <-
        if (more_from_general) {
          gridExtra::arrangeGrob(ret, this_plot, heights = c(2 / 3, 1 / 3))
        } else {
          this_plot
        }
    }

    # Then finally plot everything.
    ret
  }
)

# h_pseudo_sim_inverse_dose ----

#' Helper Function to Calculate Inverse Dose
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Creates an inverse function to find the dose corresponding to a target
#' probability.
#'
#' @param f (`function`)\cr the truth function mapping dose to probability.
#' @param lower (`number`)\cr lower bound for root finding.
#' @param upper (`number`)\cr upper bound for root finding.
#'
#' @return A function that takes a probability and returns the corresponding
#'   dose.
#'
#' @keywords internal
h_pseudo_sim_inverse_dose <- function(f, lower = -100, upper = 100) {
  function(y) {
    uniroot((function(x) f(x) - y), lower = lower, upper = upper)[1]
  }
}

# h_pseudo_sim_fit_summary ----

#' Helper Function to Calculate Fit Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Calculates fit summary statistics for pseudo simulations.
#'
#' @param fit_list (`list`)\cr list of fit objects from simulations.
#' @param x_most_selected (`integer`)\cr index of dose most often selected.
#' @param dose_grid (`numeric`)\cr dose grid.
#' @param truth (`function`)\cr truth function.
#'
#' @return A list with `fit_at_dose_most_selected` and `mean_fit` components.
#'
#' @keywords internal
h_pseudo_sim_fit_summary <- function(
  fit_list,
  x_most_selected,
  dose_grid,
  truth
) {
  # Find names in the fit list (check if with or without samples).
  fit_names <- sapply(fit_list, names)

  if ("probDLE" %in% fit_names) {
    fit_at_dose_most_selected <- sapply(
      fit_list,
      function(f) {
        f$probDLE[x_most_selected]
      }
    )
    mean_fit_matrix <- sapply(
      fit_list,
      "[[",
      "probDLE"
    )

    mean_fit <- list(
      truth = truth(dose_grid),
      average = rowMeans(mean_fit_matrix)
    )
  } else {
    fit_at_dose_most_selected <-
      sapply(
        fit_list,
        function(f) {
          f$middle[x_most_selected]
        }
      )

    # Mean fitted toxicity (average, lower and upper quantiles) at each dose
    # level (this is required for plotting).
    mean_fit_matrix <- sapply(
      fit_list,
      "[[",
      "middle"
    )
    mean_fit <- list(
      truth = truth(dose_grid),
      average = rowMeans(mean_fit_matrix),
      lower = apply(
        mean_fit_matrix,
        1L,
        quantile,
        0.025
      ),
      upper = apply(
        mean_fit_matrix,
        1L,
        quantile,
        0.975
      )
    )
  }

  list(
    fit_at_dose_most_selected = fit_at_dose_most_selected,
    mean_fit = mean_fit
  )
}

# summary-PseudoSimulations ----

#' Summarize Pseudo Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the simulations, relative to a given truth.
#'
#' @param object (`PseudoSimulations`)\cr the object we want to summarize.
#' @param truth (`function`)\cr a function which takes as input a dose (vector)
#'   and returns the true probability (vector) for toxicity.
#' @param targetEndOfTrial (`number`)\cr the target probability of DLE wanted
#'   to achieve at the end of a trial.
#' @param targetDuringTrial (`number`)\cr the target probability of DLE wanted
#'   to achieve during a trial.
#' @param ... additional arguments can be supplied here for `truth`.
#'
#' @return An object of class [`PseudoSimulationsSummary`].
#'
#' @aliases summary-PseudoSimulations
#' @example examples/Simulations-method-summarySIMsingle.R
#' @export
#'
setMethod(
  f = "summary",
  signature = signature(object = "PseudoSimulations"),
  def = function(
    object,
    truth,
    targetEndOfTrial = 0.3,
    targetDuringTrial = 0.35,
    ...
  ) {
    # Validate arguments.
    assert_function(truth)
    assert_number(targetEndOfTrial, lower = 0, upper = 1)
    assert_number(targetDuringTrial, lower = 0, upper = 1)

    # Extract dose grid.
    dose_grid <- object@data[[1]]@doseGrid

    # Evaluate true DLE at dose grid.
    true_dle <- truth(dose_grid)

    # Function to obtain corresponding dose level given target prob.
    td <- h_pseudo_sim_inverse_dose(truth, 0, max(dose_grid))

    # Find the dose corresponding to the target dose during trial.
    target_dose_end_of_trial <- as.numeric(td(targetEndOfTrial))

    # Find the dose corresponding to the target dose end of trial.
    target_dose_during_trial <- as.numeric(td(targetDuringTrial))

    # Find the dose at dose grid corresponding to the above two quantities.
    target_dose_end_of_trial_at_dose_grid <- dose_grid[max(which(
      target_dose_end_of_trial - dose_grid >= 0
    ))]
    target_dose_during_trial_at_dose_grid <- dose_grid[max(which(
      target_dose_during_trial - dose_grid >= 0
    ))]

    # A summary for all TDtargetEndOfTrial dose obtained.
    tdeot_summary <- summary(object@final_td_target_end_of_trial_estimates)

    final_dose_rec_summary <- tdeot_summary

    ratio_tdeot_summary <- summary(object@final_tdeot_ratios)
    final_ratio_summary <- ratio_tdeot_summary

    # A summary for all TDtargetDuringTrial dose obtained.
    tddt_summary <- summary(object@final_td_target_during_trial_estimates)

    # What are the levels above target End of Trial?
    x_above_target_end_of_trial <- which(true_dle > targetEndOfTrial)

    # What are the levels above target During Trial?
    x_above_target_during_trial <- which(true_dle > targetDuringTrial)

    # Proportion of DLEs in this trial.
    prop_dle <- sapply(
      object@data,
      function(d) {
        mean(d@y)
      }
    )

    # Mean toxicity risk.
    mean_tox_risk <- sapply(
      object@data,
      function(d) {
        mean(true_dle[d@xLevel])
      }
    )

    # Doses selected for MTD.
    dose_selected <- object@doses

    # Replace NA by 0.
    dose_selected[is.na(dose_selected)] <- 0

    # Dose most often selected as MTD.
    dose_most_selected <-
      as.numeric(names(which.max(table(dose_selected))))

    x_most_selected <-
      match_within_tolerance(dose_most_selected, table = dose_grid)

    # Observed toxicity rate at dose most often selected.
    # Note: this does not seem very useful!
    # Reason: In case of a fine grid, few patients if any will have been
    # treated at this dose.
    tmp <-
      sapply(
        object@data,
        function(d) {
          which_at_this_dose <- which(d@x == dose_most_selected)
          n_at_this_dose <- length(which_at_this_dose)
          n_dlt_at_this_dose <- sum(d@y[which_at_this_dose])
          c(
            nAtThisDose = n_at_this_dose,
            nDLTatThisDose = n_dlt_at_this_dose
          )
        }
      )

    obs_tox_rate_at_dose_most_selected <-
      mean(tmp["nDLTatThisDose", ]) / mean(tmp["nAtThisDose", ])

    # Number of patients overall.
    n_obs <- sapply(
      object@data,
      slot,
      "nObs"
    )

    # Number of patients treated above target End of trial.
    n_above_target_end_of_trial <- sapply(
      object@data,
      function(d) {
        sum(d@xLevel %in% x_above_target_end_of_trial)
      }
    )

    # Number of patients treated above target During trial.
    n_above_target_during_trial <- sapply(
      object@data,
      function(d) {
        sum(d@xLevel %in% x_above_target_during_trial)
      }
    )

    tox_at_doses <- truth(dose_selected)

    # Proportion of trials selecting target TDEndOfTrial and TDDuringTrial.
    nsim <- length(object@data)

    prop_at_target_end_of_trial <- (length(which(
      object@doses == target_dose_end_of_trial_at_dose_grid
    ))) /
      nsim

    prop_at_target_during_trial <- (length(which(
      object@doses == target_dose_during_trial_at_dose_grid
    ))) /
      nsim

    # Calculate fit summary using helper function.
    fit_summary <- h_pseudo_sim_fit_summary(
      fit_list = object@fit,
      x_most_selected = x_most_selected,
      dose_grid = dose_grid,
      truth = truth
    )

    # Give back an object of class PseudoSimulationsSummary.
    .PseudoSimulationsSummary(
      target_end_of_trial = targetEndOfTrial,
      target_dose_end_of_trial = target_dose_end_of_trial,
      target_during_trial = targetDuringTrial,
      target_dose_during_trial = target_dose_during_trial,
      target_dose_end_of_trial_at_dose_grid = target_dose_end_of_trial_at_dose_grid,
      target_dose_during_trial_at_dose_grid = target_dose_during_trial_at_dose_grid,
      tdeot_summary = tdeot_summary,
      tddt_summary = tddt_summary,
      final_dose_rec_summary = final_dose_rec_summary,
      ratio_tdeot_summary = ratio_tdeot_summary,
      final_ratio_summary = final_ratio_summary,
      nsim = nsim,
      prop_dle = prop_dle,
      mean_tox_risk = mean_tox_risk,
      dose_selected = dose_selected,
      dose_most_selected = dose_most_selected,
      obs_tox_rate_at_dose_most_selected = obs_tox_rate_at_dose_most_selected,
      n_obs = n_obs,
      n_above_target_end_of_trial = n_above_target_end_of_trial,
      n_above_target_during_trial = n_above_target_during_trial,
      tox_at_doses_selected = tox_at_doses,
      prop_at_target_end_of_trial = prop_at_target_end_of_trial,
      prop_at_target_during_trial = prop_at_target_during_trial,
      dose_grid = dose_grid,
      fit_at_dose_most_selected = fit_summary$fit_at_dose_most_selected,
      stop_report = object@stop_report,
      mean_fit = fit_summary$mean_fit
    )
  }
)

# show-PseudoSimulationsSummary ----

#' Show the Summary of Pseudo Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Display a summary of pseudo simulation results.
#'
#' @param object (`PseudoSimulationsSummary`)\cr the object we want to print.
#'
#' @return Invisibly returns a data frame of the results with one row and
#'   appropriate column names.
#'
#' @aliases show-PseudoSimulationsSummary
#' @example examples/Simulations-method-showSIMsingle.R
#' @export
#'
setMethod(
  f = "show",
  signature = signature(object = "PseudoSimulationsSummary"),
  def = function(object) {
    r <- Report$new(
      object = object,
      df = as.data.frame(matrix(
        nrow = 1,
        ncol = 0
      )),
      dfNames = character()
    )
    cat(
      "Summary of",
      r$dfSave(object@nsim, "nsim"),
      "simulations\n\n"
    )

    cat(
      "Target probability of DLE p(DLE) used at the end of a trial was",
      r$dfSave(
        object@target_end_of_trial * 100,
        "target_end_of_trial"
      ),
      "%\n"
    )

    cat(
      "The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was",
      r$dfSave(
        object@target_dose_end_of_trial,
        "target_dose_end_of_trial"
      ),
      "\n"
    )
    cat(
      "TDEOT at dose Grid was",
      r$dfSave(
        object@target_dose_end_of_trial_at_dose_grid,
        "target_dose_end_of_trial_at_dose_grid"
      ),
      "\n"
    )

    cat(
      "Target p(DLE) used during a trial was",
      r$dfSave(
        object@target_during_trial * 100,
        "target_during_trial"
      ),
      "%\n"
    )

    cat(
      "The dose level corresponds to the target p(DLE) used during a trial, TDDT, was",
      r$dfSave(
        object@target_dose_during_trial,
        "target_dose_during_trial"
      ),
      "\n"
    )

    cat(
      "TDDT at dose Grid was",
      r$dfSave(
        object@target_dose_during_trial_at_dose_grid,
        "target_dose_during_trial_at_dose_grid"
      ),
      "\n"
    )

    r$report("n_obs", "Number of patients overall", percent = FALSE)
    r$report(
      "n_above_target_end_of_trial",
      "Number of patients treated above the target p(DLE) used at the end of a trial",
      percent = FALSE
    )

    r$report(
      "n_above_target_during_trial",
      "Number of patients treated above the target p(DLE) used during a trial",
      percent = FALSE
    )

    r$report(
      "prop_dle",
      "Proportions of observed DLT in the trials"
    )
    r$report(
      "mean_tox_risk",
      "Mean toxicity risks for the patients"
    )
    r$report(
      "dose_selected",
      "Doses selected as TDEOT",
      percent = FALSE,
      digits = 1
    )

    r$report(
      "tox_at_doses_selected",
      "True toxicity at TDEOT"
    )

    cat(
      "Proportion of trials selecting the TDEOT:",
      r$dfSave(
        object@prop_at_target_end_of_trial * 100,
        "percentAtTarget"
      ),
      "%\n"
    )

    cat(
      "Proportion of trials selecting the TDDT:",
      r$dfSave(
        object@prop_at_target_during_trial * 100,
        "percentAtTarget"
      ),
      "%\n"
    )

    cat(
      "Dose most often selected as TDEOT:",
      r$dfSave(
        object@dose_most_selected,
        "doseMostSelected"
      ),
      "\n"
    )
    cat(
      "Observed toxicity rate at dose most often selected:",
      r$dfSave(
        round(object@obs_tox_rate_at_dose_most_selected * 100),
        "obsToxRateAtDoseMostSelected"
      ),
      "%\n"
    )
    r$report(
      "fit_at_dose_most_selected",
      "Fitted probabilities of DLE at dose most often selected"
    )

    TDEOTSum <- object@tdeot_summary

    r$dfSave(as.numeric(TDEOTSum[1]), "TDEOTMin")
    r$dfSave(as.numeric(TDEOTSum[2]), "TDEOTlower")
    r$dfSave(as.numeric(TDEOTSum[3]), "TDEOTMedian")
    r$dfSave(as.numeric(TDEOTSum[4]), "TDEOTMean")
    r$dfSave(as.numeric(TDEOTSum[5]), "TDEOTUpper")
    r$dfSave(as.numeric(TDEOTSum[6]), "TDEOTMax")

    cat(
      "The summary table of the final TDEOT across all simulations\n",
      capture.output(TDEOTSum)[1],
      "\n",
      capture.output(TDEOTSum)[2],
      "\n"
    )

    ratioTDEOTSum <- object@ratio_tdeot_summary

    r$dfSave(as.numeric(ratioTDEOTSum[1]), "ratioTDEOTMin")
    r$dfSave(as.numeric(ratioTDEOTSum[2]), "ratioTDEOTlower")
    r$dfSave(as.numeric(ratioTDEOTSum[3]), "ratioTDEOTMedian")
    r$dfSave(as.numeric(ratioTDEOTSum[4]), "ratioTDEOTMean")
    r$dfSave(as.numeric(ratioTDEOTSum[5]), "ratioTDEOTUpper")
    r$dfSave(as.numeric(ratioTDEOTSum[6]), "ratioTDEOTMax")

    cat(
      "The summary table of the final ratios of the TDEOT across all simulations\n",
      capture.output(ratioTDEOTSum)[1],
      "\n",
      capture.output(ratioTDEOTSum)[2],
      "\n"
    )

    TDDTSum <- object@tddt_summary

    r$dfSave(as.numeric(TDDTSum[1]), "TDDTMin")
    r$dfSave(as.numeric(TDDTSum[2]), "TDDTlower")
    r$dfSave(as.numeric(TDDTSum[3]), "TDDTMedian")
    r$dfSave(as.numeric(TDDTSum[4]), "TDDTMean")
    r$dfSave(as.numeric(TDDTSum[5]), "TDDTUpper")
    r$dfSave(as.numeric(TDDTSum[6]), "TDDTMax")

    cat(
      "The summary table of the final TDDT across all simulations\n",
      capture.output(TDDTSum)[1],
      "\n",
      capture.output(TDDTSum)[2],
      "\n"
    )

    final_dose_rec_sum <- object@final_dose_rec_summary

    r$dfSave(as.numeric(final_dose_rec_sum[1]), "FinalDoseRecMin")
    r$dfSave(as.numeric(final_dose_rec_sum[2]), "FinalDoseReclower")
    r$dfSave(as.numeric(final_dose_rec_sum[3]), "FinalDoseRecMedian")
    r$dfSave(as.numeric(final_dose_rec_sum[4]), "FinalDoseRecMean")
    r$dfSave(as.numeric(final_dose_rec_sum[5]), "FinalDoseRecUpper")
    r$dfSave(as.numeric(final_dose_rec_sum[6]), "FinalDoseRecMax")

    cat(
      "The summary table of dose levels, the optimal dose\n to recommend for subsequent study across all simulations\n",
      capture.output(final_dose_rec_sum)[1],
      "\n",
      capture.output(final_dose_rec_sum)[2],
      "\n"
    )

    final_ratio_sum <- object@final_ratio_summary

    r$dfSave(as.numeric(final_ratio_sum[1]), "FinalratioMin")
    r$dfSave(as.numeric(final_ratio_sum[2]), "Finalratiolower")
    r$dfSave(as.numeric(final_ratio_sum[3]), "FinalratioMedian")
    r$dfSave(as.numeric(final_ratio_sum[4]), "FinalratioMean")
    r$dfSave(as.numeric(final_ratio_sum[5]), "FinalratioUpper")
    r$dfSave(as.numeric(final_ratio_sum[6]), "FinalratioMax")

    cat(
      "The summary table of the final ratios of the optimal dose for stopping across
                  all simulations\n",
      capture.output(final_ratio_sum)[1],
      "\n",
      capture.output(final_ratio_sum)[2],
      "\n\n"
    )

    # Report individual stopping rules with non-NA labels.
    stop_pct_to_print <- h_calc_report_label_percentage(object@stop_report)

    if (length(stop_pct_to_print) > 0) {
      cat(
        "Stop reason triggered:\n",
        paste(names(stop_pct_to_print), ": ", stop_pct_to_print, "%\n")
      )
    }

    # Finally assign names to the df and return it invisibly.
    names(r$df) <- r$dfNames
    invisible(r$df)
  }
)

# plot-PseudoSimulationsSummary ----

#' Plot Pseudo Simulation Summary
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Graphical display of the simulation summary.
#'
#' This plot method can be applied to [`PseudoSimulationsSummary`] objects in
#' order to summarize them graphically. This can be used when only DLE responses
#' are involved in the simulations. This also applied to results with or without
#' samples generated during the simulations.
#'
#' @param x (`PseudoSimulationsSummary`)\cr the object we want to plot from.
#' @param y (`missing`)\cr missing object, not used.
#' @param type (`character`)\cr the types of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot2` object if a single plot is asked for, otherwise a
#'   `gtable` object.
#'
#' @aliases plot-PseudoSimulationsSummary-missing
#' @example examples/Simulations-method-plotSUMsingle.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "PseudoSimulationsSummary",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "nObs",
      "doseSelected",
      "propDLE",
      "nAboveTargetEndOfTrial",
      "meanFit"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Distribution of overall sample size.
    if ("nObs" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@n_obs,
          description = "Number of patients in total"
        )
    }

    # Distribution of final MTD estimate.
    if ("doseSelected" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@dose_selected,
          description = "MTD estimate"
        )
    }

    # Distribution of proportion of DLTs.
    if ("propDLE" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@prop_dle * 100,
          description = "Proportion of DLE [%]",
          xaxisround = 1
        )
    }

    # Distribution of number of patients treated at too much tox.
    if ("nAboveTargetEndOfTrial" %in% type) {
      plot_list[[plot_index <- plot_index + 1L]] <-
        h_barplot_percentages(
          x = x@n_above_target_end_of_trial,
          description = "Number of patients above target"
        )
    }

    # First combine these small plots.
    if (length(plot_list)) {
      ret <-
        # If there is only one plot.
        if (identical(length(plot_list), 1L)) {
          # Just use that.
          plot_list[[1L]]
        } else {
          # Multiple plots in this case.
          do.call(
            gridExtra::arrangeGrob,
            plot_list
          )
        }
    }

    # The meanFit plot.
    if ("meanFit" %in% type) {
      # Find if DLE samples are generated in the simulations by checking if
      # the lower limits of the 95% credibility interval are calculated.
      if (!is.null(x@mean_fit$lower)) {
        # Which types of lines do we have?
        linetype <- c(
          "True toxicity",
          "Average estimated toxicity",
          "95% interval for estimated toxicity"
        )

        # Create the data frame, with true tox, average estimated tox, and 95%
        # (lower, upper) estimated tox (in percentage) stacked below each other.
        dat <- data.frame(
          dose = rep(x@dose_grid, 4L),
          group = rep(1:4, each = length(x@dose_grid)),
          linetype = factor(
            rep(linetype[c(1, 2, 3, 3)], each = length(x@dose_grid)),
            levels = linetype
          ),
          lines = unlist(x@mean_fit) * 100
        )

        # Linetypes for the plot.
        lt <- c(
          "True toxicity" = 1,
          "Average estimated toxicity" = 1,
          "95% interval for estimated toxicity" = 2
        )

        # Colour for the plot.
        col <- c(
          "True toxicity" = 1,
          "Average estimated toxicity" = 2,
          "95% interval for estimated toxicity" = 2
        )

        # Now create and save the plot.
        this_plot <- ggplot() +
          geom_line(
            aes(
              x = dose,
              y = lines,
              group = group,
              linetype = linetype,
              col = linetype
            ),
            data = dat
          )

        this_plot <- this_plot +
          scale_linetype_manual(values = lt) +
          scale_colour_manual(values = col) +
          xlab("Dose level") +
          ylab("Probability of DLE [%]")
      } else {
        # Which types of lines do we have?
        linetype <- c(
          "True toxicity",
          "Average estimated toxicity"
        )

        # Create the data frame, with true tox, average estimated tox
        # (in percentage) stacked below each other.
        dat <- data.frame(
          dose = rep(x@dose_grid, 2L),
          group = rep(1:2, each = length(x@dose_grid)),
          linetype = factor(
            rep(linetype[c(1, 2)], each = length(x@dose_grid)),
            levels = linetype
          ),
          lines = unlist(x@mean_fit) * 100
        )

        # Linetypes for the plot.
        lt <- c(
          "True toxicity" = 1,
          "Average estimated toxicity" = 1
        )

        # Colour for the plot.
        col <- c(
          "True toxicity" = 1,
          "Average estimated toxicity" = 2
        )

        # Now create and save the plot.
        this_plot <- ggplot() +
          geom_line(
            aes(
              x = dose,
              y = lines,
              group = group,
              linetype = linetype,
              col = linetype
            ),
            data = dat
          )

        this_plot <- this_plot +
          scale_linetype_manual(values = lt) +
          scale_colour_manual(values = col) +
          xlab("Dose level") +
          ylab("Probability of DLE [%]")
      }

      # Then add this plot at the bottom.
      ret <- if (exists("ret")) {
        gridExtra::arrangeGrob(ret, this_plot)
      } else {
        this_plot
      }
    }
    ret
  }
)


# plot-PseudoDualSimulations-missing ----

#' Plot Pseudo Dual Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the simulations with plots.
#'
#' This plot method can be applied to [`PseudoDualSimulations`] objects in
#' order to summarize them graphically. Possible `type`s of plots at the moment
#' are:
#' \describe{
#'   \item{trajectory}{Summary of the trajectory of the simulated trials}
#'   \item{dosesTried}{Average proportions of the doses tested in patients}
#'   \item{sigma2}{The variance of the efficacy responses}
#' }
#' You can specify one or both of these in the `type` argument.
#'
#' @param x (`PseudoDualSimulations`)\\cr the object we want to plot from.
#' @param y (`missing`)\\cr missing object, not used.
#' @param type (`character`)\\cr the type of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot2` object if a single plot is asked for, otherwise a
#'   `gtable` object.
#'
#' @aliases plot-PseudoDualSimulations-missing
#' @example examples/Simulations-method-plotSIMDual.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "PseudoDualSimulations",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "trajectory",
      "dosesTried",
      "sigma2"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Subtract the specific plot types for dual-endpoint simulation results.
    type_reduced <- setdiff(
      type,
      "sigma2"
    )

    # Are there more plots from general?
    more_from_general <- (length(type_reduced) > 0)

    # If so, then produce these plots.
    if (more_from_general) {
      gen_plot <- callNextMethod(x = x, y = y, type = type_reduced)
    }

    # Now to the specific dual-endpoint plots:
    # Efficacy variance estimates boxplot.
    if ("sigma2" %in% type) {
      # Save the plot.
      plot_list[[plot_index <- plot_index + 1L]] <-
        ggplot(data = data.frame(y = x@sigma2_est), aes(x = factor(0), y = y)) +
        geom_boxplot() +
        coord_flip() +
        scale_x_discrete(breaks = NULL) +
        xlab("") +
        ylab("Efficacy variance estimates")
    }

    # Then finally plot everything.
    if (identical(length(plot_list), 0L)) {
      return(gen_plot)
    } else if (identical(length(plot_list), 1L)) {
      ret <- plot_list[[1L]]
    } else {
      ret <- do.call(
        gridExtra::arrangeGrob,
        plot_list
      )
    }

    if (more_from_general) {
      ret <- gridExtra::arrangeGrob(gen_plot, ret, heights = c(2 / 3, 1 / 3))
    }

    ret
  }
)

# plot-PseudoDualFlexiSimulations-missing ----

#' Plot Pseudo Dual Flexi Simulations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize the simulations with plots.
#'
#' This plot method can be applied to [`PseudoDualFlexiSimulations`] objects in
#' order to summarize them graphically. Possible `type`s of plots at the moment
#' are:
#'
#' \describe{
#'   \item{trajectory}{Summary of the trajectory of the simulated trials}
#'   \item{dosesTried}{Average proportions of the doses tested in patients}
#'   \item{sigma2}{The variance of the efficacy responses}
#'   \item{sigma2betaW}{The variance of the random walk model}
#' }
#'
#' You can specify one or both of these in the `type` argument.
#'
#' @param x (`PseudoDualFlexiSimulations`)\cr the object we want to plot from.
#' @param y (`missing`)\cr missing object, not used.
#' @param type (`character`)\cr the type of plots you want to obtain.
#' @param ... not used.
#'
#' @return A single `ggplot2` object if a single plot is asked for, otherwise a
#'   `gtable` object.
#'
#' @aliases plot-PseudoDualFlexiSimulations-missing
#' @example examples/Simulations-method-plotSIMDualFlexi.R
#' @export
#'
setMethod(
  f = "plot",
  signature = signature(
    x = "PseudoDualFlexiSimulations",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "trajectory",
      "dosesTried",
      "sigma2",
      "sigma2betaW"
    ),
    ...
  ) {
    # Validate arguments.
    type <- match.arg(type, several.ok = TRUE)
    assert_character(type, min.len = 1)

    # Start the plot list.
    plot_list <- list()
    plot_index <- 0L

    # Subtract the specific plot types for dual-endpoint simulation results.
    type_reduced <- setdiff(type, "sigma2betaW")

    # Are there more plots from general?
    more_from_general <- (length(type_reduced) > 0)

    # If so, then produce these plots.
    if (more_from_general) {
      gen_plot <- callNextMethod(x = x, y = y, type = type_reduced)
    }

    # Now to the specific dual-endpoint plots:
    # Random walk model variance estimates boxplot.
    if ("sigma2betaW" %in% type) {
      # Save the plot.
      plot_list[[plot_index <- plot_index + 1L]] <-
        ggplot(
          data = data.frame(y = x@sigma2_beta_w_est),
          aes(x = factor(0), y = y)
        ) +
        geom_boxplot() +
        coord_flip() +
        scale_x_discrete(breaks = NULL) +
        xlab("") +
        ylab("Random walk model variance estimates")
    }

    # Then finally plot everything.
    if (identical(length(plot_list), 0L)) {
      return(gen_plot)
    } else if (identical(length(plot_list), 1L)) {
      ret <- plot_list[[1L]]
    } else {
      ret <- do.call(
        gridExtra::arrangeGrob,
        plot_list
      )
    }

    if (more_from_general) {
      ret <- gridExtra::arrangeGrob(gen_plot, ret, heights = c(2 / 3, 1 / 3))
    }

    ret
  }
)

## -----------------------------------------------------------------------------------------
##' Summary for Pseudo Dual responses simulations, relative to a given pseudo DLE and efficacy model
##' (except the EffFlexi class model)
##'
##' @param object the \code{\linkS4class{PseudoDualSimulations}} object we want to summarize
##' @param trueDLE a function which takes as input a dose (vector) and returns the true probability (vector)
##' of DLE
##' @param trueEff a function which takes as input a dose (vector) and returns the mean efficacy value(s) (vector).
##' @param targetEndOfTrial the target probability of DLE that are used at the end of a trial. Default at 0.3.
##' @param targetDuringTrial the target probability of DLE that are used during the trial. Default at 0.35.
##' @param \dots Additional arguments can be supplied here for \code{trueDLE} and \code{trueEff}
##' @return an object of class \code{\linkS4class{PseudoDualSimulationsSummary}}
##'
##' @example examples/Simulations-method-summarySIMDual.R
##' @export
##' @keywords methods
setMethod(
  "summary",
  signature = signature(object = "PseudoDualSimulations"),
  def = function(
    object,
    trueDLE,
    trueEff,
    targetEndOfTrial = 0.3,
    targetDuringTrial = 0.35,
    ...
  ) {
    ## call the parent method
    start <- callNextMethod(
      object = object,
      truth = trueDLE,
      targetEndOfTrial = targetEndOfTrial,
      targetDuringTrial = targetDuringTrial,
      ...
    )
    doseGrid <- object@data[[1]]@doseGrid

    ## ## dose level most often selected as MTD (TDtargetEnd of Trial)
    xMostSelected <-
      match_within_tolerance(start@dose_most_selected, table = doseGrid)

    ## check if true Eff is a function
    ## check if special case applies
    isTrueEffFx <- is.function(trueEff)

    TDtargetEndOfTrial <- start@target_dose_end_of_trial

    if (isTrueEffFx) {
      negtrueGainfn <- function(dose) {
        return(-(trueEff(dose)) / (1 + (trueDLE(dose) / (1 - trueDLE(dose)))))
      }
      Gstar <- optim(exp(1), negtrueGainfn, method = "BFGS")$par
      maxGainValue <- -(optim(exp(1), negtrueGainfn, method = "BFGS")$value)
      GstarAtDoseGrid <- doseGrid[max(which(Gstar - doseGrid >= 0))]
    } else {
      trueGain <- (trueEff) /
        (1 + (trueDLE(doseGrid) / (1 - trueDLE(doseGrid))))
      maxGainValue <- max(trueGain)
      Gstar <- doseGrid[which.max(trueGain)]
      GstarAtDoseGrid <- Gstar
    }

    ## A summary for all final Gstar obtained
    GstarSummary <- summary(object@final_gstar_estimates)
    ratioGstarSummary <- summary(object@final_gstar_ratios)

    FinalDoseRecSummary <- summary(object@final_optimal_dose)
    FinalRatioSummary <- summary(object@final_ratios)

    ## find names in the fit efficacy list (check it is with or without samples)
    FitNames <- sapply(object@fit_eff, names)
    if ("ExpEff" %in% FitNames) {
      ## fitted efficacy level at dose most often selected
      EffFitAtDoseMostSelected <- sapply(
        object@fit_eff,
        function(f) {
          f$ExpEff[xMostSelected]
        }
      )
      meanEffFitMatrix <- sapply(
        object@fit_eff,
        "[[",
        "ExpEff"
      )

      meanEffFit <- list(
        truth = trueEff(doseGrid),
        average = rowMeans(meanEffFitMatrix)
      )
    } else {
      ## fitted efficacy level at dose most often selected
      EffFitAtDoseMostSelected <-
        sapply(
          object@fit_eff,
          function(f) {
            f$middle[xMostSelected]
          }
        )

      ## mean fitted  curve (average, lower and upper quantiles)
      ## at each dose level
      ## (this is required for plotting)
      meanEffFitMatrix <- sapply(
        object@fit_eff,
        "[[",
        "middle"
      )

      ## check if special case applies

      if (isTrueEffFx) {
        TRUTHeff <- trueEff(doseGrid)
      } else {
        TRUTHeff <- trueEff
      }

      meanEffFit <- list(
        truth = TRUTHeff,
        average = rowMeans(meanEffFitMatrix),
        lower = apply(
          meanEffFitMatrix,
          1L,
          quantile,
          0.025
        ),
        upper = apply(
          meanEffFitMatrix,
          1L,
          quantile,
          0.975
        )
      )
    }

    ## give back an object of class PseudoDualSimulationsSummary,
    ## for which we then define a print / plot method
    ret <- .PseudoDualSimulationsSummary(
      start,
      target_gstar = Gstar,
      target_gstar_at_dose_grid = GstarAtDoseGrid,
      gstar_summary = GstarSummary,
      ratio_gstar_summary = ratioGstarSummary,
      final_dose_rec_summary = FinalDoseRecSummary,
      final_ratio_summary = FinalRatioSummary,
      eff_fit_at_dose_most_selected = EffFitAtDoseMostSelected,
      mean_eff_fit = meanEffFit,
      stop_report = object@stop_report
    )

    return(ret)
  }
)
## --------------------------------------------------------------------------------------------------
##' Summary for Pseudo Dual responses simulations given a pseudo DLE model and the Flexible efficacy model.
##'
##' @param object the \code{\linkS4class{PseudoDualFlexiSimulations}} object we want to summarize
##' @param trueDLE a function which takes as input a dose (vector) and returns the true probability of DLE (vector)
##' @param trueEff a vector which takes as input the true mean efficacy values at all dose levels (in order)
##' @param targetEndOfTrial the target probability of DLE that are used at the end of a trial. Default at 0.3.
##' @param targetDuringTrial the target probability of DLE that are used during the trial. Default at 0.35.
##' @param \dots Additional arguments can be supplied here for \code{trueDLE} and \code{trueEff}
##' @return an object of class \code{\linkS4class{PseudoDualSimulationsSummary}}
##'
##' @example examples/Simulations-method-summarySIMDualFlexi.R
##' @export
##' @keywords methods
setMethod(
  "summary",
  signature = signature(object = "PseudoDualFlexiSimulations"),
  def = function(
    object,
    trueDLE,
    trueEff,
    targetEndOfTrial = 0.3,
    targetDuringTrial = 0.35,
    ...
  ) {
    ## call the parent method
    start <- callNextMethod(
      object = object,
      trueDLE = trueDLE,
      trueEff = trueEff,
      targetEndOfTrial = targetEndOfTrial,
      targetDuringTrial = targetDuringTrial,
      ...
    )

    ## give back an object of class PseudoDualSimulationsSummary,
    ## for which we then define a print / plot method
    ret <- .PseudoDualSimulationsSummary(start)

    return(ret)
  }
)

## ----------------------------------------------------------------------------------------
##' Show the summary of Pseudo Dual simulations summary
##'
##' @param object the \code{\linkS4class{PseudoDualSimulationsSummary}} object we want to print
##' @return invisibly returns a data frame of the results with one row and appropriate column names
##'
##'
##' @example examples/Simulations-method-showSIMDual.R
##' @export
##' @keywords methods
setMethod(
  "show",
  signature = signature(object = "PseudoDualSimulationsSummary"),
  def = function(object) {
    ## call the parent method
    df <- callNextMethod(object)
    dfNames <- names(df)

    ## start report object
    r <- Report$new(
      object = object,
      df = df,
      dfNames = dfNames
    )

    ## add three reporting lines
    cat(
      "Target Gstar, the dose which gives the maximum gain value was",
      r$dfSave(
        object@target_gstar,
        "targetGstar"
      ),
      "\n"
    )
    cat(
      "Target Gstar at dose Grid was",
      r$dfSave(
        object@target_gstar_at_dose_grid,
        "targetGstarAtDoseGrid"
      ),
      "\n"
    )

    GstarSum <- object@gstar_summary

    r$dfSave(as.numeric(GstarSum[1]), "GstarMin")
    r$dfSave(as.numeric(GstarSum[2]), "Gstarlower")
    r$dfSave(as.numeric(GstarSum[3]), "GstarMedian")
    r$dfSave(as.numeric(GstarSum[4]), "GstarMean")
    r$dfSave(as.numeric(GstarSum[5]), "GstarUpper")
    r$dfSave(as.numeric(GstarSum[6]), "GstarMax")

    cat(
      "The summary table of the final Gstar across all simulations\n",
      capture.output(GstarSum)[1],
      "\n",
      capture.output(GstarSum)[2],
      "\n"
    )

    ratioGstarSum <- object@ratio_gstar_summary

    r$dfSave(as.numeric(ratioGstarSum[1]), "ratioGstarMin")
    r$dfSave(as.numeric(ratioGstarSum[2]), "ratioGstarlower")
    r$dfSave(as.numeric(ratioGstarSum[3]), "ratioGstarMedian")
    r$dfSave(as.numeric(ratioGstarSum[4]), "ratioGstarMean")
    r$dfSave(as.numeric(ratioGstarSum[5]), "ratioGstarUpper")
    r$dfSave(as.numeric(ratioGstarSum[6]), "ratioGstarMax")

    cat(
      "The summary table of the final ratios of the Gstar across all simulations\n",
      capture.output(ratioGstarSum)[1],
      "\n",
      capture.output(ratioGstarSum)[2],
      "\n"
    )

    FinalDoseRecSum <- object@final_dose_rec_summary

    r$dfSave(as.numeric(FinalDoseRecSum[1]), "FinalDoseRecMin")
    r$dfSave(as.numeric(FinalDoseRecSum[2]), "FinalDoseReclower")
    r$dfSave(as.numeric(FinalDoseRecSum[3]), "FinalDoseRecMedian")
    r$dfSave(as.numeric(FinalDoseRecSum[4]), "FinalDoseRecMean")
    r$dfSave(as.numeric(FinalDoseRecSum[5]), "FinalDoseRecUpper")
    r$dfSave(as.numeric(FinalDoseRecSum[6]), "FinalDoseRecMax")

    cat(
      "The summary table of dose levels, the optimal dose\n to recommend for subsequent study across all simulations\n",
      capture.output(FinalDoseRecSum)[1],
      "\n",
      capture.output(FinalDoseRecSum)[2],
      "\n"
    )

    FinalratioSum <- object@final_ratio_summary

    r$dfSave(as.numeric(FinalratioSum[1]), "FinalratioMin")
    r$dfSave(as.numeric(FinalratioSum[2]), "Finalratiolower")
    r$dfSave(as.numeric(FinalratioSum[3]), "FinalratioMedian")
    r$dfSave(as.numeric(FinalratioSum[4]), "FinalratioMean")
    r$dfSave(as.numeric(FinalratioSum[5]), "FinalratioUpper")
    r$dfSave(as.numeric(FinalratioSum[6]), "FinalratioMax")

    cat(
      "The summary table of the final ratios of the optimal dose for stopping across
        all simulations\n",
      capture.output(FinalratioSum)[1],
      "\n",
      capture.output(FinalratioSum)[2],
      "\n"
    )

    r$report(
      "eff_fit_at_dose_most_selected",
      "Fitted expected efficacy level at dose most often selected",
      percent = FALSE,
      digits = 1
    )

    # Report individual stopping rules with non-<NA> labels.

    stop_pct_to_print <- h_calc_report_label_percentage(object@stop_report)

    if (length(stop_pct_to_print) > 0) {
      cat(
        "Stop reason triggered:\n",
        paste(names(stop_pct_to_print), ": ", stop_pct_to_print, "%\n")
      )
    }

    ## and return the updated information
    names(r$df) <- r$dfNames
    invisible(r$df)
  }
)

## --------------------------------------------------------------------------------------------------
##' Plot the summary of Pseudo Dual Simulations summary
##'
##' This plot method can be applied to \code{\linkS4class{PseudoDualSimulationsSummary}} objects in order
##' to summarize them graphically. Possible \code{type} of plots at the moment are those listed in
##' \code{\link{plot,PseudoSimulationsSummary,missing-method}} plus:
##' \describe{\item{meanEffFit}{Plot showing the fitted dose-efficacy curve. If no samples are involved, only the
##' average fitted dose-efficacy curve across the trials will be plotted. If samples (DLE and efficacy) are involved,
##' the average fitted dose-efficacy curve across the trials, together with the 95% credibility interval; and comparison
##' with the assumed truth (as specified by the \code{trueEff} argument to
##' \code{\link{summary,PseudoDualSimulations-method}})}}
##' You can specify any subset of these in the \code{type} argument.
##'
##' @param x the \code{\linkS4class{PseudoDualSimulationsSummary}} object we want to plot from
##' @param y missing
##' @param type the types of plots you want to obtain.
##' @param \dots not used
##' @return A single \code{\link[ggplot2]{ggplot}} object if a single plot is
##' asked for, otherwise a `gtable` object.
##'
##' @importFrom ggplot2 geom_histogram ggplot aes xlab ylab geom_line
##' scale_linetype_manual scale_colour_manual
##' @importFrom gridExtra arrangeGrob
##'
##' @example examples/Simulations-method-plotSUMDual.R
##' @export
##' @keywords methods
setMethod(
  "plot",
  signature = signature(
    x = "PseudoDualSimulationsSummary",
    y = "missing"
  ),
  def = function(
    x,
    y,
    type = c(
      "nObs",
      "doseSelected",
      "propDLE",
      "nAboveTargetEndOfTrial",
      "meanFit",
      "meanEffFit"
    ),
    ...
  ) {
    ## which plots should be produced?
    type <- match.arg(type, several.ok = TRUE)
    stopifnot(length(type) > 0L)

    ## substract the specific plot types for dual-endpoint
    ## designs
    typeReduced <- setdiff(
      type,
      "meanEffFit"
    )

    ## are there more plots from general?
    moreFromGeneral <- (length(typeReduced) > 0)

    ## if so, then produce these plots
    if (moreFromGeneral) {
      ret <- callNextMethod(x = x, y = y, type = typeReduced)
    }

    ## is the meanBiomarkerFit plot requested?
    if ("meanEffFit" %in% type) {
      ## Find if Effsamples are generated in the simulations
      ## by checking if there the lower limits of the 95% Credibility
      ## interval are calculated
      if (!is.null(x@mean_eff_fit$lower)) {
        ## which types of lines do we have?
        linetype <- c(
          "True Expected Efficacy",
          "Average estimated expected efficacy",
          "95% interval for estimated expected efficacy"
        )

        ## create the data frame, with
        ## true biomarker, average estimated expected efficacy, and 95% (lower, upper)
        ## estimated biomarker stacked below each other
        dat <- data.frame(
          dose = rep(x@dose_grid, 4L),
          group = rep(1:4, each = length(x@dose_grid)),
          linetype = factor(
            rep(linetype[c(1, 2, 3, 3)], each = length(x@dose_grid)),
            levels = linetype
          ),
          lines = unlist(x@mean_eff_fit)
        )

        ## linetypes for the plot
        lt <- c(
          "True Expected Efficacy" = 1,
          "Average estimated expected efficacy" = 1,
          "95% interval for estimated expected efficacy" = 2
        )

        ## colour for the plot
        col <- c(
          "True Expected Efficacy" = 1,
          "Average estimated expected efficacy" = 4,
          "95% interval for estimated expected efficacy" = 4
        )

        ## now create and save the plot
        thisPlot <- ggplot() +
          geom_line(
            aes(
              x = dose,
              y = lines,
              group = group,
              linetype = linetype,
              col = linetype
            ),
            data = dat
          )

        thisPlot <- thisPlot +
          scale_linetype_manual(values = lt) +
          scale_colour_manual(values = col) +
          xlab("Dose level") +
          ylab("Expected Efficacy level")
      } else {
        linetype <- c(
          "True Expected Efficacy",
          "Average estimated expected efficacy"
        )

        ## create the data frame, with
        ## true biomarker, average estimated expected efficacy
        dat <- data.frame(
          dose = rep(x@dose_grid, 2L),
          group = rep(1:2, each = length(x@dose_grid)),
          linetype = factor(
            rep(linetype[c(1, 2)], each = length(x@dose_grid)),
            levels = linetype
          ),
          lines = unlist(x@mean_eff_fit)
        )

        ## linetypes for the plot
        lt <- c(
          "True Expected Efficacy" = 1,
          "Average estimated expected efficacy" = 1
        )

        ## colour for the plot
        col <- c(
          "True Expected Efficacy" = 1,
          "Average estimated expected efficacy" = 4
        )

        ## now create and save the plot
        thisPlot <- ggplot() +
          geom_line(
            aes(
              x = dose,
              y = lines,
              group = group,
              linetype = linetype,
              col = linetype
            ),
            data = dat
          )

        thisPlot <- thisPlot +
          scale_linetype_manual(values = lt) +
          scale_colour_manual(values = col) +
          xlab("Dose level") +
          ylab("Expected Efficacy level")
      }

      ## add this plot to the bottom
      ret <-
        if (moreFromGeneral) {
          gridExtra::arrangeGrob(ret, thisPlot, heights = c(2 / 3, 1 / 3))
        } else {
          thisPlot
        }
    }

    ## then finally plot everything
    ret
  }
)
