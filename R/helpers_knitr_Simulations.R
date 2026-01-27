# Integration with knitr ----
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional utility functions to allow human-friendly rendition of
#' crmPack objects in Markdown and Quarto files.  This file contains methods for
#' all Simulations and SimulationsSummary classes.
#'
#' @return a character string that represents the object in markdown.
#' @name knit_print
NULL

# GeneralSimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @param x (`GeneralSimulations`)\cr the object to be printed.
#' @param asis (`flag`)\cr should the result be returned as is (using
#'   [`knitr::asis_output()`]). Defaults to `TRUE`.
#' @param ... passed to [`knitr::kable()`].
#' @rdname knit_print
#' @export
#' @method knit_print GeneralSimulations
knit_print.GeneralSimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  n_sims <- length(x@doses)
  n_doses <- if (length(x@data) > 0) length(x@data[[1]]@doseGrid) else 0

  rv <- paste0(
    "### Simulation Results\n\n",
    "- **Number of simulations:** ", n_sims, "\n",
    "- **Random seed:** ", x@seed, "\n",
    "- **Dose grid size:** ", n_doses, "\n",
    "- **Final recommended doses:** ",
    paste(sprintf("%.2f", unique(sort(x@doses))), collapse = ", "),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Simulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`Simulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print Simulations
knit_print.Simulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.GeneralSimulations(x, asis = FALSE)

  stop_counts <- table(unlist(x@stop_reasons))
  stop_summary <- paste(
    names(stop_counts),
    " (",
    sprintf("%.1f%%", 100 * stop_counts / length(x@stop_reasons)),
    ")",
    sep = "",
    collapse = ", "
  )

  rv <- paste0(
    base_output,
    "- **Stopping reasons:** ", stop_summary, "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# DualSimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`DualSimulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print DualSimulations
knit_print.DualSimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.Simulations(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "- **Rho estimates:** ",
    "Mean = ", sprintf("%.3f", mean(x@rho_est)),
    ", Range = [", sprintf("%.3f", min(x@rho_est)),
    ", ", sprintf("%.3f", max(x@rho_est)), "]\n",
    "- **Sigma2W estimates:** ",
    "Mean = ", sprintf("%.3f", mean(x@sigma2w_est)),
    ", Range = [", sprintf("%.3f", min(x@sigma2w_est)),
    ", ", sprintf("%.3f", max(x@sigma2w_est)), "]\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# PseudoSimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`PseudoSimulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print PseudoSimulations
knit_print.PseudoSimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.GeneralSimulations(x, asis = FALSE)

  stop_counts <- table(unlist(x@stop_reasons))
  stop_summary <- paste(
    names(stop_counts),
    " (",
    sprintf("%.1f%%", 100 * stop_counts / length(x@stop_reasons)),
    ")",
    sep = "",
    collapse = ", "
  )

  rv <- paste0(
    base_output,
    "- **TD target during trial:** ",
    "Mean = ", sprintf("%.2f", mean(x@final_td_target_during_trial_estimates)),
    "\n",
    "- **TD target end of trial:** ",
    "Mean = ", sprintf("%.2f", mean(x@final_td_target_end_of_trial_estimates)),
    "\n",
    "- **Stopping reasons:** ", stop_summary, "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# PseudoDualSimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`PseudoDualSimulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print PseudoDualSimulations
knit_print.PseudoDualSimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.PseudoSimulations(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "- **Gstar estimates:** ",
    "Mean = ", sprintf("%.2f", mean(x@final_gstar_estimates)),
    ", Range = [", sprintf("%.2f", min(x@final_gstar_estimates)),
    ", ", sprintf("%.2f", max(x@final_gstar_estimates)), "]\n",
    "- **Optimal dose:** ",
    "Mean = ", sprintf("%.2f", mean(x@final_optimal_dose)),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# PseudoDualFlexiSimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`PseudoDualFlexiSimulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print PseudoDualFlexiSimulations
knit_print.PseudoDualFlexiSimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.PseudoDualSimulations(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "- **Sigma2 beta W estimates:** ",
    "Mean = ", sprintf("%.3f", mean(x@sigma2_beta_w_est)),
    ", Range = [", sprintf("%.3f", min(x@sigma2_beta_w_est)),
    ", ", sprintf("%.3f", max(x@sigma2_beta_w_est)), "]\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# DASimulations ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`DASimulations`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print DASimulations
knit_print.DASimulations <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.Simulations(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "- **Trial duration:** ",
    "Mean = ", sprintf("%.2f", mean(x@trial_duration)),
    ", Range = [", sprintf("%.2f", min(x@trial_duration)),
    ", ", sprintf("%.2f", max(x@trial_duration)), "]\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Helper functions for SimulationsSummary ----

h_format_summary_table <- function(x) {
  if (is.table(x) || is.matrix(x)) {
    as.numeric(x)
  } else {
    x
  }
}

h_six_number_summary <- function(tbl, caption = NULL) {
  if (is.null(tbl) || length(tbl) == 0) {
    return(NULL)
  }

  df <- data.frame(
    Statistic = names(tbl),
    Value = sprintf("%.2f", h_format_summary_table(tbl)),
    stringsAsFactors = FALSE
  )

  param <- list(x = df, row.names = FALSE)
  if (!is.null(caption)) {
    param[["caption"]] <- caption
  }

  paste(do.call(knitr::kable, param), collapse = "\n")
}

# GeneralSimulationsSummary ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`GeneralSimulationsSummary`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print GeneralSimulationsSummary
knit_print.GeneralSimulationsSummary <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "### Simulation Summary (", x@nsim, " simulations)\n\n",
    "**Target toxicity interval:** ",
    sprintf("[%.1f%%, %.1f%%]", 100 * x@target[1], 100 * x@target[2]),
    "\n\n",
    "**Target dose interval:** ",
    sprintf("[%.2f, %.2f]", x@target_dose_interval[1], x@target_dose_interval[2]),
    "\n\n",
    "**Dose most often selected as MTD:** ", x@dose_most_selected,
    " (observed toxicity rate: ", sprintf("%.1f%%", 100 * x@obs_tox_rate_at_dose_most_selected),
    ")\n\n",
    "**Proportion selecting target MTD:** ", sprintf("%.1f%%", 100 * x@prop_at_target),
    "\n\n",
    "**Number of patients:** ", ifelse(is.list(x@n_obs), paste(x@n_obs, collapse = ", "), x@n_obs),
    "\n\n",
    "**Patients treated above target:** ", x@n_above_target, "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# SimulationsSummary ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`SimulationsSummary`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print SimulationsSummary
knit_print.SimulationsSummary <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.GeneralSimulationsSummary(x, asis = FALSE)

  stop_report_summary <- if (nrow(x@stop_report) > 0) {
    stop_pct <- apply(x@stop_report, 2, function(col) {
      sprintf("%.1f%%", 100 * mean(col, na.rm = TRUE))
    })
    paste(
      "**Stopping rules triggered:** ",
      paste(
        colnames(x@stop_report),
        " (",
        stop_pct,
        ")",
        sep = "",
        collapse = ", "
      ),
      "\n\n"
    )
  } else {
    ""
  }

  rv <- paste0(
    base_output,
    "**Fitted toxicity at dose most selected:** ",
    sprintf("%.3f", x@fit_at_dose_most_selected),
    "\n\n",
    stop_report_summary
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# DualSimulationsSummary ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`DualSimulationsSummary`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print DualSimulationsSummary
knit_print.DualSimulationsSummary <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.SimulationsSummary(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "**Biomarker fit at dose most selected:** ",
    sprintf("%.3f", x@biomarker_fit_at_dose_most_selected),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# PseudoSimulationsSummary ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`PseudoSimulationsSummary`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print PseudoSimulationsSummary
knit_print.PseudoSimulationsSummary <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "### Simulation Summary (", x@nsim, " simulations)\n\n",
    "**Target probability of DLE at end of trial:** ",
    sprintf("%.1f%%", 100 * x@target_end_of_trial),
    "\n\n",
    "**TDEOT:** ", sprintf("%.2f", x@target_dose_end_of_trial),
    " (at dose grid: ", sprintf("%.2f", x@target_dose_end_of_trial_at_dose_grid), ")\n\n",
    "**Target probability of DLE during trial:** ",
    sprintf("%.1f%%", 100 * x@target_during_trial),
    "\n\n",
    "**TDDT:** ", sprintf("%.2f", x@target_dose_during_trial),
    " (at dose grid: ", sprintf("%.2f", x@target_dose_during_trial_at_dose_grid), ")\n\n",
    "**Dose most often selected:** ", x@dose_most_selected,
    " (observed toxicity rate: ", sprintf("%.1f%%", 100 * x@obs_tox_rate_at_dose_most_selected),
    ")\n\n",
    "**Proportion selecting TDEOT:** ", sprintf("%.1f%%", 100 * x@prop_at_target_end_of_trial),
    "\n\n",
    "**Proportion selecting TDDT:** ", sprintf("%.1f%%", 100 * x@prop_at_target_during_trial),
    "\n\n",
    "**Number of patients:** ", x@n_obs, "\n\n",
    "**Patients treated above TDEOT:** ", x@n_above_target_end_of_trial, "\n\n",
    "**Patients treated above TDDT:** ", x@n_above_target_during_trial, "\n\n"
  )

  if (length(x@tdeot_summary) > 0) {
    rv <- paste0(
      rv,
      "\n",
      h_six_number_summary(x@tdeot_summary, caption = "TDEOT Summary"),
      "\n\n"
    )
  }

  if (length(x@tddt_summary) > 0) {
    rv <- paste0(
      rv,
      "\n",
      h_six_number_summary(x@tddt_summary, caption = "TDDT Summary"),
      "\n\n"
    )
  }

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# PseudoDualSimulationsSummary ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.GeneralSimulations
#' @param x (`PseudoDualSimulationsSummary`)\cr the object to be printed.
#' @rdname knit_print
#' @export
#' @method knit_print PseudoDualSimulationsSummary
knit_print.PseudoDualSimulationsSummary <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  base_output <- knit_print.PseudoSimulationsSummary(x, asis = FALSE)

  rv <- paste0(
    base_output,
    "**Target Gstar:** ", sprintf("%.2f", x@target_gstar),
    " (at dose grid: ", sprintf("%.2f", x@target_gstar_at_dose_grid), ")\n\n",
    "**Efficacy fit at dose most selected:** ",
    sprintf("%.3f", x@eff_fit_at_dose_most_selected),
    "\n\n"
  )

  if (length(x@gstar_summary) > 0) {
    rv <- paste0(
      rv,
      "\n",
      h_six_number_summary(x@gstar_summary, caption = "Gstar Summary"),
      "\n\n"
    )
  }

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
