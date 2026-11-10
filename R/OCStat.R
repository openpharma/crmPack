#' Define an Operating Characteristic
#'
#' Statistics contain calculation and display instructions, independently of a
#' table renderer. Functions receive the original simulations, their cached
#' summary, and the scenario name. They must not rerun simulation or MCMC.
#'
#' @param label A nonempty row label. For multiple rows it prefixes each name.
#' @param fun Function with arguments `sims`, `summary`, `scenario`, returning a
#'   numeric vector for one cell, or a uniquely named list of numeric vectors
#'   for multiple rows. A vector can hold, for example, a mean and SD.
#' @param format A `sprintf()` format string, or a function taking a numeric
#'   vector and returning one character string per cell. A string is applied
#'   using the vector elements as successive `sprintf()` arguments.
#' @return An `OCStat` S3 object with components `label`, `fun`, and `format`.
#' @examples
#' OCStat("Mean number of patients", function(sims, summary, scenario) {
#'   mean(summary@n_obs)
#' }, format = "%.1f")
#' OCStat("Patients, mean (SD)", function(sims, summary, scenario) {
#'   c(mean(summary@n_obs), sd(summary@n_obs))
#' }, format = "%.1f (%.1f)")
#' @export
OCStat <- function(label, fun, format = "%.1f") {
  assert_string(label, min.chars = 1L)
  if (!nzchar(trimws(label))) {
    stop("label must be nonempty.")
  }
  assert_function(fun)
  if (!is.function(format)) {
    assert_string(format, min.chars = 1L)
  }
  structure(list(label = label, fun = fun, format = format), class = "OCStat")
}

#' Evaluate and Format One Operating Characteristic
#' @param stat An `OCStat` object.
#' @param sims Original simulations.
#' @param summary Cached summary.
#' @param scenario Scenario name.
#' @return Tibble with label, numeric list-column value, and formatted columns.
#' @keywords internal
#' @noRd
h_eval_oc_stat <- function(stat, sims, summary, scenario) {
  assert_class(stat, "OCStat")
  result <- stat$fun(sims = sims, summary = summary, scenario = scenario)
  multiple <- is.list(result)
  if (multiple) {
    h_oc_names(result)
  } else {
    result <- list(result)
  }
  if (
    !all(vapply(
      result,
      function(x) is.numeric(x) && is.null(dim(x)) && length(x) > 0L,
      logical(1)
    ))
  ) {
    stop(
      "OCStat functions must return a numeric vector or a named list of numeric vectors."
    )
  }
  formatted <- vapply(
    result,
    function(value) {
      text <- if (is.function(stat$format)) {
        stat$format(value)
      } else {
        do.call(sprintf, c(list(fmt = stat$format), as.list(unname(value))))
      }
      assert_string(text, na.ok = TRUE)
      text
    },
    character(1)
  )
  tibble::tibble(
    label = if (multiple) paste(stat$label, names(result)) else stat$label,
    value = unname(result),
    formatted = unname(formatted)
  )
}

#' Built-in Operating Characteristics Registry
#'
#' Returns a fresh named list of `OCStat` objects. Select entries by name, change
#' their labels, or mix them with custom `OCStat`s for subsequent table assembly.
#' Nothing is calculated until an entry's `fun` is called.
#'
#' Built-ins include `n_pat_*` and `n_dlt_*` with suffixes `mean`, `mean_sd`,
#' `minmax`, `quantiles`; `prop_stop_tox`, `prop_stop_max_patients`,
#' `prop_stop_by_label`; `prop_mtd_correct`, `prop_mtd_over`, `prop_mtd_under`,
#' `prop_mtd_none`; `dose_selection`; `n_above_target_mean_sd`,
#' `duration_mean_sd`; `n_pat_per_dose`, `n_dlt_per_dose`, `prop_selected`,
#' and `true_prob`. Percentages are on a 0--100 scale, true probabilities on 0--1.
#'
#' `dose_selection` includes every grid dose and no recommendation. A highest
#' dose recommendation is not interpreted as evidence that the highest dose is
#' safe. Duration is taken from `DASimulations@trial_duration` in its original
#' units; it is unavailable for other designs, rather than inferred from cohorts.
#' Stop rows use category unions, so overlapping rules are never double counted.
#' Missing stop reports or class metadata produce `NA` for default category
#' percentages. `prop_stop_by_label` uses the collection's category mapping, or
#' individual labels when that mapping is empty. Missing numeric data propagate.
#'
#' With `x`, scalar numeric `additional_stats` stored for every trial become
#' candidate entries named `additional:<name>`, reporting the per-trial mean.
#' Non-scalar or nonnumeric derived values are not offered. An entry unavailable
#' in a particular scenario evaluates to `NA`.
#'
#' @param x Optional `SimulationsSummaryList`, supplying category mappings and
#'   additional-statistic candidates. Without it, cached summaries supply the
#'   target and class-based stopping defaults are used.
#' @param sd Include SD in per-dose count cells.
#' @param quantiles Increasing pair of probabilities for the quantile variants.
#' @return A named list of `OCStat` objects.
#' @examples
#' names(oc_stat_registry())
#' stat <- oc_stat_registry()$n_pat_mean_sd
#' stat$label <- "Total patients, mean (SD)"
#' @export
oc_stat_registry <- function(x = NULL, sd = FALSE, quantiles = c(0.1, 0.9)) {
  if (!is.null(x)) {
    assert_class(x, "SimulationsSummaryList")
  }
  assert_flag(sd)
  assert_numeric(quantiles, len = 2L, lower = 0, upper = 1, any.missing = FALSE)
  if (quantiles[1L] >= quantiles[2L]) {
    stop("quantiles must be strictly increasing.")
  }
  registry <- list()
  for (quantity in c("pat", "dlt")) {
    for (variant in c("mean", "mean_sd", "minmax", "quantiles")) {
      key <- paste("n", quantity, variant, sep = "_")
      registry[[key]] <- h_oc_count_stat(quantity, variant, quantiles)
    }
  }
  registry$prop_stop_tox <- h_oc_stop_stat("Toxicity", x)
  registry$prop_stop_max_patients <- h_oc_stop_stat("Max patients", x)
  registry$prop_stop_by_label <- h_oc_stop_stat(NULL, x)
  classifications <- c(
    correct = "correct",
    over = "overestimated",
    under = "underestimated",
    none = "none"
  )
  for (key in names(classifications)) {
    registry[[paste0("prop_mtd_", key)]] <- h_oc_mtd_stat(classifications[[
      key
    ]])
  }
  registry$dose_selection <- OCStat(
    "Dose selected:",
    function(sims, summary, scenario) {
      values <- report_dose_selection(sims)
      stats::setNames(
        as.list(values$percent),
        ifelse(is.na(values$dose), "none", as.character(values$dose))
      )
    }
  )
  registry$n_above_target_mean_sd <- OCStat(
    "Patients above target, mean (SD)",
    function(sims, summary, scenario) {
      c(mean(summary@n_above_target), stats::sd(summary@n_above_target))
    },
    "%.1f (%.1f)"
  )
  registry$duration_mean_sd <- OCStat(
    "Trial duration, mean (SD)",
    function(sims, summary, scenario) {
      if (!is(sims, "DASimulations")) {
        return(c(NA_real_, NA_real_))
      }
      c(mean(sims@trial_duration), stats::sd(sims@trial_duration))
    },
    "%.1f (%.1f)"
  )
  registry$n_pat_per_dose <- h_oc_per_dose_stat("pat", sd)
  registry$n_dlt_per_dose <- h_oc_per_dose_stat("dlt", sd)
  registry$prop_selected <- registry$dose_selection
  registry$true_prob <- OCStat(
    "True P(DLT):",
    function(sims, summary, scenario) {
      grid <- h_report_dose_grid(sims)
      stats::setNames(
        as.list(h_toxicity_prob(get_simulation_truth(sims), grid, scenario)),
        as.character(grid)
      )
    },
    "%.3f"
  )
  if (!is.null(x)) {
    candidates <- unique(unlist(
      lapply(x@simulations, function(sims) {
        if (!methods::.hasSlot(sims, "additional_stats")) {
          return(character())
        }
        unique(unlist(lapply(sims@additional_stats, names), use.names = FALSE))
      }),
      use.names = FALSE
    ))
    for (name in candidates) {
      eligible <- any(vapply(
        x@simulations,
        function(sims) !is.null(h_oc_additional_values(sims, name)),
        logical(1)
      ))
      if (eligible) {
        registry[[paste0("additional:", name)]] <- h_oc_additional_stat(name)
      }
    }
  }
  registry
}

#' Construct a Count Summary Statistic
#' @param quantity Patient or DLT counts (`pat` or `dlt`).
#' @param variant Summary display variant.
#' @param quantiles Quantile probabilities.
#' @return An `OCStat`.
#' @keywords internal
#' @noRd
h_oc_count_stat <- function(quantity, variant, quantiles) {
  force(quantity)
  force(variant)
  force(quantiles)
  formats <- c(
    mean = "%.1f",
    mean_sd = "%.1f (%.1f)",
    minmax = "[%.0f, %.0f]",
    quantiles = "[%.1f, %.1f]"
  )
  OCStat(
    paste(if (quantity == "pat") "Patients" else "DLTs", variant),
    function(sims, summary, scenario) {
      if (quantity == "pat") {
        values <- report_sample_size(sims, quantiles)
        columns <- switch(
          variant,
          mean = "mean",
          mean_sd = c("mean", "sd"),
          minmax = c("min", "max"),
          quantiles = paste0("q", quantiles * 100)
        )
        return(unlist(values[columns], use.names = FALSE))
      }
      values <- vapply(sims@data, function(data) sum(data@y), numeric(1))
      switch(
        variant,
        mean = mean(values),
        mean_sd = c(mean(values), stats::sd(values)),
        minmax = range(values),
        quantiles = stats::quantile(values, quantiles, names = FALSE)
      )
    },
    unname(formats[variant])
  )
}

#' Construct a Per-Dose Count Statistic
#' @param quantity Patient or DLT counts.
#' @param sd Include SD in cells.
#' @return An `OCStat`.
#' @keywords internal
#' @noRd
h_oc_per_dose_stat <- function(quantity, sd) {
  force(quantity)
  force(sd)
  OCStat(
    if (quantity == "pat") "Patients at dose" else "DLTs at dose",
    function(sims, summary, scenario) {
      values <- if (quantity == "pat") {
        report_patients_per_dose(sims)
      } else {
        report_dlts_per_dose(sims)
      }
      stats::setNames(
        lapply(seq_len(nrow(values)), function(i) {
          if (sd) c(values$mean[i], values$sd[i]) else values$mean[i]
        }),
        as.character(values$dose)
      )
    },
    if (sd) "%.1f (%.1f)" else "%.1f"
  )
}

#' Construct an MTD Classification Statistic
#' @param category Classification name.
#' @return An `OCStat`.
#' @keywords internal
#' @noRd
h_oc_mtd_stat <- function(category) {
  force(category)
  OCStat(paste("MTD", category, "(%)"), function(sims, summary, scenario) {
    values <- report_mtd_classification(sims, summary@target)
    values$percent[match(category, values$classification)]
  })
}

#' Construct a Stop Activation Statistic
#' @param category Category name or NULL for all labels/categories.
#' @param x Optional summary collection.
#' @return An `OCStat`.
#' @keywords internal
#' @noRd
h_oc_stop_stat <- function(category, x) {
  force(category)
  force(x)
  OCStat(
    if (is.null(category)) "Stop (%) :" else paste("Stop:", category, "(%)"),
    function(sims, summary, scenario) {
      if (!methods::.hasSlot(sims, "stop_report")) {
        return(NA_real_)
      }
      mapping <- if (is.null(x)) {
        h_default_stop_categories(sims@stop_report)
      } else {
        x@stop_categories[[scenario]]
      }
      if (is.null(mapping)) {
        stop("Unknown scenario in statistics registry.")
      }
      values <- report_stop_reasons(sims, mapping)
      if (is.null(category)) {
        if (!nrow(values)) {
          return(NA_real_)
        }
        return(stats::setNames(as.list(values$percent), values$label))
      }
      if (category %in% values$label) {
        return(values$percent[match(category, values$label)])
      }
      classes <- attr(sims@stop_report, "stopping_classes")
      if (
        is.null(classes) ||
          anyNA(classes) ||
          is.null(colnames(sims@stop_report)) ||
          anyNA(colnames(sims@stop_report))
      ) {
        return(NA_real_)
      }
      0
    }
  )
}

#' Extract Scalar Derived Statistics from Every Trial
#' @param sims Original simulations.
#' @param name Derived statistic name.
#' @return Numeric vector, or NULL when unavailable or nonscalar.
#' @keywords internal
#' @noRd
h_oc_additional_values <- function(sims, name) {
  if (
    !methods::.hasSlot(sims, "additional_stats") ||
      length(sims@additional_stats) != length(sims@data)
  ) {
    return(NULL)
  }
  values <- lapply(sims@additional_stats, function(trial) {
    if (!is.list(trial) || is.null(names(trial)) || !name %in% names(trial)) {
      return(NULL)
    }
    trial[[name]]
  })
  if (
    !length(values) ||
      !all(vapply(
        values,
        function(value) {
          is.numeric(value) &&
            is.null(dim(value)) &&
            length(value) == 1L
        },
        logical(1)
      ))
  ) {
    return(NULL)
  }
  unlist(values, use.names = FALSE)
}

#' Construct a Derived-Statistic Mean
#' @param name Derived statistic name.
#' @return An `OCStat`.
#' @keywords internal
#' @noRd
h_oc_additional_stat <- function(name) {
  force(name)
  OCStat(paste(name, "(mean)"), function(sims, summary, scenario) {
    values <- h_oc_additional_values(sims, name)
    if (is.null(values)) NA_real_ else mean(values)
  })
}
