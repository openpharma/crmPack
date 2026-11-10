#' Report Evaluated Hypothetical Data Scenarios
#'
#' Bind evaluated scenarios without rerunning MCMC. Raw probabilities and proposed
#' doses remain available after stopping; rendered tables show Stop and suppress
#' proposed-dose probabilities for stopped arms.
#' @param x A DataScenario or a uniquely named nonempty list of them.
#' @param probabilities Probability names, or NULL to use those shared by all
#'   evaluated arms and scenarios.
#' @param probability_labels Named display labels for probability columns.
#' @param format Output format; defaults to an unrounded tibble.
#' @param ... Arguments to [tidy()] (dose display options).
#' @param digits Probability decimal places for rendering.
#' @param column_labels Named display labels for rendered columns.
#' @return A tbl_DataScenarios tibble, knitr_kable, or gt_tbl.
#' @export
report_data_scenarios <- function(
  x,
  probabilities = NULL,
  probability_labels = NULL,
  format = c("tibble", "gt", "kable"),
  ...,
  digits = 3L,
  column_labels = NULL
) {
  format <- match.arg(format)
  if (is(x, "DataScenario")) {
    x <- list(Scenario = x)
  }
  if (
    !is.list(x) ||
      !length(x) ||
      !all(vapply(x, function(z) is(z, "DataScenario"), logical(1)))
  ) {
    stop("x must be a DataScenario or a named list of DataScenario objects.")
  }
  assert_names(names(x), type = "unique")
  if (is.null(names(x)) || any(!nzchar(trimws(names(x))))) {
    stop("Scenario names must be nonempty.")
  }
  values <- lapply(x, function(z) tidy(z, probabilities = probabilities, ...))
  if (is.null(probabilities)) {
    evaluated <- vapply(
      x,
      function(z) any(!vapply(z@next_best, is.null, logical(1))),
      logical(1)
    )
    probabilities <- if (any(evaluated)) {
      Reduce(intersect, lapply(values[evaluated], attr, "probabilities"))
    } else {
      character()
    }
    values <- lapply(values, function(z) {
      z[setdiff(attr(z, "probabilities"), probabilities)] <- NULL
      attr(z, "probabilities") <- probabilities
      z
    })
  }
  rows <- lapply(seq_along(values), function(i) {
    tibble::add_column(
      tibble::as_tibble(values[[i]]),
      scenario = names(x)[i],
      .before = 1L
    )
  })
  result <- dplyr::bind_rows(rows)
  class(result) <- c("tbl_DataScenarios", class(result))
  attr(result, "probabilities") <- probabilities
  attr(result, "hierarchical") <- any(vapply(
    values,
    function(z) isTRUE(attr(z, "hierarchical")),
    logical(1)
  ))
  if (format == "tibble") {
    return(result)
  }
  report_table(
    result,
    format = format,
    digits = digits,
    probability_labels = probability_labels,
    column_labels = column_labels
  )
}

#' Render Evaluated Data Scenarios
#' @param x A DataScenario or a classed scenario tibble.
#' @param format Renderer.
#' @param ... Renderer arguments.
#' @param digits Probability decimal places.
#' @param probability_labels,column_labels Named display labels.
#' @return A rendered table.
#' @export
report_table.tbl_DataScenarios <- function(
  x,
  format = c("kable", "gt"),
  ...,
  digits = 3L,
  probability_labels = NULL,
  column_labels = NULL
) {
  format <- match.arg(format)
  assert_count(digits)
  probs <- attr(x, "probabilities")
  hierarchical <- isTRUE(attr(x, "hierarchical"))
  first <- !duplicated(x[c("scenario", "arm")])
  action <- ifelse(
    x$stop %in% TRUE,
    paste0(
      "Stop",
      ifelse(is.na(x$stop_reason), "", paste0(": ", x$stop_reason))
    ),
    ifelse(
      is.na(x$stop),
      x$stop_reason,
      ifelse(
        is.na(x$proposed_next_dose_display),
        NA_character_,
        paste0(x$proposed_next_dose_display, " (n = ", x$cohort_size, ")")
      )
    )
  )
  display <- data.frame(
    scenario = x$scenario,
    arm = x$arm,
    observed_dose = x$observed_dose_display,
    n_patients = x$n_patients,
    n_dlts = x$n_dlts,
    current_dose = x$current_dose_display,
    current_overdose_prob = x$current_overdose_prob,
    next_action = action,
    check.names = FALSE
  )
  for (p in probs) {
    display[[p]] <- ifelse(x$stop %in% TRUE, NA_real_, x[[p]])
  }
  decision <- c("current_dose", "current_overdose_prob", "next_action", probs)
  for (column in c("current_overdose_prob", probs)) {
    display[[column]] <- ifelse(
      is.na(display[[column]]),
      "—",
      formatC(display[[column]], format = "f", digits = digits)
    )
  }
  display[!first, decision] <- ""
  display[is.na(display)] <- "—"
  if (!hierarchical) {
    display$arm <- NULL
  }
  labels <- c(
    scenario = "Scenario",
    arm = "Arm",
    observed_dose = "Dose",
    n_patients = "Patients",
    n_dlts = "DLTs",
    current_dose = "Current dose",
    current_overdose_prob = "Current P(ODS)",
    next_action = "Next action",
    stats::setNames(probs, probs)
  )
  for (custom in list(probability_labels, column_labels)) {
    if (length(custom)) {
      assert_character(custom, any.missing = FALSE)
      assert_names(names(custom), subset.of = names(labels))
      labels[names(custom)] <- custom
    }
  }
  if (format == "kable") {
    names(display) <- unname(labels[names(display)])
    return(knitr::kable(display, format = "pipe", row.names = FALSE, ...))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for format = 'gt'. Please install it.")
  }
  table <- gt::gt(display, groupname_col = "scenario", ...)
  table <- gt::cols_label(
    table,
    .list = as.list(labels[setdiff(names(display), "scenario")])
  )
  table <- gt::tab_spanner(
    table,
    label = "Observed data",
    columns = c("observed_dose", "n_patients", "n_dlts")
  )
  if (length(probs)) {
    table <- gt::tab_spanner(
      table,
      label = "Interval probabilities of proposed next dose",
      columns = probs
    )
  }
  table
}

#' @rdname report_table.tbl_DataScenarios
#' @export
report_table.tbl_DataScenario <- function(x, format = c("kable", "gt"), ...) {
  x$scenario <- "Scenario"
  report_table.tbl_DataScenarios(x, format = format, ...)
}

#' @rdname report_table.tbl_DataScenarios
#' @export
report_table.DataScenario <- function(x, format = c("kable", "gt"), ...) {
  report_table(tidy(x), format = format, ...)
}

#' Knit Evaluated Data Scenarios
#' @param x Evaluated scenario or classed tidy result.
#' @param ... Arguments passed to report_table.
#' @param options Unused knitr options.
#' @return Markdown as asis_output.
#' @export
knit_print.DataScenario <- function(x, ..., options = NULL) {
  knitr::asis_output(paste(
    report_table(x, format = "kable", ...),
    collapse = "\n"
  ))
}

#' @rdname knit_print.DataScenario
#' @export
knit_print.tbl_DataScenario <- function(x, ..., options = NULL) {
  knit_print.DataScenario(x, ..., options = options)
}

#' @rdname knit_print.DataScenario
#' @export
knit_print.tbl_DataScenarios <- function(x, ..., options = NULL) {
  knit_print.DataScenario(x, ..., options = options)
}
