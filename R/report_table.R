#' Render a Reporting Table
#'
#' Render reporting objects or their classed tidy tibbles. Toxicity scenario
#' tables have scenarios as rows and doses as columns. Numeric probabilities
#' are rounded only for display. Use [tidy()] to obtain unrounded long data.
#'
#' @param x A reporting object, classed reporting tibble, or nonempty list
#'   of these.
#' @param format Renderer: `"kable"` (pipe markdown, the default) or `"gt"`
#'   (requires the suggested package `gt`).
#' @param ... Arguments passed to the renderer, such as `caption` for kable.
#' @return A `knitr_kable` or `gt_tbl` object. For a nonempty list of reporting
#'   tables, a `report_tables` list of rendered tables, displayed consecutively
#'   by knitr. See [report_paginate()] to split wide tables.
#' @details Bare reporting objects in R Markdown or Quarto use pipe markdown,
#'   including for Word output. For a styled standalone Word table, use
#'   `report_table(x, format = "gt") |> gt::gtsave("table.docx")`. This
#'   requires `gt`, `rmarkdown`, and Pandoc. Save each rendered part separately
#'   when exporting a paginated table.
#' @seealso [report_paginate()], [gt::gtsave()]
#' @export
report_table <- function(x, format = c("kable", "gt"), ...) {
  UseMethod("report_table")
}

#' @rdname report_table
#' @export
report_table.ToxicityScenarios <- function(x, format = c("kable", "gt"), ...) {
  report_table(tidy(x), format = format, ...)
}

#' @rdname report_table
#' @param digits Non-negative integer number of decimal places for probabilities.
#' @export
report_table.tbl_ToxicityScenarios <- function(
  x,
  format = c("kable", "gt"),
  ...,
  digits = 3L
) {
  format <- match.arg(format)
  assert_count(digits)
  scenarios <- unique(x$scenario)
  doses <- unique(x$dose)
  # Index explicitly so reordering the long tibble does not change cell pairing.
  keys <- paste(match(x$scenario, scenarios), match(x$dose, doses), sep = ":")
  if (
    anyDuplicated(keys) || length(keys) != length(scenarios) * length(doses)
  ) {
    stop(
      "The toxicity scenario table requires exactly one row per scenario and dose."
    )
  }
  wide <- data.frame(Scenario = scenarios, check.names = FALSE)
  for (i in seq_along(doses)) {
    wide[[paste0("Dose ", doses[i])]] <- x$prob[
      match(paste(seq_along(scenarios), i, sep = ":"), keys)
    ]
  }
  if (format == "kable") {
    return(knitr::kable(
      wide,
      format = "pipe",
      digits = digits,
      format.args = list(nsmall = digits),
      row.names = FALSE,
      ...
    ))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for format = 'gt'. Please install it.",
      call. = FALSE
    )
  }
  table <- gt::gt(wide, ...)
  gt::fmt_number(table, columns = seq_along(doses) + 1L, decimals = digits)
}

#' Render True Toxicity Scenarios in Knitr
#'
#' @param x A `ToxicityScenarios` object or its tidy tibble.
#' @param ... Arguments passed to [report_table()].
#' @param options Knitr chunk options (unused).
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @export
knit_print.ToxicityScenarios <- function(x, ..., options = NULL) {
  h_knit_report_table(x, ...)
}

#' @rdname knit_print.ToxicityScenarios
#' @export
knit_print.tbl_ToxicityScenarios <- function(x, ..., options = NULL) {
  knit_print.ToxicityScenarios(x, ..., options = options)
}

#' @rdname report_table
#' @export
report_table.list <- function(x, format = c("kable", "gt"), ...) {
  format <- match.arg(format)
  if (!length(x)) {
    stop("x must be a nonempty list of reporting tables.")
  }
  result <- lapply(x, report_table, format = format, ...)
  structure(result, class = c("report_tables", "list"))
}

#' Knit Consecutive Reporting Tables
#' @param x A `report_tables` list from [report_paginate()] or [report_table()].
#' @param ... Arguments passed to each table's `knit_print()` method.
#' @param options Knitr chunk options.
#' @return Consecutive tables wrapped in `knitr::asis_output()`, retaining
#'   metadata from the individual rendered tables.
#' @export
knit_print.report_tables <- function(x, ..., options = NULL) {
  parts <- lapply(x, knitr::knit_print, ..., options = options)
  meta <- unlist(lapply(parts, attr, "knit_meta"), recursive = FALSE)
  knitr::asis_output(
    paste(
      vapply(
        parts,
        function(part) trimws(paste(part, collapse = "\n")),
        character(1)
      ),
      collapse = "\n\n"
    ),
    meta = meta
  )
}

#' Split Wide Reporting Tables into Column Pages
#'
#' Split stored table data without recomputing statistics. The table class,
#' raw values, headings and display labels are preserved on each part.
#' Parts render consecutively in R Markdown and Quarto; physical page breaks
#' depend on the output document and are not inserted by this function.
#'
#' @param x A reporting tibble or a `ToxicityScenarios` or `DataScenario` object.
#' @param max_cols Maximum rendered columns per part, including repeated
#'   identifier columns. For detail tables, the scenario column counts even
#'   when gt renders it as a row group.
#' @param keep Extra column keys to repeat on every part. The default `"label"`
#'   denotes the table's mandatory identifiers: statistic for an overview,
#'   scenario and statistic for detail, scenario for toxicity scenarios, dose
#'   for probability summaries, and scenario/arm/observed dose for data scenarios.
#'   Identifiers always repeat. Other keys are original scenario names for an
#'   overview, dose/SUM/STOP/MAX keys for detail, character doses for toxicity
#'   scenarios, or original column names for probability and data scenarios.
#' @return A nonempty `report_tables` list of classed reporting tibbles, each
#'   accepted by [report_table()]. A table already within the limit yields
#'   one part. `max_cols` must fit the repeated columns and at least one
#'   remaining column, if any.
#' @examples
#' truths <- ToxicityScenarios(dose_grid = 1:8, scenarios = list(Low = function(dose) dose / 20))
#' pages <- report_paginate(truths, max_cols = 4)
#' report_table(pages)
#' @export
report_paginate <- function(x, max_cols = 6L, keep = "label") {
  assert_count(max_cols, positive = TRUE)
  assert_character(keep, any.missing = FALSE, unique = TRUE)
  if (is(x, "ToxicityScenarios") || is(x, "DataScenario")) {
    x <- tidy(x)
  }
  if (inherits(x, "tbl_OCOverview")) {
    key <- "scenario"
    columns <- h_oc_present_order(attr(x, "scenarios"), x$scenario)
    identifiers <- "label"
  } else if (inherits(x, "tbl_OCDetail")) {
    key <- "column"
    columns <- h_oc_present_order(attr(x, "columns"), x$column)
    identifiers <- c("scenario", "label")
  } else if (inherits(x, "tbl_ToxicityScenarios")) {
    key <- "dose"
    columns <- as.character(unique(x$dose))
    identifiers <- "scenario"
  } else if (inherits(x, "tbl_ProbSummary")) {
    key <- NULL
    identifiers <- "dose"
    columns <- setdiff(names(x), identifiers)
  } else if (
    inherits(x, "tbl_DataScenarios") || inherits(x, "tbl_DataScenario")
  ) {
    key <- NULL
    identifiers <- c(
      "scenario",
      if (isTRUE(attr(x, "hierarchical"))) "arm",
      "observed_dose"
    )
    columns <- c(
      "n_patients",
      "n_dlts",
      "current_dose",
      "current_overdose_prob",
      "next_action",
      attr(x, "probabilities")
    )
  } else {
    stop("x must be a supported classed reporting table.")
  }
  if (!is.null(attr(x, "report_columns"))) {
    columns <- intersect(columns, attr(x, "report_columns"))
  }
  keep <- setdiff(keep, "label")
  if (!all(keep %in% c(identifiers, columns))) {
    stop(
      "Unknown keep column: ",
      paste(setdiff(keep, c(identifiers, columns)), collapse = ", ")
    )
  }
  repeated <- intersect(columns, keep)
  remaining <- setdiff(columns, repeated)
  capacity <- max_cols - length(identifiers) - length(repeated)
  if (capacity < as.integer(length(remaining) > 0L)) {
    stop("max_cols must leave room for at least one non-repeated column.")
  }
  groups <- if (length(remaining)) {
    split(remaining, ceiling(seq_along(remaining) / capacity))
  } else {
    list(character())
  }
  parts <- lapply(groups, function(group) {
    selected <- columns[columns %in% c(repeated, group)]
    if (!is.null(key)) {
      part <- x[as.character(x[[key]]) %in% selected, ]
      if (inherits(x, "tbl_OCOverview")) {
        attr(part, "scenarios") <- selected
      }
      if (inherits(x, "tbl_OCDetail")) attr(part, "columns") <- selected
    } else if (inherits(x, "tbl_ProbSummary")) {
      part <- x[c(identifiers, selected)]
    } else {
      part <- x
      attr(part, "report_columns") <- c(identifiers, selected)
    }
    part
  })
  names(parts) <- NULL
  structure(parts, class = c("report_tables", "list"))
}

#' Render a Single Reporting Table as Pipe Markdown for Knitr
#' @param x A reporting object or classed reporting tibble.
#' @param ... Arguments passed to [report_table()].
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @keywords internal
#' @noRd
h_knit_report_table <- function(x, ...) {
  knitr::asis_output(paste(
    report_table(x, format = "kable", ...),
    collapse = "\n"
  ))
}
