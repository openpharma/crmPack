#' Render a Reporting Table
#'
#' Render reporting objects or their classed tidy tibbles. Toxicity scenario
#' tables have scenarios as rows and doses as columns. Numeric probabilities
#' are rounded only for display. Use [tidy()] to obtain unrounded long data.
#'
#' @param x A reporting object, classed reporting tibble, or nonempty list
#'   of these.
#' @param format Renderer: `"kable"` (pipe markdown, the default) or `"gt"`
#'   (requires the suggested package `gt`), or `"data.frame"` for a flat
#'   display table with rounded strings and identifier metadata.
#' @param ... Arguments passed to the renderer, such as `caption` for kable.
#' @return A `knitr_kable`, `gt_tbl`, or formatted data frame. For a list of reporting
#'   tables, a `report_tables` list of rendered tables, displayed consecutively
#'   by knitr. See [report_paginate()] to split wide tables.
#' @details Bare reporting objects in R Markdown or Quarto use pipe markdown,
#'   including for Word output. For a styled standalone Word table, use
#'   `report_table(x, format = "gt") |> gt::gtsave("table.docx")`. This
#'   requires `gt`, `rmarkdown`, and Pandoc. Save each rendered part separately
#'   when exporting a paginated table.
#' @seealso [report_paginate()], [gt::gtsave()]
#' @export
report_table <- function(x, format = c("kable", "gt", "data.frame"), ...) {
  UseMethod("report_table")
}

#' @rdname report_table
#' @export
report_table.ToxicityScenarios <- function(
  x,
  format = c("kable", "gt", "data.frame"),
  ...
) {
  report_table(tidy(x), format = format, ...)
}

#' @rdname report_table
#' @param digits Non-negative integer number of decimal places for probabilities.
#' @export
report_table.tbl_ToxicityScenarios <- function(
  x,
  format = c("kable", "gt", "data.frame"),
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
  if (format == "data.frame") {
    return(h_report_display(
      wide,
      names(wide),
      1L,
      digits = digits,
      column_label = "Dose"
    ))
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
report_table.list <- function(x, format = c("kable", "gt", "data.frame"), ...) {
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
