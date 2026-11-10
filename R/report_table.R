#' Render a Reporting Table
#'
#' Render reporting objects or their classed tidy tibbles. Toxicity scenario
#' tables have scenarios as rows and doses as columns. Numeric probabilities
#' are rounded only for display. Use [tidy()] to obtain unrounded long data.
#'
#' @param x A reporting object or classed reporting tibble.
#' @param format Renderer: `"kable"` (pipe markdown, the default) or `"gt"`
#'   (requires the suggested package `gt`).
#' @param ... Arguments passed to the renderer, such as `caption` for kable.
#' @return A `knitr_kable` or `gt_tbl` object.
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
  x, format = c("kable", "gt"), ..., digits = 3L
) {
  format <- match.arg(format)
  assert_count(digits)
  scenarios <- unique(x$scenario)
  doses <- unique(x$dose)
  # Index explicitly so reordering the long tibble does not change cell pairing.
  keys <- paste(match(x$scenario, scenarios), match(x$dose, doses), sep = ":")
  if (anyDuplicated(keys) || length(keys) != length(scenarios) * length(doses)) {
    stop("The toxicity scenario table requires exactly one row per scenario and dose.")
  }
  wide <- data.frame(Scenario = scenarios, check.names = FALSE)
  for (i in seq_along(doses)) {
    wide[[paste0("Dose ", doses[i])]] <- x$prob[
      match(paste(seq_along(scenarios), i, sep = ":"), keys)
    ]
  }
  if (format == "kable") {
    return(knitr::kable(wide, format = "pipe", digits = digits,
                        format.args = list(nsmall = digits), row.names = FALSE, ...))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for format = 'gt'. Please install it.", call. = FALSE)
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
  knitr::asis_output(paste(report_table(x, format = "kable", ...), collapse = "\n"))
}

#' @rdname knit_print.ToxicityScenarios
#' @export
knit_print.tbl_ToxicityScenarios <- function(x, ..., options = NULL) {
  knit_print.ToxicityScenarios(x, ..., options = options)
}
