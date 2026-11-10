#' @include report_table.R
NULL

#' Transpose a Reporting Table
#'
#' Exchange displayed rows and columns before rendering with [report_table()].
#' The source object, including unrounded values and layout metadata, is retained;
#' transposing twice returns it unchanged. Statistics are not recalculated.
#'
#' @param x Any reporting object accepted by [report_table()], or a nonempty
#'   list of such objects (including paginated tables).
#' @param ... Display arguments passed to [report_table()], for example `digits`
#'   or label overrides, when capturing the original orientation.
#' @return A `transposed_report`, or a `report_tables` list for list inputs.
#' @details Transposition applies to a flat display table, independently of gt.
#'   Multiple identifiers (for example scenario and statistic in detail tables)
#'   are combined with ` / ` to label the new columns. Row sections and column
#'   spanners are flattened; the original layout is recovered on transposing
#'   twice. Duplicate display labels are permitted and kept distinct internally.
#'   Transpose each page after [report_paginate()] to keep those page boundaries.
#' @examples
#' truths <- ToxicityScenarios(
#'   scenarios = list(Low = function(dose) dose / 10), dose_grid = 1:3
#' )
#' report_table(report_transpose(truths))
#' @export
report_transpose <- function(x, ...) {
  if (inherits(x, "transposed_report")) {
    return(x$source)
  }
  if (inherits(x, "report_tables") || (is.list(x) && !is.object(x))) {
    if (!length(x)) {
      stop("x must be a nonempty list of reporting tables.")
    }
    return(structure(
      lapply(x, report_transpose, ...),
      class = c("report_tables", "list")
    ))
  }
  display <- report_table(x, format = "data.frame", ...)
  structure(list(source = x, display = display), class = "transposed_report")
}

#' Capture a Flat Formatted Reporting Table
#' @param data A wide data frame.
#' @param labels Display column labels, in column order.
#' @param identifiers Integer positions of identifier columns.
#' @param title Optional title or caption.
#' @param digits Optional decimal places for numeric data cells.
#' @param column_label Label for the original column axis after transposition.
#' @return A character data frame with display labels and identifier metadata.
#' @keywords internal
#' @noRd
h_report_display <- function(
  data,
  labels,
  identifiers,
  title = NULL,
  digits = NULL,
  column_label = "Column"
) {
  data <- as.data.frame(data)
  for (i in seq_along(data)) {
    values <- data[[i]]
    if (is.numeric(values) && !is.null(digits) && !i %in% identifiers) {
      values <- formatC(values, format = "f", digits = digits)
    } else {
      values <- as.character(values)
    }
    values[is.na(data[[i]])] <- "\u2014"
    data[[i]] <- values
  }
  names(data) <- make.unique(labels)
  attr(data, "display_labels") <- labels
  attr(data, "identifiers") <- identifiers
  attr(data, "column_label") <- column_label
  attr(data, "title") <- if (!is.null(title) && nzchar(title)) title else NULL
  data
}

#' Render a Transposed Reporting Table
#' @inheritParams report_table
#' @param title Optional title, defaulting to the source table's title.
#' @return A `knitr_kable`, `gt_tbl`, or formatted data frame.
#' @export
report_table.transposed_report <- function(
  x,
  format = c("kable", "gt", "data.frame"),
  ...,
  title = attr(x$display, "title")
) {
  format <- match.arg(format)
  data <- x$display
  identifiers <- attr(data, "identifiers")
  columns <- setdiff(seq_along(data), identifiers)
  labels <- attr(data, "display_labels")
  row_labels <- vapply(
    seq_len(nrow(data)),
    function(i) {
      parts <- unlist(data[i, identifiers, drop = FALSE], use.names = FALSE)
      paste(
        parts[!is.na(parts) & nzchar(parts) & parts != "\u2014"],
        collapse = " / "
      )
    },
    character(1)
  )
  wide <- data.frame(.label = labels[columns], check.names = FALSE)
  for (i in seq_len(nrow(data))) {
    wide[[paste0(".row_", i)]] <- unlist(
      data[i, columns, drop = FALSE],
      use.names = FALSE
    )
  }
  headers <- c(attr(data, "column_label"), row_labels)
  if (format == "data.frame") {
    return(h_report_display(
      wide,
      headers,
      1L,
      title = title,
      column_label = paste(labels[identifiers], collapse = " / ")
    ))
  }
  if (format == "kable") {
    return(knitr::kable(
      wide,
      format = "pipe",
      col.names = headers,
      row.names = FALSE,
      caption = title,
      ...
    ))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Package 'gt' is required for format = 'gt'. Please install it.")
  }
  table <- gt::gt(wide, rowname_col = ".label", ...)
  table <- gt::cols_label(
    table,
    .list = stats::setNames(as.list(row_labels), names(wide)[-1L])
  )
  table <- gt::tab_stubhead(table, label = headers[1L])
  if (!is.null(title) && nzchar(title)) {
    table <- gt::tab_header(table, title = title)
  }
  table
}

#' Render a Transposed Report in Knitr
#' @param x A `transposed_report` object.
#' @param ... Arguments passed to [report_table()].
#' @param options Unused knitr options.
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @export
knit_print.transposed_report <- function(x, ..., options = NULL) {
  h_knit_report_table(x, ...)
}
