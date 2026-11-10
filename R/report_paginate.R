#' Split Wide Reporting Tables into Column Pages
#'
#' Split stored table data without recomputing statistics. The table class,
#' raw values, headings and display labels are preserved on each part.
#' Parts render consecutively in R Markdown and Quarto; physical page breaks
#' depend on the output document and are not inserted by this function.
#'
#' @details This S3 generic dispatches on the reporting table class, as does
#'   [report_table()]. Methods for `ToxicityScenarios` and `DataScenario`
#'   delegate to the corresponding tidy reporting table.
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
#' @param ... Further arguments passed to class-specific methods.
#' @return A nonempty `report_tables` list of classed reporting tibbles, each
#'   accepted by [report_table()]. A table already within the limit yields
#'   one part. `max_cols` must fit the repeated columns and at least one
#'   remaining column, if any.
#' @examples
#' truths <- ToxicityScenarios(dose_grid = 1:8, scenarios = list(Low = function(dose) dose / 20))
#' pages <- report_paginate(truths, max_cols = 4)
#' report_table(pages)
#' @export
report_paginate <- function(x, max_cols = 6L, keep = "label", ...) {
  UseMethod("report_paginate")
}

#' @rdname report_paginate
#' @export
report_paginate.default <- function(x, max_cols = 6L, keep = "label", ...) {
  stop("x must be a supported classed reporting table.")
}

#' @rdname report_paginate
#' @export
report_paginate.ToxicityScenarios <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  report_paginate(tidy(x), max_cols = max_cols, keep = keep, ...)
}

#' @rdname report_paginate
#' @export
report_paginate.DataScenario <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  report_paginate(tidy(x), max_cols = max_cols, keep = keep, ...)
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_OCOverview <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  h_report_paginate(
    columns = h_oc_present_order(attr(x, "scenarios"), x$scenario),
    identifiers = "label",
    max_cols = max_cols,
    keep = keep,
    subset = function(selected) {
      part <- x[x$scenario %in% selected, ]
      attr(part, "scenarios") <- selected
      part
    }
  )
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_OCDetail <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  h_report_paginate(
    columns = h_oc_present_order(attr(x, "columns"), x$column),
    identifiers = c("scenario", "label"),
    max_cols = max_cols,
    keep = keep,
    subset = function(selected) {
      part <- x[x$column %in% selected, ]
      attr(part, "columns") <- selected
      part
    }
  )
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_ToxicityScenarios <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  h_report_paginate(
    columns = as.character(unique(x$dose)),
    identifiers = "scenario",
    max_cols = max_cols,
    keep = keep,
    subset = function(selected) x[as.character(x$dose) %in% selected, ]
  )
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_ProbSummary <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  h_report_paginate(
    columns = setdiff(names(x), "dose"),
    identifiers = "dose",
    max_cols = max_cols,
    keep = keep,
    subset = function(selected) x[c("dose", selected)]
  )
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_DataScenario <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
  report_paginate.tbl_DataScenarios(x, max_cols = max_cols, keep = keep, ...)
}

#' @rdname report_paginate
#' @export
report_paginate.tbl_DataScenarios <- function(
  x,
  max_cols = 6L,
  keep = "label",
  ...
) {
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
  if (!is.null(attr(x, "report_columns"))) {
    columns <- intersect(columns, attr(x, "report_columns"))
  }
  h_report_paginate(
    columns = columns,
    identifiers = identifiers,
    max_cols = max_cols,
    keep = keep,
    subset = function(selected) {
      part <- x
      attr(part, "report_columns") <- c(identifiers, selected)
      part
    }
  )
}

#' Split Column Keys and Construct Reporting Pages
#' @param columns Ordered keys of the columns that can be split.
#' @param identifiers Mandatory repeated identifier keys.
#' @param max_cols Maximum rendered columns, including repeated columns.
#' @param keep Extra column keys to repeat; `"label"` denotes identifiers.
#' @param subset Function taking selected column keys and returning a classed
#'   reporting tibble with the corresponding data and display metadata.
#' @return A `report_tables` list of reporting tibbles.
#' @keywords internal
#' @noRd
h_report_paginate <- function(columns, identifiers, max_cols, keep, subset) {
  assert_count(max_cols, positive = TRUE)
  assert_character(keep, any.missing = FALSE, unique = TRUE)
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
    subset(columns[columns %in% c(repeated, group)])
  })
  names(parts) <- NULL
  structure(parts, class = c("report_tables", "list"))
}
