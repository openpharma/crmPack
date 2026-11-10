#' Report Operating Characteristics by Dose
#'
#' Assemble per-dose statistics using the registry and cached scenario summaries.
#' Tables have scenarios as row groups, statistics as rows, and doses as columns.
#' Dose cells include zero counts for unvisited doses; doses absent from a
#' scenario's grid and inapplicable summary cells remain missing.
#'
#' @param x A `SimulationsSummaryList`.
#' @param stats Nonempty character vector or list mixing `OCStat` objects and
#'   built-in names: `n_dlt_per_dose`, `n_pat_per_dose`, `prop_selected` (or
#'   `dose_selection`), and `true_prob`. Names assign unique row IDs, as in
#'   [report_oc_overview()]. Custom functions must return a named list of numeric
#'   vectors keyed by `as.character(dose)` on the scenario's grid; omitted doses
#'   remain missing. An entirely missing numeric vector means unavailable.
#' @param sd Include SD in per-dose patient and DLT counts, and patient SUM cells.
#' @param sum_column Include SUM for the two patient-count rows
#'   (`n_pat_per_dose` and `n_dlt_per_dose`). Its mean and optional SD summarize
#'   per-trial totals, including placebo; dose-level SDs are never added.
#' @param stop_columns Include STOP and MAX only for selection/probability rows.
#'   STOP reports toxicity stopping and MAX maximum-patient stopping, using the
#'   collection's configured categories (`Toxicity` and `Max patients`), exactly
#'   as `prop_stop_tox` and `prop_stop_max_patients` in [oc_stat_registry()].
#'   These count unions of terminal rule activations. They can overlap each other
#'   and dose recommendations; the columns do not form a partition. A missing
#'   recommendation alone does not establish toxicity stopping. Reports without
#'   the necessary metadata yield unavailable stop percentages.
#' @param format Output format; defaults to the unrounded long tibble.
#' @param labels Optional named statistic row-label overrides.
#' @param scenario_labels Optional named scenario group-label overrides.
#' @param column_labels Optional named dose/SUM/STOP/MAX display-label overrides.
#'   Dose keys are `as.character(dose)`.
#' @param title Optional table title (caption for pipe markdown).
#' @param ... Arguments passed to [report_table()] when rendering.
#' @return A `tbl_OCDetail` tibble with `scenario`, `stat`, `label`, `dose`,
#'   `column`, `value`, and `formatted`, or a `knitr_kable` / `gt_tbl`.
#'   `value` is a list-column retaining raw numeric cell components; `dose` is
#'   numeric and missing for SUM/STOP/MAX. Column keys and scenario/statistic IDs
#'   are stable independently of display labels. Dose columns use the sorted
#'   union of the scenario grids, followed by SUM/STOP/MAX when requested.
#' @examples
#' sims <- GeneralSimulations(
#'   data = list(Data(x = c(1, 1), y = c(0, 1), doseGrid = 1:3),
#'               Data(x = c(1, 2), y = c(0, 0), doseGrid = 1:3)),
#'   doses = c(2, 3), seed = 1L, truth = function(dose) dose / 10
#' )
#' scenarios <- SimulationsSummaryList(Low = sims, High = sims)
#' detail <- report_oc_detail(scenarios, sd = TRUE, title = "Per-dose results")
#' report_table(detail, format = "kable")
#' plot(scenarios, type = "doseSelected")
#' plot(scenarios, type = "nPatients")
#' @export
report_oc_detail <- function(
  x,
  stats = c("n_dlt_per_dose", "n_pat_per_dose", "prop_selected", "true_prob"),
  sd = FALSE,
  sum_column = TRUE,
  stop_columns = TRUE,
  format = c("tibble", "gt", "kable"),
  labels = NULL,
  scenario_labels = NULL,
  column_labels = NULL,
  title = NULL,
  ...
) {
  assert_class(x, "SimulationsSummaryList")
  format <- match.arg(format)
  assert_flag(sum_column)
  assert_flag(stop_columns)
  if (!is.null(title)) {
    assert_string(title)
  }
  scenarios <- names(x@simulations)
  registry <- oc_stat_registry(x, sd = sd)
  totals <- registry[c(
    if (sd) "n_pat_mean_sd" else "n_pat_mean",
    if (sd) "n_dlt_mean_sd" else "n_dlt_mean"
  )]
  stop_stats <- registry[c("prop_stop_tox", "prop_stop_max_patients")]
  registry <- registry[c(
    "n_dlt_per_dose",
    "n_pat_per_dose",
    "prop_selected",
    "dose_selection",
    "true_prob"
  )]
  selected <- h_resolve_oc_stats(stats, registry)
  h_oc_display_labels(labels, names(selected))
  h_oc_display_labels(scenario_labels, scenarios)
  grids <- lapply(x@simulations, h_report_dose_grid)
  doses <- sort(unique(unlist(grids, use.names = FALSE)))
  columns <- c(
    as.character(doses),
    if (sum_column) "SUM",
    if (stop_columns) c("STOP", "MAX")
  )
  h_oc_display_labels(column_labels, columns)
  sources <- vapply(
    stats,
    function(stat) if (is.character(stat)) stat else "",
    character(1)
  )
  results <- lapply(scenarios, function(scenario) {
    sims <- x@simulations[[scenario]]
    grid <- grids[[scenario]]
    dplyr::bind_rows(lapply(seq_along(selected), function(i) {
      id <- names(selected)[i]
      stat <- selected[[i]]
      if (id %in% names(labels)) {
        stat$label <- labels[[id]]
      }
      original <- stat$fun
      source <- sources[i]
      stat$fun <- function(sims, summary, scenario) {
        if (source %in% c("prop_selected", "dose_selection")) {
          return(h_oc_detail_selection(
            sims,
            summary,
            scenario,
            stop_stats,
            stop_columns
          ))
        }
        values <- original(sims, summary, scenario)
        if (source %in% c("n_pat_per_dose", "n_dlt_per_dose") && sum_column) {
          total <- totals[[if (source == "n_pat_per_dose") 1L else 2L]]
          values$SUM <- total$fun(sims, summary, scenario)
        }
        values
      }
      values <- h_eval_oc_scenario(
        stat,
        sims,
        x@summaries[[scenario]],
        scenario,
        id
      )
      allowed <- c(
        as.character(grid),
        if (source %in% c("n_pat_per_dose", "n_dlt_per_dose") && sum_column) {
          "SUM"
        },
        if (source %in% c("prop_selected", "dose_selection") && stop_columns) {
          c("STOP", "MAX")
        }
      )
      if (identical(values$row, "") && all(is.na(unlist(values$value)))) {
        values <- values[FALSE, ]
      } else if (!all(values$row %in% allowed)) {
        stop(sprintf(
          "Statistic '%s', scenario '%s': return a named list keyed by doses on this scenario's grid.",
          id,
          scenario
        ))
      }
      missing_stop <- values$row %in%
        c("STOP", "MAX") &
        vapply(values$value, function(cell) all(is.na(cell)), logical(1))
      values$formatted[missing_stop] <- NA_character_
      index <- match(columns, values$row)
      cells <- lapply(index, function(j) {
        if (is.na(j)) NA_real_ else values$value[[j]]
      })
      text <- values$formatted[index]
      row_label <- stat$label
      tibble::tibble(
        scenario = scenario,
        stat = id,
        label = row_label,
        dose = c(doses, rep(NA_real_, length(columns) - length(doses))),
        column = columns,
        value = cells,
        formatted = text
      )
    }))
  })
  result <- dplyr::bind_rows(results)
  class(result) <- c("tbl_OCDetail", class(result))
  attr(result, "scenarios") <- scenarios
  attr(result, "stats") <- names(selected)
  attr(result, "columns") <- columns
  attr(result, "scenario_labels") <- h_oc_headers(
    scenarios,
    overrides = scenario_labels
  )
  attr(result, "column_labels") <- h_oc_headers(
    columns,
    overrides = column_labels
  )
  attr(result, "title") <- title
  if (format == "tibble") {
    return(result)
  }
  report_table(result, format = format, ...)
}

#' Collect Selection and Terminal Stopping Percentages for Detail Tables
#' @param sims Original simulations.
#' @param summary Cached summary.
#' @param scenario Scenario name.
#' @param stop_stats Configured toxicity and maximum-patient registry entries.
#' @param stop_columns Include STOP/MAX results.
#' @return Named list of percentages by dose and optional STOP/MAX categories.
#' @keywords internal
#' @noRd
h_oc_detail_selection <- function(
  sims,
  summary,
  scenario,
  stop_stats,
  stop_columns
) {
  values <- report_dose_selection(sims)
  result <- stats::setNames(
    as.list(values$percent[-nrow(values)]),
    as.character(values$dose[-nrow(values)])
  )
  if (stop_columns) {
    result$STOP <- stop_stats$prop_stop_tox$fun(sims, summary, scenario)
    result$MAX <- stop_stats$prop_stop_max_patients$fun(sims, summary, scenario)
  }
  result
}

#' Apply Stored and Explicit Display Headers
#' @param keys Original column or scenario keys.
#' @param stored Optional stored labels.
#' @param overrides Optional named label overrides.
#' @return Named character vector in key order.
#' @keywords internal
#' @noRd
h_oc_headers <- function(keys, stored = NULL, overrides = NULL) {
  h_oc_display_labels(overrides, keys)
  headers <- stats::setNames(keys, keys)
  shared <- intersect(names(stored), keys)
  headers[shared] <- stored[shared]
  headers[names(overrides)] <- overrides
  headers
}

#' Render Per-Dose Operating Characteristics
#'
#' Render stored cells with doses as columns, statistics as rows and scenarios
#' as row groups. Pipe markdown repeats the scenario label in its own column;
#' gt uses native row groups. Reordering the skeleton preserves configured order.
#' Missing formatted cells display as an em dash; existing cells keep their
#' statistic's formatting for missing components.
#' @param x A `tbl_OCDetail` tibble from [report_oc_detail()].
#' @param format Renderer: `"kable"`, `"gt"`, or formatted `"data.frame"`.
#' @param ... Arguments passed to `knitr::kable()` or `gt::gt()`.
#' @param title Optional title, defaulting to the stored title. `NULL` removes it.
#' @param scenario_labels,column_labels Optional named display-label overrides.
#' @return A `knitr_kable`, `gt_tbl`, or formatted data frame.
#' @export
report_table.tbl_OCDetail <- function(
  x,
  format = c("kable", "gt", "data.frame"),
  ...,
  title = attr(x, "title"),
  scenario_labels = NULL,
  column_labels = NULL
) {
  format <- match.arg(format)
  if (!is.null(title)) {
    assert_string(title)
  }
  scenarios <- h_oc_present_order(attr(x, "scenarios"), x$scenario)
  ids <- h_oc_present_order(attr(x, "stats"), x$stat)
  columns <- h_oc_present_order(attr(x, "columns"), x$column)
  if (
    !length(scenarios) ||
      !length(ids) ||
      !length(columns) ||
      anyNA(x$scenario) ||
      anyNA(x$stat) ||
      anyNA(x$column) ||
      anyDuplicated(x[c("scenario", "stat", "column")]) ||
      nrow(x) != length(scenarios) * length(ids) * length(columns)
  ) {
    stop(
      "The detail table requires exactly one row per scenario, statistic, and column."
    )
  }
  labels <- vapply(
    ids,
    function(id) {
      label <- unique(x$label[x$stat == id])
      if (length(label) != 1L || is.na(label)) {
        stop("Each statistic must have one consistent label across scenarios.")
      }
      label
    },
    character(1)
  )
  groups <- h_oc_headers(scenarios, attr(x, "scenario_labels"), scenario_labels)
  headers <- h_oc_headers(columns, attr(x, "column_labels"), column_labels)
  wide <- data.frame(
    .scenario = rep(scenarios, each = length(ids)),
    .label = rep(unname(labels), length(scenarios)),
    check.names = FALSE
  )
  cells <- paste0(".column_", seq_along(columns))
  # Integer component keys avoid collisions with punctuation in user names.
  keys <- paste(
    match(x$scenario, scenarios),
    match(x$stat, ids),
    match(x$column, columns),
    sep = ":"
  )
  for (i in seq_along(columns)) {
    wanted <- paste(
      rep(seq_along(scenarios), each = length(ids)),
      rep(seq_along(ids), length(scenarios)),
      i,
      sep = ":"
    )
    values <- x$formatted[match(wanted, keys)]
    wide[[cells[i]]] <- ifelse(is.na(values), "\u2014", values)
  }
  if (format == "data.frame") {
    wide$.scenario <- unname(groups[wide$.scenario])
    return(h_report_display(
      wide,
      c("Scenario", "Statistic", unname(headers)),
      identifiers = 1:2,
      title = title,
      column_label = "Dose / summary"
    ))
  }
  if (format == "kable") {
    wide$.scenario <- unname(groups[wide$.scenario])
    defaults <- list(
      x = wide,
      format = "pipe",
      row.names = FALSE,
      col.names = c("Scenario", "Statistic", unname(headers)),
      align = c("l", "l", rep("r", length(columns)))
    )
    if (!is.null(title)) {
      defaults$caption <- title
    }
    args <- list(...)
    defaults[names(args)] <- args
    return(do.call(knitr::kable, defaults))
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for format = 'gt'. Please install it.",
      call. = FALSE
    )
  }
  table <- gt::gt(
    wide,
    rowname_col = ".label",
    ...
  )
  table <- gt::cols_hide(table, columns = ".scenario")
  table <- gt::cols_label(
    table,
    .list = stats::setNames(as.list(unname(headers)), cells)
  )
  # Group IDs remain original scenario names even if display labels coincide.
  for (scenario in scenarios) {
    table <- gt::tab_row_group(
      table,
      label = groups[[scenario]],
      rows = which(wide$.scenario == scenario),
      id = scenario
    )
  }
  table <- gt::row_group_order(table, groups = scenarios)
  table <- gt::cols_align(table, align = "right", columns = cells)
  if (!is.null(title)) {
    table <- gt::tab_header(table, title = title)
  }
  table
}

#' Preserve Configured Order for Retained Keys
#' @param configured Original ordered keys.
#' @param present Keys present in a modified skeleton.
#' @return Ordered unique keys.
#' @keywords internal
#' @noRd
h_oc_present_order <- function(configured, present) {
  c(intersect(configured, present), setdiff(unique(present), configured))
}

#' Render Per-Dose Operating Characteristics in Knitr
#' @param x A `tbl_OCDetail` tibble.
#' @param ... Arguments passed to [report_table()].
#' @param options Knitr chunk options (unused).
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @export
knit_print.tbl_OCDetail <- function(x, ..., options = NULL) {
  h_knit_report_table(x, ...)
}
