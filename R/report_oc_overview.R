#' Report Operating Characteristics Across Scenarios
#'
#' Assemble selected statistics from a [SimulationsSummaryList()] without
#' recomputing its summaries or running MCMC. Rendered tables have statistics
#' as rows and scenarios as columns by default. Use `layout = "scenarios"` to
#' place scenarios in rows with automatic statistic headers and column groups.
#' Arm sub-columns are not yet supported.
#'
#' @param x A `SimulationsSummaryList`.
#' @param stats Nonempty character vector of [oc_stat_registry()] names, or a
#'   list mixing those names and [OCStat()] objects. Optional names assign
#'   unique statistic IDs; unnamed custom statistics get IDs `custom1`, etc.,
#'   using their position in `stats`. Order determines statistic order.
#' @param format Output format; defaults to the long unrounded tibble.
#' @param labels Optional named character vector or named list of statistic-label
#'   overrides, keyed by the names in `stats`. Use a string to rename a statistic,
#'   or a named character vector inside a list to rename its individual rows,
#'   e.g. `list(prop_stop_by_label = c("Lowest dose unsafe" = "Unsafe"))`.
#'   A statistic-level string also supplies the prefix for its expanded rows.
#'   Flat expanded row IDs are also accepted.
#' @param scenario_labels Optional named character vector of scenario display-label
#'   overrides. Names identify original scenarios; partial overrides are allowed.
#' @param title,spanner Optional strings for the table title and the heading
#'   spanning all result columns. Pipe markdown combines them in its caption.
#' @param row_groups Optional named character vector or named list mapping
#'   statistics to group labels, with the same nesting syntax as `labels` for
#'   individual rows. A statistic mapping applies to all its expanded rows;
#'   specific row mappings take precedence. Grouped expanded rows show just their
#'   row names, omitting the statistic prefix unless `labels` explicitly sets it.
#'   gt shows native row sections; markdown adds a Group column. With scenarios
#'   in rows, these groups become column spanners in gt and column-label prefixes
#'   in markdown and data frames.
#' @param layout Table layout: `"statistics"` (default) puts statistics in rows;
#'   `"scenarios"` puts scenarios in rows, with short labels and automatic column
#'   groups for built-in count summaries and MTD classification percentages.
#'   A gt footnote defines "Stopped" as no final MTD recommendation when that
#'   statistic is selected. `stats` has the same default in both layouts;
#'   custom statistics keep their own labels. Explicit `labels` override the
#'   short labels; `row_groups` replaces automatic groups (`NULL` disables them).
#'   Layout is stored for subsequent [report_table()] calls; numeric values
#'   are unchanged.
#' @param sd,quantiles Passed to [oc_stat_registry()] for per-dose SDs and
#'   quantile variants. Custom `OCStat` objects retain their own settings.
#' @param ... Arguments passed to [report_table()] when rendering.
#'
#' @return A `tbl_OCOverview` tibble with columns `scenario`, `stat`, `label`,
#'   `value`, and `formatted`, or a `knitr_kable` / `gt_tbl`. `value` is a numeric
#'   list-column retaining all cell components, such as mean and SD. Scenario
#'   names and statistic IDs remain unchanged by display-label overrides.
#'
#'   Named multi-row results align by row name, in order of first appearance
#'   within each statistic. Their union is included across scenarios; unavailable
#'   rows receive `NA` and render as an em dash. An entirely missing scalar
#'   result can stand in for an unavailable multi-row statistic. Other mixtures
#'   of scalar and multi-row results are errors. Missing numeric components in
#'   an existing cell retain the statistic's own formatting.
#'
#' @examples
#' sims <- GeneralSimulations(
#'   data = list(Data(x = c(1, 1), y = c(0, 1), doseGrid = 1:3),
#'               Data(x = c(1, 2), y = c(0, 0), doseGrid = 1:3)),
#'   doses = c(2, 3), seed = 1L, truth = function(dose) dose / 10
#' )
#' scenarios <- SimulationsSummaryList(Low = sims, High = sims)
#' overview <- report_oc_overview(
#'   scenarios, stats = c("n_pat_mean_sd", "prop_mtd_correct"),
#'   labels = c(n_pat_mean_sd = "Patients, mean (SD)"),
#'   scenario_labels = c(Low = "Low toxicity"),
#'   title = "Simulation results", spanner = "Target: 20-35%"
#' )
#' report_table(overview, format = "kable")
#' report_oc_overview(scenarios, layout = "scenarios", format = "kable")
#' custom <- OCStat("Trials simulated", function(sims, summary, scenario) {
#'   summary@nsim
#' }, "%.0f")
#' report_oc_overview(scenarios, stats = list("n_pat_mean", trials = custom))
#' @export
report_oc_overview <- function(
  x,
  stats = c(
    "n_pat_mean",
    "n_dlt_mean",
    "prop_stop_tox",
    "prop_mtd_correct",
    "prop_stop_max_patients",
    "prop_mtd_over"
  ),
  format = c("tibble", "gt", "kable"),
  labels = NULL,
  scenario_labels = NULL,
  title = NULL,
  spanner = NULL,
  sd = FALSE,
  quantiles = c(0.1, 0.9),
  row_groups = NULL,
  layout = c("statistics", "scenarios"),
  ...
) {
  assert_class(x, "SimulationsSummaryList")
  format <- match.arg(format)
  layout <- match.arg(layout)
  default_groups <- missing(row_groups)
  labels <- h_oc_expand_overrides(labels)
  row_groups <- h_oc_expand_overrides(row_groups)
  h_oc_display_labels(scenario_labels, names(x@simulations))
  if (!is.null(title)) {
    assert_string(title)
  }
  if (!is.null(spanner)) {
    assert_string(spanner)
  }
  selected <- h_resolve_oc_stats(stats, oc_stat_registry(x, sd, quantiles))
  sources <- stats::setNames(
    vapply(
      stats,
      function(stat) {
        if (is.character(stat)) stat else NA_character_
      },
      character(1)
    ),
    names(selected)
  )
  if (layout == "scenarios") {
    defaults <- h_oc_scenario_layout(quantiles)
    short_labels <- defaults$labels[sources]
    names(short_labels) <- names(selected)
    short_labels <- short_labels[!is.na(short_labels)]
    short_labels[names(labels)] <- labels
    labels <- short_labels
    if (default_groups) {
      row_groups <- defaults$groups[sources]
      names(row_groups) <- names(selected)
      row_groups <- row_groups[!is.na(row_groups)]
    }
  }
  scenarios <- names(x@simulations)
  results <- lapply(names(selected), function(id) {
    stat <- selected[[id]]
    if (id %in% names(labels)) {
      stat$label <- labels[[id]]
    }
    values <- lapply(scenarios, function(scenario) {
      h_eval_oc_scenario(
        stat,
        x@simulations[[scenario]],
        x@summaries[[scenario]],
        scenario,
        id
      )
    })
    multiple <- vapply(
      values,
      function(value) any(nzchar(value$row)),
      logical(1)
    )
    if (any(multiple)) {
      for (i in which(!multiple)) {
        if (!all(is.na(unlist(values[[i]]$value)))) {
          stop(sprintf(
            "Statistic '%s' mixes scalar and multi-row results.",
            id
          ))
        }
        values[[i]] <- values[[i]][FALSE, ]
      }
    }
    keys <- unique(unlist(
      lapply(values, function(value) value$row),
      use.names = FALSE
    ))
    row_ids <- ifelse(nzchar(keys), paste0(id, "::", keys), id)
    row_labels <- ifelse(nzchar(keys), paste(stat$label, keys), stat$label)
    groups <- h_oc_row_groups(row_ids, row_groups, validate = FALSE)
    if (!is.null(groups) && !id %in% names(labels)) {
      grouped <- nzchar(keys) & !is.na(groups)
      row_labels[grouped] <- keys[grouped]
    }
    rows <- dplyr::bind_rows(lapply(seq_along(values), function(i) {
      value <- values[[i]]
      index <- match(keys, value$row)
      cells <- lapply(index, function(j) {
        if (is.na(j)) NA_real_ else value$value[[j]]
      })
      text <- value$formatted[index]
      tibble::tibble(
        scenario = scenarios[i],
        stat = row_ids,
        label = row_labels,
        value = cells,
        formatted = text
      )
    }))
    rows
  })
  result <- dplyr::bind_rows(results)
  if (anyDuplicated(result[c("scenario", "stat")])) {
    stop(
      "Expanded statistic IDs collide; give the selected statistics distinct names."
    )
  }
  h_oc_display_labels(labels, c(names(selected), unique(result$stat)))
  for (id in intersect(names(labels), result$stat)) {
    result$label[result$stat == id] <- labels[[id]]
  }
  class(result) <- c("tbl_OCOverview", class(result))
  h_oc_row_groups(unique(result$stat), row_groups)
  attr(result, "row_groups") <- row_groups
  attr(result, "scenarios") <- scenarios
  attr(result, "stats") <- unique(result$stat)
  attr(result, "scenario_labels") <- h_oc_headers(
    scenarios,
    overrides = scenario_labels
  )
  attr(result, "title") <- title
  attr(result, "spanner") <- spanner
  attr(result, "layout") <- layout
  attr(result, "stat_sources") <- sources
  if (format == "tibble") {
    return(result)
  }
  report_table(result, format = format, ...)
}

#' Define Scenario-Row Overview Formatting Defaults
#' @param quantiles Quantile probabilities used in count-summary headers.
#' @return A list of statistic labels, column groups and a stopping footnote.
#' @keywords internal
#' @noRd
h_oc_scenario_layout <- function(quantiles = c(0.1, 0.9)) {
  labels <- c(
    prop_mtd_under = "Underdose",
    prop_mtd_correct = "Target dose",
    prop_mtd_over = "Overdose",
    prop_mtd_none = "Stopped"
  )
  groups <- stats::setNames(
    rep("% of trials declaring an MTD with true DLT rate in", length(labels)),
    names(labels)
  )
  quantities <- c(
    pat = "# Patients",
    dlt = "# DLTs",
    above_target = "# Patients above target",
    cohorts = "# Dose escalation cohorts (duration proxy)"
  )
  variants <- c(
    mean = "Mean",
    mean_sd = "Mean (SD)",
    minmax = "Min–Max",
    quantiles = sprintf(
      "%g–%g%% quantiles",
      quantiles[1] * 100,
      quantiles[2] * 100
    )
  )
  for (quantity in names(quantities)) {
    ids <- paste("n", quantity, names(variants), sep = "_")
    labels[ids] <- variants
    groups[ids] <- quantities[[quantity]]
  }
  list(
    labels = labels,
    groups = groups,
    footnote = paste(
      "Trials with no final MTD recommendation, including trials stopped",
      "for safety or reaching the maximum sample size without selecting an MTD."
    )
  )
}

#' Normalize Statistic and Nested Row Overrides
#' @param overrides A named character vector or named list of overrides.
#' @return A validated named character vector keyed by statistic or expanded ID.
#' @keywords internal
#' @noRd
h_oc_expand_overrides <- function(overrides) {
  if (!length(overrides)) {
    return(NULL)
  }
  if (is.list(overrides)) {
    h_oc_names(overrides)
    overrides <- unlist(
      lapply(names(overrides), function(id) {
        value <- overrides[[id]]
        if (is.null(names(value))) {
          assert_string(value)
          stats::setNames(value, id)
        } else {
          h_oc_display_labels(value)
          stats::setNames(value, paste0(id, "::", names(value)))
        }
      }),
      use.names = TRUE
    )
  }
  h_oc_display_labels(overrides)
  overrides
}

#' Validate Named Display Overrides
#' @param labels Optional named character vector.
#' @param keys Optional allowed names.
#' @return Nothing; errors for invalid overrides.
#' @keywords internal
#' @noRd
h_oc_display_labels <- function(labels, keys = NULL) {
  if (is.null(labels)) {
    return(invisible(NULL))
  }
  assert_character(labels, any.missing = FALSE)
  assert_names(names(labels), type = "unique")
  if (
    is.null(names(labels)) ||
      anyNA(names(labels)) ||
      any(!nzchar(trimws(names(labels)))) ||
      any(!nzchar(trimws(labels)))
  ) {
    stop("Display overrides must have unique, nonempty names and labels.")
  }
  if (!is.null(keys) && !all(names(labels) %in% keys)) {
    unknown <- setdiff(names(labels), keys)
    descriptions <- vapply(
      unknown,
      function(key) {
        if (grepl("::", key, fixed = TRUE)) {
          sprintf(
            "row '%s' of statistic '%s'",
            sub("^[^:]*::", "", key),
            sub("::.*$", "", key)
          )
        } else {
          key
        }
      },
      character(1)
    )
    stop(paste(
      "Unknown display override:",
      paste(descriptions, collapse = ", ")
    ))
  }
  invisible(NULL)
}

#' Resolve Built-in Names and Custom Statistics
#' @param stats Character vector or mixed list of statistics.
#' @param registry Built-in statistics.
#' @return Named list of `OCStat` objects with unique IDs.
#' @keywords internal
#' @noRd
h_resolve_oc_stats <- function(stats, registry) {
  if (
    (!is.character(stats) && !is.list(stats)) ||
      inherits(stats, "OCStat") ||
      !length(stats)
  ) {
    stop(
      "stats must be a nonempty character vector or list of names and OCStat objects."
    )
  }
  ids <- names(stats)
  if (is.null(ids)) {
    ids <- rep("", length(stats))
  }
  if (anyNA(ids)) {
    stop("Statistic IDs must not be missing.")
  }
  selected <- vector("list", length(stats))
  for (i in seq_along(stats)) {
    stat <- stats[[i]]
    if (is.character(stat) && length(stat) == 1L && !is.na(stat)) {
      if (!stat %in% names(registry)) {
        stop(paste("Unknown statistic:", stat))
      }
      if (!nzchar(ids[i])) {
        ids[i] <- stat
      }
      selected[[i]] <- registry[[stat]]
    } else {
      if (!inherits(stat, "OCStat")) {
        stop("Each statistic must be a registry name or OCStat object.")
      }
      if (!nzchar(ids[i])) {
        ids[i] <- paste0("custom", i)
      }
      selected[[i]] <- stat
    }
  }
  if (anyDuplicated(ids) || any(!nzchar(trimws(ids)))) {
    stop(
      "Statistic IDs must be unique and nonempty; name repeated statistics explicitly."
    )
  }
  stats::setNames(selected, ids)
}

#' Render an Operating Characteristics Overview
#'
#' Uses stored formatted cells without recalculating statistics. Reordering the
#' long tibble preserves the original configured row and scenario order. Every
#' retained statistic/scenario pair must occur exactly once. Missing formatted
#' cells render as an em dash. Literal scenario labels can include spaces or
#' match column names such as `label` without affecting cell alignment.
#'
#' @param x A `tbl_OCOverview` tibble from [report_oc_overview()].
#' @param format Renderer: `"kable"`, `"gt"`, or formatted `"data.frame"`.
#' @param ... Arguments passed to `knitr::kable()` or `gt::gt()`.
#' @param title,spanner Optional title and result-column heading; defaults to the
#'   metadata stored on `x`. Use `NULL` to remove a stored heading.
#' @param scenario_labels Optional named scenario header overrides, applied
#'   over any stored overrides.
#' @param row_groups Group overrides as in [report_oc_overview()], defaulting to
#'   the stored row groups. Use `NULL` to remove grouping.
#' @param layout Layout as in [report_oc_overview()], defaulting to the stored
#'   layout. Changing it here changes rendering only; it does not select new
#'   statistics or apply the automatic short labels and groups.
#' @return A `knitr_kable`, `gt_tbl`, or formatted data frame. Kable uses a pipe table and combines
#'   the title and spanner into its caption because pipe tables cannot span cells.
#' @export
report_table.tbl_OCOverview <- function(
  x,
  format = c("kable", "gt", "data.frame"),
  ...,
  title = attr(x, "title"),
  spanner = attr(x, "spanner"),
  scenario_labels = NULL,
  row_groups = attr(x, "row_groups"),
  layout = attr(x, "layout")
) {
  format <- match.arg(format)
  if (is.null(layout)) {
    layout <- "statistics"
  }
  layout <- match.arg(layout, c("statistics", "scenarios"))
  if (!is.null(title)) {
    assert_string(title)
  }
  if (!is.null(spanner)) {
    assert_string(spanner)
  }
  scenarios <- h_oc_present_order(attr(x, "scenarios"), x$scenario)
  ids <- h_oc_present_order(attr(x, "stats"), x$stat)
  if (
    !length(scenarios) ||
      !length(ids) ||
      anyNA(x$scenario) ||
      anyNA(x$stat) ||
      anyDuplicated(x[c("scenario", "stat")]) ||
      nrow(x) != length(ids) * length(scenarios)
  ) {
    stop("The overview requires exactly one row per scenario and statistic.")
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
  headers <- h_oc_headers(
    scenarios,
    attr(x, "scenario_labels"),
    scenario_labels
  )
  groups <- h_oc_row_groups(unique(c(attr(x, "stats"), ids)), row_groups)
  groups <- groups[ids]
  scenario_rows <- layout != "statistics"
  row_keys <- if (scenario_rows) scenarios else ids
  column_keys <- if (scenario_rows) ids else scenarios
  wide <- data.frame(
    .label = unname(if (scenario_rows) headers else labels),
    check.names = FALSE
  )
  columns <- paste0(
    if (scenario_rows) ".stat_" else ".scenario_",
    seq_along(column_keys)
  )
  for (i in seq_along(column_keys)) {
    rows <- if (scenario_rows) {
      x[x$stat == column_keys[i], ]
    } else {
      x[x$scenario == column_keys[i], ]
    }
    values <- rows$formatted[match(
      row_keys,
      if (scenario_rows) rows$scenario else rows$stat
    )]
    wide[[columns[i]]] <- ifelse(is.na(values), "\u2014", values)
  }
  stub <- if (scenario_rows) "Scenario" else "Statistic"
  display <- c(stub, unname(if (scenario_rows) labels else headers))
  if (scenario_rows && length(groups) && format != "gt") {
    grouped <- !is.na(groups)
    display[which(grouped) + 1L] <- paste(groups[grouped], labels[grouped])
  }
  if (!scenario_rows && length(groups)) {
    wide <- cbind(.group = unname(groups[ids]), wide)
    display <- c("Group", display)
  }
  if (format == "data.frame") {
    return(h_report_display(
      wide,
      display,
      identifiers = seq_len(ncol(wide) - length(columns)),
      title = paste(c(title, spanner), collapse = " — "),
      column_label = if (scenario_rows) "Statistic" else "Scenario"
    ))
  }
  if (format == "kable") {
    caption <- c(title, spanner)
    args <- list(...)
    if (length(caption) && is.null(args$caption)) {
      args$caption <- paste(caption, collapse = " \u2014 ")
    }
    defaults <- list(
      x = wide,
      format = "pipe",
      row.names = FALSE,
      col.names = display,
      align = c(
        rep("l", ncol(wide) - length(columns)),
        rep("r", length(columns))
      )
    )
    defaults[names(args)] <- args
    return(do.call(knitr::kable, defaults))
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    stop(
      "Package 'gt' is required for format = 'gt'. Please install it.",
      call. = FALSE
    )
  }
  table <- if (scenario_rows) {
    gt::gt(wide, rowname_col = ".label", ...) |>
      gt::tab_stubhead(label = stub)
  } else if (length(groups)) {
    gt::gt(wide, rowname_col = ".label", groupname_col = ".group", ...) |>
      gt::row_group_order(groups = unique(unname(groups[!is.na(groups)])))
  } else {
    gt::gt(wide, ...)
  }
  table <- gt::cols_label(
    table,
    .list = stats::setNames(as.list(display), names(wide))
  )
  table <- gt::cols_align(table, align = "right", columns = columns)
  if (!is.null(title)) {
    table <- gt::tab_header(table, title = title)
  }
  if (scenario_rows && length(groups)) {
    runs <- rle(unname(groups))
    ends <- cumsum(runs$lengths)
    for (i in which(!is.na(runs$values))) {
      table <- gt::tab_spanner(
        table,
        label = runs$values[i],
        id = paste0(".stat_group_", i),
        columns = columns[seq.int(ends[i] - runs$lengths[i] + 1L, ends[i])],
        gather = FALSE
      )
    }
  }
  if (!is.null(spanner)) {
    table <- gt::tab_spanner(table, label = spanner, columns = columns)
  }
  if (layout == "scenarios") {
    sources <- attr(x, "stat_sources")
    stopped <- which(sources[sub("::.*$", "", ids)] == "prop_mtd_none")
    if (length(stopped)) {
      table <- gt::tab_footnote(
        table,
        footnote = h_oc_scenario_layout()$footnote,
        locations = gt::cells_column_labels(columns = columns[stopped])
      ) |>
        gt::opt_footnote_marks(marks = "standard")
    }
  }
  table
}

#' Render an Operating Characteristics Overview in Knitr
#' @param x A `tbl_OCOverview` tibble.
#' @param ... Arguments passed to [report_table()].
#' @param options Knitr chunk options (unused).
#' @return Markdown wrapped in `knitr::asis_output()`.
#' @export
knit_print.tbl_OCOverview <- function(x, ..., options = NULL) {
  h_knit_report_table(x, ...)
}


#' Expand Overview Row Group Overrides
#' @param ids Ordered expanded statistic IDs.
#' @param groups Optional named group labels keyed by statistic or expanded ID.
#' @param validate Reject group names absent from `ids`. Set to `FALSE` when
#'   resolving a subset of the selected statistics.
#' @return Named group labels, with missing labels for ungrouped rows.
#' @keywords internal
#' @noRd
h_oc_row_groups <- function(ids, groups, validate = TRUE) {
  groups <- h_oc_expand_overrides(groups)
  if (is.null(groups) || !length(groups)) {
    return(NULL)
  }
  base <- sub("::.*$", "", ids)
  if (validate) {
    h_oc_display_labels(groups, unique(c(base, ids)))
  }
  out <- groups[base]
  specific <- ids %in% names(groups)
  out[specific] <- groups[ids[specific]]
  stats::setNames(unname(out), ids)
}
