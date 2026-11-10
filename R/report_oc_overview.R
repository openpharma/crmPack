#' Report Operating Characteristics Across Scenarios
#'
#' Assemble selected statistics from a [SimulationsSummaryList()] without
#' recomputing its summaries or running MCMC. Rendered tables have statistics
#' as rows and scenarios as columns. Arm sub-columns are not yet supported.
#'
#' @param x A `SimulationsSummaryList`.
#' @param stats Nonempty character vector of [oc_stat_registry()] names, or a
#'   list mixing those names and [OCStat()] objects. Optional names assign
#'   unique statistic IDs; unnamed custom statistics get IDs `custom1`, etc.,
#'   using their position in `stats`. Order determines table row order.
#' @param format Output format; defaults to the long unrounded tibble.
#' @param labels Optional named character vector of row-label overrides. Names
#'   identify statistic IDs or expanded row IDs (`<statistic ID>::<row name>`).
#'   A statistic-level override also supplies the prefix for its expanded rows.
#' @param scenario_labels Optional named character vector of scenario header
#'   overrides. Names identify original scenarios; partial overrides are allowed.
#' @param title,spanner Optional strings for the table title and the heading
#'   spanning all scenario columns. Pipe markdown combines them in its caption.
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
  ...
) {
  assert_class(x, "SimulationsSummaryList")
  format <- match.arg(format)
  h_oc_display_labels(labels)
  h_oc_display_labels(scenario_labels, names(x@simulations))
  if (!is.null(title)) {
    assert_string(title)
  }
  if (!is.null(spanner)) {
    assert_string(spanner)
  }
  selected <- h_resolve_oc_stats(stats, oc_stat_registry(x, sd, quantiles))
  scenarios <- names(x@simulations)
  results <- lapply(names(selected), function(id) {
    stat <- selected[[id]]
    if (id %in% names(labels)) {
      stat$label <- labels[[id]]
    }
    values <- lapply(scenarios, function(scenario) {
      tryCatch(
        h_eval_oc_stat(
          stat,
          x@simulations[[scenario]],
          x@summaries[[scenario]],
          scenario
        ),
        error = function(e) {
          stop(
            sprintf(
              "Statistic '%s', scenario '%s': %s",
              id,
              scenario,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
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
  attr(result, "scenarios") <- scenarios
  attr(result, "stats") <- unique(result$stat)
  headers <- stats::setNames(scenarios, scenarios)
  headers[names(scenario_labels)] <- scenario_labels
  attr(result, "scenario_labels") <- headers
  attr(result, "title") <- title
  attr(result, "spanner") <- spanner
  if (format == "tibble") {
    return(result)
  }
  report_table(result, format = format, ...)
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
    stop(paste(
      "Unknown display override:",
      paste(setdiff(names(labels), keys), collapse = ", ")
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
#' @param format Renderer: `"kable"` or `"gt"`.
#' @param ... Arguments passed to `knitr::kable()` or `gt::gt()`.
#' @param title,spanner Optional title and scenario heading; defaults to the
#'   metadata stored on `x`. Use `NULL` to remove a stored heading.
#' @param scenario_labels Optional named scenario header overrides, applied
#'   over any stored overrides.
#' @return A `knitr_kable` or `gt_tbl` object. Kable uses a pipe table and combines
#'   the title and spanner into its caption because pipe tables cannot span cells.
#' @export
report_table.tbl_OCOverview <- function(
  x,
  format = c("kable", "gt"),
  ...,
  title = attr(x, "title"),
  spanner = attr(x, "spanner"),
  scenario_labels = NULL
) {
  format <- match.arg(format)
  if (!is.null(title)) {
    assert_string(title)
  }
  if (!is.null(spanner)) {
    assert_string(spanner)
  }
  scenarios <- attr(x, "scenarios")
  scenarios <- c(
    intersect(scenarios, x$scenario),
    setdiff(unique(x$scenario), scenarios)
  )
  ids <- attr(x, "stats")
  ids <- c(intersect(ids, x$stat), setdiff(unique(x$stat), ids))
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
  h_oc_display_labels(scenario_labels, scenarios)
  headers <- stats::setNames(scenarios, scenarios)
  stored <- attr(x, "scenario_labels")
  headers[intersect(names(stored), scenarios)] <- stored[intersect(
    names(stored),
    scenarios
  )]
  headers[names(scenario_labels)] <- scenario_labels
  wide <- data.frame(.label = unname(labels), check.names = FALSE)
  columns <- paste0(".scenario_", seq_along(scenarios))
  for (i in seq_along(scenarios)) {
    rows <- x[x$scenario == scenarios[i], ]
    values <- rows$formatted[match(ids, rows$stat)]
    wide[[columns[i]]] <- ifelse(is.na(values), "\u2014", values)
  }
  display <- c("Statistic", unname(headers))
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
      align = c("l", rep("r", length(scenarios)))
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
  table <- gt::gt(wide, ...)
  table <- gt::cols_label(
    table,
    .list = stats::setNames(as.list(display), names(wide))
  )
  table <- gt::cols_align(table, align = "right", columns = columns)
  if (!is.null(title)) {
    table <- gt::tab_header(table, title = title)
  }
  if (!is.null(spanner)) {
    table <- gt::tab_spanner(table, label = spanner, columns = columns)
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
  knitr::asis_output(paste(
    report_table(x, format = "kable", ...),
    collapse = "\n"
  ))
}
