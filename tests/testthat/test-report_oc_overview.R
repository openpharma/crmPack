test_that("overview defaults preserve numeric values and scenario order", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(
    `Z low` = sims,
    `A high` = sims,
    target = c(.1, .2),
    stop_categories = h_default_stop_categories(sims@stop_report)
  )
  out <- report_oc_overview(x)
  expect_s3_class(out, "tbl_OCOverview")
  expect_named(out, c("scenario", "stat", "label", "value", "formatted"))
  expect_equal(unique(out$scenario), c("Z low", "A high"))
  expect_equal(
    unique(out$stat),
    c(
      "n_pat_mean",
      "n_dlt_mean",
      "prop_stop_tox",
      "prop_mtd_correct",
      "prop_stop_max_patients",
      "prop_mtd_over"
    )
  )
  first <- out[out$scenario == "Z low", ]
  expect_equal(unlist(first$value), c(5 / 3, 1, 200 / 3, 200 / 3, 200 / 3, 0))
  expect_equal(first$formatted, c("1.7", "1.0", "66.7", "66.7", "66.7", "0.0"))
})

test_that("mixed statistics, aliases, labels and display options are independent", {
  sims <- oc_test_sims()
  sims@additional_stats[[3]]$score <- 3
  x <- SimulationsSummaryList(A = sims)
  custom <- OCStat(
    "Cached trials",
    function(sims, summary, scenario) {
      if (scenario != "A") {
        stop("Wrong scenario")
      }
      summary@nsim
    },
    "%.0f"
  )
  out <- report_oc_overview(
    x,
    stats = list(
      "n_pat_mean_sd",
      trials = custom,
      "additional:score",
      "n_pat_quantiles",
      "n_pat_per_dose"
    ),
    labels = list(
      n_pat_mean_sd = "Patients, mean (SD)",
      trials = "Trials",
      n_pat_per_dose = c("2" = "Dose two")
    ),
    sd = TRUE,
    quantiles = c(0, 1)
  )
  expect_equal(out$label[1:2], c("Patients, mean (SD)", "Trials"))
  expect_equal(out$value[[1]], c(5 / 3, sd(c(3, 2, 0))))
  expect_equal(out$formatted[1:4], c("1.7 (1.5)", "3", "2.0", "[0.0, 3.0]"))
  expect_equal(out$label[out$stat == "n_pat_per_dose::2"], "Dose two")
  expect_equal(out$value[[5]], c(2 / 3, sd(c(2, 0, 0))))
  expect_equal(report_oc_overview(x, stats = list(custom))$stat, "custom1")
  out <- report_oc_overview(
    x,
    stats = c(first = "n_pat_mean", second = "n_pat_mean"),
    labels = c(first = "First", second = "Second")
  )
  expect_equal(out$label, c("First", "Second"))
  expect_equal(out$value[[1]], out$value[[2]])
})

test_that("multi-row statistics align by name across scenarios and permit unavailable families", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, B = sims, C = sims)
  custom <- OCStat(
    "Metric",
    function(sims, summary, scenario) {
      switch(
        scenario,
        A = list(one = 1, two = 2),
        B = list(two = 20, three = 30),
        C = NA_real_
      )
    },
    "%.0f"
  )
  out <- report_oc_overview(
    x,
    stats = list(metric = custom),
    labels = c(metric = "Result")
  )
  expect_equal(out$stat, rep(paste0("metric::", c("one", "two", "three")), 3))
  expect_equal(out$label, rep(paste("Result", c("one", "two", "three")), 3))
  expect_equal(unlist(out$value), c(1, 2, NA, NA, 20, 30, NA, NA, NA))
  table <- report_table(out)
  expect_match(paste(table, collapse = "\n"), "Result two.*2.*20.*\u2014")
  expect_match(
    paste(table, collapse = "\n"),
    "Result three.*\u2014.*30.*\u2014"
  )
  custom$fun <- function(sims, summary, scenario) {
    if (scenario == "A") list(one = 1) else 2
  }
  expect_error(
    report_oc_overview(x, stats = list(custom)),
    "mixes scalar and multi-row"
  )
})

test_that("overview renderers retain pairing after reordering and use header metadata", {
  sims <- oc_test_sims()
  other <- sims
  other@doses <- c(4, 4, 4)
  x <- SimulationsSummaryList(
    label = sims,
    `Z scenario` = other,
    target = c(.1, .2)
  )
  out <- report_oc_overview(
    x,
    stats = c("n_pat_mean_sd", "prop_mtd_correct", "prop_mtd_over"),
    labels = c(n_pat_mean_sd = "Patients"),
    scenario_labels = c(label = "Low toxicity"),
    title = "Operating characteristics",
    spanner = "Target 10-20%"
  )
  table <- report_table(out)
  expect_s3_class(table, "knitr_kable")
  expect_equal(report_table(out[c(6, 1, 4, 2, 5, 3), ]), table)
  expect_equal(
    report_oc_overview(
      x,
      stats = c("n_pat_mean_sd", "prop_mtd_correct", "prop_mtd_over"),
      labels = c(n_pat_mean_sd = "Patients"),
      scenario_labels = c(label = "Low toxicity"),
      title = "Operating characteristics",
      spanner = "Target 10-20%",
      format = "kable"
    ),
    table
  )
  text <- paste(table, collapse = "\n")
  expect_match(text, "Operating characteristics.*Target 10-20%")
  expect_match(text, "Low toxicity.*Z scenario")
  expect_match(text, "MTD correct.*66.7.*0.0")
  expect_match(text, "MTD overestimated.*0.0.*100.0")
  expect_s3_class(knitr::knit_print(out), "knit_asis")
  expect_match(
    paste(
      report_table(
        out,
        title = NULL,
        spanner = NULL,
        scenario_labels = c(label = "New header")
      ),
      collapse = "\n"
    ),
    "New header"
  )
  expect_false(grepl(
    "Operating characteristics",
    paste(report_table(out, title = NULL, spanner = NULL), collapse = "\n")
  ))
  expect_snapshot(cat(paste(table, collapse = "\n")))
  expect_error(report_table(out[-1, ]), "exactly one row")
  expect_error(report_table(out[c(1, 1, 2:6), ]), "exactly one row")
  out$label[2] <- "Inconsistent"
  expect_error(report_table(out), "consistent label")
})

test_that("gt overview renders a title and a spanner over scenario columns", {
  skip_if_not_installed("gt")
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, B = sims)
  table <- report_oc_overview(
    x,
    stats = "n_pat_mean_sd",
    format = "gt",
    title = "Results",
    spanner = "Assumptions",
    scenario_labels = c(A = "Scenario A")
  )
  expect_s3_class(table, "gt_tbl")
  expect_equal(table[["_data"]][[2]], "1.7 (1.5)")
  expect_equal(table[["_heading"]]$title, "Results")
  expect_equal(unlist(table[["_spanners"]]$spanner_label), "Assumptions")
  expect_equal(table[["_spanners"]]$vars[[1]], c(".scenario_1", ".scenario_2"))
  withr::local_options(sass.cache = FALSE)
  html <- gt::as_raw_html(table)
  expect_match(html, "Scenario A")
  expect_match(html, "Assumptions")
  expect_match(html, "1.7 \\(1.5\\)")
})

test_that("invalid selections and overrides fail clearly", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  expect_error(report_oc_overview(list()), "SimulationsSummaryList")
  expect_error(report_oc_overview(x, stats = character()), "nonempty")
  expect_error(report_oc_overview(x, stats = "missing"), "Unknown statistic")
  expect_error(
    report_oc_overview(x, stats = list(1)),
    "registry name or OCStat"
  )
  expect_error(
    report_oc_overview(x, stats = c("n_pat_mean", "n_pat_mean")),
    "unique"
  )
  expect_error(
    report_oc_overview(x, labels = c(nopat = "Patients")),
    "Unknown display override"
  )
  expect_error(report_oc_overview(x, labels = "Patients"), "names")
  expect_error(
    report_oc_overview(x, scenario_labels = c(B = "Other")),
    "Unknown display override"
  )
  expect_error(report_oc_overview(x, title = NA))
  custom <- OCStat("Broken", function(sims, summary, scenario) {
    stop("broken calculation")
  })
  expect_error(
    report_oc_overview(x, stats = list(broken = custom)),
    "Statistic 'broken', scenario 'A': broken calculation"
  )
  custom$fun <- function(sims, summary, scenario) list(b = 1)
  expect_error(
    report_oc_overview(x, stats = list(a = custom, `a::b` = "n_pat_mean")),
    "collide"
  )
})

test_that("rendering uses stored cells and overview uses cached summaries", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  custom <- OCStat(
    "Trials",
    function(sims, summary, scenario) summary@nsim,
    "%.0f"
  )
  # Replacing the stored fit with an unusable object makes re-summarizing fail.
  x@simulations$A@fit <- list()
  out <- report_oc_overview(x, stats = list(custom))
  expect_equal(out$value[[1]], 3L)
  out$formatted <- "Stored display"
  expect_match(paste(report_table(out), collapse = "\n"), "Stored display")
})

test_that("missing gt produces an actionable dependency error", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  out <- report_oc_overview(x)
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(report_table(out, "gt"), "Package 'gt' is required")
})

test_that("different dose grids distinguish unavailable doses from zero selections", {
  sims <- oc_test_sims()
  general <- GeneralSimulations(
    data = sims@data,
    doses = sims@doses,
    seed = 1L,
    truth = sims@truth
  )
  other <- general
  for (i in seq_along(other@data)) {
    other@data[[i]]@doseGrid <- c(1, 2, 8)
  }
  x <- SimulationsSummaryList(A = general, B = other)
  out <- report_oc_overview(x, stats = "dose_selection")
  expect_equal(
    unique(out$stat),
    paste0("dose_selection::", c("none", "1", "2", "4", "8"))
  )
  expect_equal(
    unlist(out$value),
    c(100 / 3, 100 / 3, 100 / 3, 0, NA, 100 / 3, 100 / 3, 100 / 3, NA, 0)
  )
  grouped <- report_oc_overview(
    x,
    stats = "dose_selection",
    labels = list(dose_selection = c(none = "None")),
    row_groups = c(dose_selection = "Dose selection (%)")
  )
  expect_equal(unique(grouped$label), c("None", "1", "2", "4", "8"))
  expect_equal(grouped$value, out$value)
  display <- report_table(grouped, format = "data.frame")
  expect_equal(display$Statistic, c("None", "1", "2", "4", "8"))
  x <- SimulationsSummaryList(A = sims, B = general)
  out <- report_oc_overview(x, stats = "prop_stop_by_label")
  expect_equal(
    unique(out$stat),
    paste0("prop_stop_by_label::", colnames(sims@stop_report))
  )
  expect_true(all(is.na(unlist(out$value[out$scenario == "B"]))))
})

test_that("overview row sections preserve report labels, numeric cells and pagination", {
  x <- SimulationsSummaryList(
    A = oc_test_sims(),
    B = oc_test_sims()
  )
  out <- report_oc_overview(
    x,
    stats = c("prop_stop_by_label", "n_pat_mean_sd"),
    row_groups = c(
      prop_stop_by_label = "Stop reason (%)",
      n_pat_mean_sd = "Patients"
    )
  )
  expect_true(all(
    c("unsafe", "unsafe2", "size", "precision") %in% out$label
  ))
  plain <- report_oc_overview(
    x,
    stats = c("prop_stop_by_label", "n_pat_mean_sd")
  )
  expect_equal(out$value, plain$value)
  expect_equal(report_table(out[rev(seq_len(nrow(out))), ]), report_table(out))
  page <- report_paginate(out, max_cols = 3)[[1]]
  expect_match(paste(report_table(page), collapse = "\n"), "Stop reason")
  expect_equal(ncol(report_table(page, "data.frame")), 3L)
  expect_error(
    report_oc_overview(x, row_groups = c(missing = "Group")),
    "Unknown"
  )
  skip_if_not_installed("gt")
  table <- report_table(out, "gt")
  expect_equal(table[["_row_groups"]], c("Stop reason (%)", "Patients"))
  withr::local_options(sass.cache = FALSE)
  html <- gt::as_raw_html(table)
  expect_match(html, "unsafe2")
  expect_match(html, "Stop reason")
  partial <- report_oc_overview(x, row_groups = c(n_pat_mean = "Patients"))
  expect_s3_class(report_table(partial, "gt"), "gt_tbl")
})


test_that("grouped expanded rows omit prefixes and honor nested overrides", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, B = sims)
  plain <- report_oc_overview(x, stats = "prop_stop_by_label")
  grouped <- report_oc_overview(
    x,
    stats = "prop_stop_by_label",
    row_groups = c(prop_stop_by_label = "Stop reason (%)")
  )
  expect_equal(unique(grouped$label), colnames(sims@stop_report))
  expect_equal(
    unique(plain$label),
    paste("Stop (%) :", colnames(sims@stop_report))
  )
  expect_equal(grouped$value, plain$value)
  expect_equal(grouped$stat, plain$stat)
  renamed <- report_oc_overview(
    x,
    stats = "prop_stop_by_label",
    labels = list(
      prop_stop_by_label = c(unsafe = "Unsafe", size = "Maximum size")
    ),
    row_groups = c(prop_stop_by_label = "Stop reason (%)")
  )
  expect_equal(
    unique(renamed$label),
    c("Unsafe", "unsafe2", "Maximum size", "precision")
  )
  expect_equal(renamed$value, grouped$value)
  prefixed <- report_oc_overview(
    x,
    stats = "prop_stop_by_label",
    labels = c(prop_stop_by_label = "Rule:"),
    row_groups = c(prop_stop_by_label = "Stop reason (%)")
  )
  expect_equal(
    unique(prefixed$label),
    paste("Rule:", colnames(sims@stop_report))
  )
  partial <- report_oc_overview(
    x,
    stats = "prop_stop_by_label",
    row_groups = list(prop_stop_by_label = c(unsafe = "Safety"))
  )
  expect_equal(
    unique(partial$label),
    c(
      "unsafe",
      paste("Stop (%) :", c("unsafe2", "size", "precision"))
    )
  )
  table <- report_table(partial, "data.frame")
  expect_equal(table$Group, c("Safety", "—", "—", "—"))
  expect_error(
    report_oc_overview(
      x,
      stats = "prop_stop_by_label",
      labels = list(prop_stop_by_label = c(missing = "Missing"))
    ),
    "Unknown display override: row 'missing' of statistic 'prop_stop_by_label'",
    fixed = TRUE
  )
  expect_error(
    report_oc_overview(x, labels = list(n_pat_mean = c("One", "Two")))
  )
  expect_equal(
    report_oc_overview(x, labels = list(), row_groups = list()),
    report_oc_overview(x)
  )
})

test_that("nested row overrides align across scenarios and apply during rendering", {
  x <- SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims())
  stat <- OCStat("Metric:", function(sims, summary, scenario) {
    if (scenario == "A") list(one = 1) else list(two = 2)
  })
  out <- report_oc_overview(
    x,
    stats = list(metric = stat),
    labels = list(metric = c(two = "Second")),
    row_groups = c(metric = "Metrics")
  )
  expect_equal(out$label, c("one", "Second", "one", "Second"))
  expect_equal(unlist(out$value), c(1, NA, NA, 2))
  table <- report_table(
    out,
    "data.frame",
    row_groups = list(metric = c(two = "Second group"))
  )
  expect_equal(table$Group, c("—", "Second group"))
})

test_that("scenario layouts preserve cell alignment and scenario overrides", {
  sims <- oc_test_sims()
  other <- sims
  other@doses <- c(4, 4, 4)
  x <- SimulationsSummaryList(A = sims, B = other, target = c(.1, .2))
  out <- report_oc_overview(
    x,
    stats = c("prop_mtd_correct", "prop_mtd_over"),
    layout = "scenarios",
    labels = c(
      prop_mtd_correct = "MTD correct (%)",
      prop_mtd_over = "MTD overestimated (%)"
    ),
    row_groups = NULL,
    scenario_labels = c(A = "Low toxicity")
  )
  display <- report_table(out, "data.frame")
  expect_named(
    display,
    c("Scenario", "MTD correct (%)", "MTD overestimated (%)")
  )
  expect_equal(display[[1]], c("Low toxicity", "B"))
  expect_equal(display[[2]], c("66.7", "0.0"))
  expect_equal(display[[3]], c("0.0", "100.0"))
  expect_equal(report_table(out[c(4, 2, 1, 3), ], "data.frame"), display)
  expect_match(
    paste(report_table(out), collapse = "\n"),
    "Low toxicity.*66.7.*0.0"
  )
  expect_equal(
    unname(unlist(report_table(report_transpose(out), "data.frame")[-1])),
    c("66.7", "0.0", "0.0", "100.0")
  )
})

test_that("scenario-row defaults preserve calculations and supply grouped headers and footnotes", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, B = sims)
  ids <- c(
    "prop_mtd_under",
    "prop_mtd_correct",
    "prop_mtd_over",
    "prop_mtd_none",
    "n_pat_mean",
    "n_pat_minmax",
    "n_dlt_mean",
    "n_dlt_minmax"
  )
  out <- report_oc_overview(x, stats = ids, layout = "scenarios")
  plain <- report_oc_overview(x, stats = ids)
  expect_equal(out$value, plain$value)
  expect_equal(out$formatted, plain$formatted)
  expect_equal(unique(out$stat), ids)
  display <- report_table(out, "data.frame")
  expect_equal(dim(display), c(2L, 9L))
  expect_equal(display[[1]], c("A", "B"))
  expect_match(names(display)[6], "# Patients Mean", fixed = TRUE)
  expect_match(names(display)[8], "# DLTs Mean", fixed = TRUE)
  expect_s3_class(
    report_oc_overview(x, stats = ids, layout = "scenarios", format = "kable"),
    "knitr_kable"
  )
  skip_if_not_installed("gt")
  withr::local_options(sass.cache = FALSE)
  table <- report_table(out, "gt")
  expect_equal(lengths(table[["_spanners"]]$vars), c(4L, 2L, 2L))
  expect_equal(
    unlist(table[["_spanners"]]$spanner_label)[2:3],
    c("# Patients", "# DLTs")
  )
  html <- gt::as_raw_html(table)
  expect_match(html, "Stopped")
  expect_match(html, "Trials with no final MTD recommendation")
})

test_that("scenario-row overrides support aliases, custom statistics and explicit ungrouping", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  out <- report_oc_overview(
    x,
    layout = "scenarios",
    stats = list(
      none = "prop_mtd_none",
      patients = "n_pat_mean",
      trials = OCStat(
        "Trials",
        function(sims, summary, scenario) summary@nsim,
        "%.0f"
      )
    ),
    labels = c(none = "No selection"),
    row_groups = NULL
  )
  expect_equal(out$label, c("No selection", "Mean", "Trials"))
  expect_null(attr(out, "row_groups"))
  expect_equal(dim(report_table(out, "data.frame")), c(1L, 4L))
  skip_if_not_installed("gt")
  withr::local_options(sass.cache = FALSE)
  table <- report_table(out, "gt")
  expect_equal(nrow(table[["_spanners"]]), 0L)
  expect_match(
    gt::as_raw_html(table),
    "Trials with no final MTD recommendation"
  )
  without_stopped <- report_oc_overview(
    x,
    layout = "scenarios",
    stats = "n_pat_mean"
  )
  expect_equal(nrow(report_table(without_stopped, "gt")[["_footnotes"]]), 0L)
})

test_that("scenario-row pagination splits statistic columns and retains scenario rows", {
  x <- SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims())
  out <- report_oc_overview(
    x,
    layout = "scenarios",
    stats = c(
      "prop_mtd_under",
      "prop_mtd_correct",
      "prop_mtd_over",
      "prop_mtd_none",
      "n_pat_mean",
      "n_pat_minmax",
      "n_dlt_mean",
      "n_dlt_minmax"
    )
  )
  pages <- report_paginate(out, max_cols = 4)
  expect_length(pages, 3)
  expect_equal(
    lapply(pages, function(page) unique(page$scenario)),
    rep(list(c("A", "B")), 3)
  )
  expect_equal(
    unlist(lapply(pages, function(page) unique(page$stat))),
    unique(out$stat)
  )
  expect_equal(
    lapply(pages, function(page) ncol(report_table(page, "data.frame"))),
    list(4L, 4L, 3L)
  )
  skip_if_not_installed("gt")
  tables <- report_table(pages, "gt")
  expect_equal(
    vapply(tables, function(table) nrow(table[["_footnotes"]]), integer(1)),
    c(0L, 1L, 0L)
  )
})


test_that("scenario layout uses the same selected statistics for a Merck-style overview", {
  x <- SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims())
  default <- report_oc_overview(x)
  scenario_rows <- report_oc_overview(x, layout = "scenarios")
  expect_equal(scenario_rows$stat, default$stat)
  expect_equal(scenario_rows$value, default$value)
  expect_equal(scenario_rows$formatted, default$formatted)
  stats <- c(
    "n_pat_mean",
    "n_dlt_mean",
    "prop_stop_tox",
    "prop_mtd_correct",
    "max_without_mtd",
    "prop_mtd_over"
  )
  labels <- c(
    prop_stop_tox = "Stop: Toxicity",
    prop_mtd_correct = "Correctly chosen MTD",
    max_without_mtd = "Reaching max without MTD",
    prop_mtd_over = "Overestimated MTD"
  )
  groups <- c(
    n_pat_mean = "# Patients",
    n_dlt_mean = "# DLTs",
    stats::setNames(rep("% of trials with", 4), stats[3:6])
  )
  out <- report_oc_overview(
    x,
    stats = stats,
    layout = "scenarios",
    labels = labels,
    row_groups = groups
  )
  plain <- report_oc_overview(x, stats = stats)
  expect_equal(out$value, plain$value)
  expect_equal(out$formatted, plain$formatted)
  expect_equal(
    out$label[out$stat == "max_without_mtd"],
    rep("Reaching max without MTD", 2)
  )
  skip_if_not_installed("gt")
  table <- report_table(out, "gt")
  expect_equal(
    table[["_spanners"]]$vars,
    list(".stat_1", ".stat_2", paste0(".stat_", 3:6))
  )
  expect_equal(nrow(table[["_footnotes"]]), 0L)
})

test_that("scenario headers cover count variants and keep explicit groups", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  out <- report_oc_overview(
    x,
    layout = "scenarios",
    quantiles = c(.25, .75),
    stats = c("n_pat_mean_sd", "n_pat_quantiles", "n_dlt_minmax"),
    labels = c(n_pat_mean_sd = "Average (SD)")
  )
  display <- report_table(out, "data.frame")
  expect_named(
    display,
    c(
      "Scenario",
      "# Patients Average (SD)",
      "# Patients 25–75% quantiles",
      "# DLTs Min–Max"
    )
  )
  grouped <- report_oc_overview(
    x,
    layout = "scenarios",
    stats = c("n_pat_mean", "n_dlt_mean"),
    row_groups = c(n_pat_mean = "Sample size")
  )
  expect_equal(
    names(report_table(grouped, "data.frame")),
    c("Scenario", "Sample size Mean", "Mean")
  )
})
