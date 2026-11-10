test_that("overview defaults preserve numeric values and scenario order", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(
    `Z low` = sims,
    `A high` = sims,
    target = c(.1, .2)
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
    labels = c(
      n_pat_mean_sd = "Patients, mean (SD)",
      trials = "Trials",
      `n_pat_per_dose::2` = "Dose two"
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
    paste0("dose_selection::", c("1", "2", "4", "none", "8"))
  )
  expect_equal(
    unlist(out$value),
    c(100 / 3, 100 / 3, 0, 100 / 3, NA, 100 / 3, 100 / 3, NA, 100 / 3, 0)
  )
  x <- SimulationsSummaryList(A = sims, B = general)
  out <- report_oc_overview(x, stats = "prop_stop_by_label")
  expect_equal(
    unique(out$stat),
    paste0("prop_stop_by_label::", c("Toxicity", "Max patients", "Other"))
  )
  expect_true(all(is.na(unlist(out$value[out$scenario == "B"]))))
})
