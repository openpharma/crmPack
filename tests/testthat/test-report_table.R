test_that("overview pages retain configured order, cells and headings", {
  collection <- SimulationsSummaryList(
    Alpha = oc_test_sims(),
    Beta = oc_test_sims(),
    Gamma = oc_test_sims(),
    Delta = oc_test_sims(),
    Epsilon = oc_test_sims()
  )
  x <- report_oc_overview(
    collection,
    stats = c("n_pat_mean_sd", "prop_mtd_correct"),
    title = "Results",
    spanner = "Target 20-35%",
    scenario_labels = c(Gamma = "Third scenario")
  )
  # Only stored data can be used; no simulation collection is passed to pagination.
  x$formatted <- paste(x$scenario, x$stat)
  pages <- report_paginate(x[nrow(x):1, ], max_cols = 3)
  expect_s3_class(pages, "report_tables")
  expect_length(pages, 3)
  expect_equal(
    lapply(pages, attr, "scenarios"),
    list(
      c("Alpha", "Beta"),
      c("Gamma", "Delta"),
      "Epsilon"
    )
  )
  for (page in pages) {
    expect_equal(
      page$value,
      x$value[match(
        paste(page$scenario, page$stat),
        paste(x$scenario, x$stat)
      )]
    )
    text <- paste(report_table(page), collapse = "\n")
    expect_match(text, "Results.*Target 20-35%")
    expect_match(text, page$formatted[1], fixed = TRUE)
  }
  expect_match(
    paste(report_table(pages[[2]]), collapse = "\n"),
    "Third scenario"
  )
  expect_equal(report_paginate(x, max_cols = 6)[[1]], x)
  repeated <- report_paginate(x, max_cols = 3, keep = c("label", "Beta"))
  expect_length(repeated, 4)
  expect_true(all(vapply(
    repeated,
    function(p) "Beta" %in% p$scenario,
    logical(1)
  )))
  expect_equal(attr(repeated[[1]], "scenarios"), c("Alpha", "Beta"))
})

test_that("detail pagination retains row groups and special column meaning", {
  x <- report_oc_detail(
    SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims()),
    sd = TRUE,
    title = "Dose results",
    column_labels = c(SUM = "Total"),
    scenario_labels = c(A = "First")
  )
  pages <- report_paginate(x[nrow(x):1, ], max_cols = 4)
  expect_equal(
    lapply(pages, attr, "columns"),
    list(c("1", "2"), c("4", "SUM"), c("STOP", "MAX"))
  )
  for (page in pages) {
    expect_equal(unique(page$scenario), c("B", "A"))
    expect_equal(attr(page, "stats"), attr(x, "stats"))
    text <- paste(report_table(page), collapse = "\n")
    expect_match(text, "Scenario.*Statistic")
    expect_match(text, "First")
  }
  expect_match(paste(report_table(pages[[2]]), collapse = "\n"), "Total")
  expect_equal(
    pages[[3]]$value,
    x$value[rev(which(x$column %in% c("STOP", "MAX")))]
  )
  repeated <- report_paginate(x, max_cols = 4, keep = "SUM")
  expect_length(repeated, 5)
  expect_true(all(vapply(
    repeated,
    function(p) "SUM" %in% p$column,
    logical(1)
  )))
})

test_that("toxicity and probability pages retain raw probabilities and identifiers", {
  truths <- ToxicityScenarios(
    list(Low = function(dose) dose / 30, High = function(dose) dose / 15),
    dose_grid = 1:7
  )
  pages <- report_paginate(truths, max_cols = 4)
  expect_length(pages, 3)
  expect_equal(lapply(pages, function(p) unique(p$dose)), list(1:3, 4:6, 7L))
  expect_equal(pages[[3]]$prob, c(7 / 30, 7 / 15))
  expect_match(paste(report_table(pages[[3]]), collapse = "\n"), "Dose 7")
  expect_match(paste(report_table(pages[[3]]), collapse = "\n"), "0.233")
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 2), alpha1 = c(1, 1, 1)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  x <- report_prob_summary(
    samples,
    model,
    Data(doseGrid = 1:2),
    intervals = c(0, .5, 1)
  )
  pages <- report_paginate(x, max_cols = 3, keep = "mean")
  expect_true(all(vapply(
    pages,
    function(p) all(c("dose", "mean") %in% names(p)),
    logical(1)
  )))
  expect_equal(pages[[1]]$mean, x$mean)
  expect_equal(unique(unlist(lapply(pages, names))), names(x))
  expect_true(all(lengths(pages) <= 3))
  only_dose <- x["dose"]
  expect_equal(report_paginate(only_dose, max_cols = 1)[[1]], only_dose)
  expect_equal(report_paginate(x, max_cols = ncol(x), keep = names(x))[[1]], x)
})

test_that("table lists knit consecutively and preserve rendered metadata", {
  x <- report_oc_detail(SimulationsSummaryList(A = oc_test_sims()))
  pages <- report_paginate(x, max_cols = 4)
  rendered <- report_table(pages)
  expect_true(all(vapply(rendered, inherits, logical(1), "knitr_kable")))
  text <- knitr::knit_print(rendered)
  expect_s3_class(text, "knit_asis")
  expect_match(text, "STOP")
  expect_match(text, "SUM")
  expect_equal(length(gregexpr("Statistic", text, fixed = TRUE)[[1]]), 3)
  expect_equal(as.character(knitr::knit_print(pages)), as.character(text))
  knitted <- knitr::knit(
    text = c("```{r, echo=FALSE}", "rendered", "```"),
    quiet = TRUE
  )
  expect_equal(length(gregexpr("Statistic", knitted, fixed = TRUE)[[1]]), 3)
  expect_false(grepl("\\[\\[1\\]\\]", knitted))
  expect_s3_class(report_table(list(x, x)), "report_tables")
  # Preserve html dependencies (and Word/LaTeX metadata) returned by renderers.
  local_mocked_bindings(
    knit_print = function(...) {
      knitr::asis_output("table", meta = list(list(name = "dependency")))
    },
    .package = "knitr"
  )
  out <- knit_print.report_tables(rendered)
  expect_equal(
    attr(out, "knit_meta"),
    rep(list(list(name = "dependency")), length(rendered))
  )
})

test_that("pagination rejects invalid limits, keys and inputs", {
  x <- report_oc_detail(SimulationsSummaryList(A = oc_test_sims()))
  expect_error(report_paginate(x, max_cols = 2), "leave room")
  expect_error(report_paginate(x, max_cols = 3, keep = "SUM"), "leave room")
  expect_error(report_paginate(x, max_cols = 0))
  expect_error(report_paginate(x, max_cols = 3.5))
  expect_error(report_paginate(x, keep = NA_character_))
  expect_error(report_paginate(x, keep = c("SUM", "SUM")))
  expect_error(report_paginate(x, keep = "unknown"), "Unknown keep column")
  expect_error(
    report_paginate(tibble::tibble(label = "x")),
    "classed reporting"
  )
  expect_error(report_table(list()), "nonempty list")
})

test_that("gt pages render selected columns, labels and row groups", {
  skip_if_not_installed("gt")
  x <- report_oc_detail(
    SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims()),
    title = "Dose results",
    column_labels = c(SUM = "Total")
  )
  tables <- report_table(report_paginate(x, max_cols = 4), format = "gt")
  expect_true(all(vapply(tables, inherits, logical(1), "gt_tbl")))
  expect_equal(
    tables[[2]][["_data"]]$.column_2,
    c("1.0", "1.7", "\u2014", "\u2014", "1.0", "1.7", "\u2014", "\u2014")
  )
  expect_equal(tables[[2]][["_row_groups"]], c("A", "B"))
  expect_equal(tables[[2]][["_heading"]]$title, "Dose results")
  withr::local_options(sass.cache = FALSE)
  expect_match(gt::as_raw_html(tables[[2]]), "Total")
  overview <- report_oc_overview(
    SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims()),
    spanner = "Scenarios"
  )
  tables <- report_table(report_paginate(overview, max_cols = 2), "gt")
  expect_equal(tables[[2]][["_spanners"]]$vars[[1]], ".scenario_1")
})
