test_that("detail statistics agree with per-dose accessors and terminal stopping categories", {
  x <- SimulationsSummaryList(
    `Z low` = oc_test_sims(),
    `A high` = oc_test_sims()
  )
  out <- report_oc_detail(x)
  expect_s3_class(out, "tbl_OCDetail")
  expect_named(
    out,
    c("scenario", "stat", "label", "dose", "column", "value", "formatted")
  )
  expect_equal(unique(out$scenario), c("Z low", "A high"))
  expect_equal(
    unique(out$stat),
    c("n_dlt_per_dose", "n_pat_per_dose", "prop_selected", "true_prob")
  )
  first <- out[out$scenario == "Z low", ]
  expect_equal(unique(first$column), c("1", "2", "4", "SUM", "STOP", "MAX"))
  expect_equal(
    unlist(first$value[first$stat == "n_dlt_per_dose"]),
    c(1 / 3, 2 / 3, 0, 1, NA, NA)
  )
  expect_equal(
    unlist(first$value[first$stat == "n_pat_per_dose"]),
    c(2 / 3, 1, 0, 5 / 3, NA, NA)
  )
  expect_equal(
    unlist(first$value[first$stat == "prop_selected"]),
    c(100 / 3, 100 / 3, 0, NA, 200 / 3, 200 / 3)
  )
  expect_equal(
    unlist(first$value[first$stat == "true_prob"]),
    c(.1, .2, .4, NA, NA, NA)
  )
  expect_true(all(is.na(first$dose[first$column %in% c("SUM", "STOP", "MAX")])))
  # Overlapping stop rules count by union, while recommendations stay intact.
  expect_equal(
    first$formatted[first$stat == "prop_selected" & first$column == "STOP"],
    "66.7"
  )
  expect_s3_class(knitr::knit_print(out), "knit_asis")
})

test_that("SUM mean and SD use per-trial totals for both patient-count rows", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims)
  out <- report_oc_detail(x, sd = TRUE)
  totals <- out[out$column == "SUM", ]
  expect_equal(totals$value[[1]], c(1, sd(c(2, 1, 0))))
  expect_equal(totals$value[[2]], c(5 / 3, sd(c(3, 2, 0))))
  expect_equal(totals$formatted[1:2], c("1.0 (1.0)", "1.7 (1.5)"))
  expect_false(isTRUE(all.equal(
    totals$value[[2]][2],
    sum(report_patients_per_dose(sims)$sd)
  )))
  expect_equal(out$value[[1]], c(1 / 3, sd(c(1, 0, 0))))
  expect_true(all(is.na(unlist(totals$value[3:4]))))
  out <- report_oc_detail(x, sum_column = FALSE, stop_columns = FALSE)
  expect_equal(unique(out$column), c("1", "2", "4"))
  expect_equal(nrow(out), 12L)
  # Highest-dose recommendations remain in their dose column, not MAX.
  sims@doses <- c(4, 4, 4)
  x <- SimulationsSummaryList(
    A = sims,
    stop_categories = list(Toxicity = "precision", `Max patients` = "unsafe2")
  )
  out <- report_oc_detail(x, stats = "prop_selected")
  expect_equal(unlist(out$value), c(0, 0, 100, NA, 100 / 3, 100 / 3))
})

test_that("different grids and missing stopping metadata remain distinct from zero", {
  sims <- oc_test_sims()
  general <- GeneralSimulations(
    data = sims@data,
    doses = sims@doses,
    seed = 1L,
    truth = sims@truth
  )
  for (i in seq_along(general@data)) {
    general@data[[i]]@doseGrid <- c(1, 2, 8)
  }
  x <- SimulationsSummaryList(A = sims, B = general)
  out <- report_oc_detail(x)
  expect_equal(unique(out$column), c("1", "2", "4", "8", "SUM", "STOP", "MAX"))
  expect_true(all(is.na(unlist(out$value[
    out$scenario == "A" & out$column == "8"
  ]))))
  expect_true(all(is.na(unlist(out$value[
    out$scenario == "B" & out$column == "4"
  ]))))
  expect_equal(
    out$value[[which(
      out$scenario == "B" & out$stat == "n_pat_per_dose" & out$column == "8"
    )]],
    0
  )
  expect_true(all(is.na(out$formatted[
    out$scenario == "B" & out$column %in% c("STOP", "MAX")
  ])))
  # No recommendation is not automatically a toxicity stop.
  sims@stop_report[,] <- FALSE
  sims@doses[] <- NA_real_
  out <- report_oc_detail(
    SimulationsSummaryList(A = sims),
    stats = "prop_selected"
  )
  expect_equal(unlist(out$value), c(0, 0, 0, NA, 0, 0))
})

test_that("custom dose statistics and aliases preserve raw cells and display controls", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  custom <- OCStat(
    "Custom",
    function(sims, summary, scenario) list(`1` = c(1, 2), `4` = c(4, 5)),
    "%g/%g"
  )
  out <- report_oc_detail(
    x,
    stats = list(counts = "n_pat_per_dose", ratio = custom),
    labels = c(counts = "Patients", ratio = "Ratio"),
    column_labels = c(`1` = "Dose 1", SUM = "Total"),
    scenario_labels = c(A = "Scenario 1")
  )
  expect_equal(out$value[[7]], c(1, 2))
  expect_equal(out$formatted[7:9], c("1/2", NA, "4/5"))
  expect_equal(out$label, rep(c("Patients", "Ratio"), each = 6))
  text <- paste(report_table(out), collapse = "\n")
  expect_match(text, "Dose 1.*Total")
  expect_match(text, "Scenario 1.*Ratio.*1/2.*\u2014.*4/5")
  x@simulations$A@fit <- list()
  expect_s3_class(report_oc_detail(x), "tbl_OCDetail")
})

test_that("detail renderers preserve grouping, pairing and stored formatting", {
  sims <- oc_test_sims()
  other <- sims
  other@doses <- c(4, 4, 4)
  x <- SimulationsSummaryList(label = sims, SUM = other)
  out <- report_oc_detail(
    x,
    title = "Per-dose results",
    labels = c(n_dlt_per_dose = "Patients with DLTs"),
    scenario_labels = c(label = "Scenario 1", SUM = "Scenario 2")
  )
  table <- report_table(out)
  expect_s3_class(table, "knitr_kable")
  expect_equal(report_table(out[rev(seq_len(nrow(out))), ]), table)
  expect_equal(
    report_oc_detail(
      x,
      title = "Per-dose results",
      labels = c(n_dlt_per_dose = "Patients with DLTs"),
      scenario_labels = c(label = "Scenario 1", SUM = "Scenario 2"),
      format = "kable"
    ),
    table
  )
  text <- paste(table, collapse = "\n")
  expect_match(
    text,
    "Scenario 1.*Dose selected:.*33.3.*33.3.*0.0.*\u2014.*66.7.*66.7"
  )
  expect_match(
    text,
    "Scenario 2.*Dose selected:.*0.0.*0.0.*100.0.*\u2014.*66.7.*66.7"
  )
  expect_snapshot(cat(paste(table, collapse = "\n")))
  expect_error(report_table(out[-1, ]), "exactly one row")
  expect_error(report_table(out[c(1, 1, 2:nrow(out)), ]), "exactly one row")
  bad <- out
  bad$label[2] <- "Changed"
  expect_error(report_table(bad), "consistent label")
  out$formatted[1] <- "Stored"
  expect_match(
    paste(
      report_table(out, title = NULL, column_labels = c(`1` = "Dose one")),
      collapse = "\n"
    ),
    "Stored"
  )
})

test_that("gt detail uses native scenario groups and a title", {
  skip_if_not_installed("gt")
  x <- SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims())
  table <- report_oc_detail(
    x,
    format = "gt",
    title = "Dose results",
    scenario_labels = c(A = "First", B = "Second")
  )
  expect_s3_class(table, "gt_tbl")
  expect_equal(table[["_heading"]]$title, "Dose results")
  expect_equal(
    table[["_data"]]$.column_1,
    rep(c("0.3", "0.7", "33.3", "0.100"), 2)
  )
  withr::local_options(sass.cache = FALSE)
  html <- gt::as_raw_html(table)
  expect_match(html, "First")
  expect_match(html, "Second")
  expect_match(html, "SUM")
  expect_match(html, "STOP")
  expect_match(html, "MAX")
})

test_that("detail errors identify invalid specifications and dose keys", {
  x <- SimulationsSummaryList(A = oc_test_sims())
  expect_error(report_oc_detail(list()), "SimulationsSummaryList")
  expect_error(report_oc_detail(x, stats = "n_pat_mean"), "Unknown statistic")
  expect_error(report_oc_detail(x, sum_column = NA))
  expect_error(
    report_oc_detail(x, labels = c(missing = "Label")),
    "Unknown display override"
  )
  expect_error(
    report_oc_detail(x, column_labels = c(missing = "Dose")),
    "Unknown display override"
  )
  custom <- OCStat("Wrong", function(sims, summary, scenario) list(`999` = 1))
  expect_error(
    report_oc_detail(x, stats = list(custom)),
    "scenario 'A'.*keyed by doses"
  )
  custom$fun <- function(sims, summary, scenario) 1
  expect_error(report_oc_detail(x, stats = list(custom)), "keyed by doses")
  out <- report_oc_detail(x)
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(report_table(out, "gt"), "Package 'gt' is required")
})

test_that("detail counts and faceted plots include placebo observations", {
  data <- Data(
    x = c(.001, 1, 1),
    y = c(0, 0, 1),
    doseGrid = c(.001, 1, 2),
    placebo = TRUE,
    ID = 1:3,
    cohort = rep(1L, 3)
  )
  sims <- GeneralSimulations(
    data = list(data, data),
    doses = c(1, 1),
    seed = 1L,
    truth = function(dose) dose / 10
  )
  x <- SimulationsSummaryList(A = sims)
  out <- report_oc_detail(
    x,
    stats = c("n_pat_per_dose", "n_dlt_per_dose"),
    sd = TRUE
  )
  expect_equal(out$value[[1]], c(1, 0))
  expect_equal(out$value[[4]], c(3, 0))
  expect_equal(out$value[[10]], c(1, 0))
  expect_equal(plot(x, type = "nPatients")$data$value, c(1, 2, 0))
})
