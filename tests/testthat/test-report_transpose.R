test_that("transposition retains exact sources and reverses displayed cell coordinates", {
  sims <- oc_test_sims()
  oc <- SimulationsSummaryList(A = sims, B = sims)
  overview <- report_oc_overview(
    oc,
    stats = c("n_pat_mean_sd", "dose_selection")
  )
  truths <- ToxicityScenarios(
    scenarios = list(Low = function(dose) dose / 10),
    dose_grid = 1:3
  )
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 2), alpha1 = c(1, 1, 1)))
  prior <- report_prob_summary(
    samples,
    LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1),
    Data(doseGrid = c(1, 2))
  )
  for (x in list(overview, report_oc_detail(oc), truths, prior)) {
    original <- report_table(x, "data.frame")
    transposed <- report_transpose(x)
    out <- report_table(transposed, "data.frame")
    identifiers <- attr(original, "identifiers")
    expect_equal(
      dim(out),
      c(ncol(original) - length(identifiers), nrow(original) + 1L)
    )
    expect_equal(
      unname(as.matrix(out[-1L])),
      t(unname(as.matrix(original[-identifiers])))
    )
    expect_identical(report_transpose(transposed), x)
    expect_s3_class(report_table(transposed), "knitr_kable")
    expect_s3_class(knitr::knit_print(transposed), "knit_asis")
    if (requireNamespace("gt", quietly = TRUE)) {
      expect_s3_class(report_table(transposed, "gt"), "gt_tbl")
    }
  }
  expect_equal(
    names(report_table(report_transpose(overview), "data.frame"))[1],
    "Scenario"
  )
  expect_equal(
    names(report_table(report_transpose(prior), "data.frame"))[1],
    "Statistic"
  )
  pages <- report_paginate(overview, max_cols = 2)
  flipped <- report_transpose(pages)
  expect_s3_class(flipped, "report_tables")
  expect_equal(length(flipped), 2)
  expect_identical(report_transpose(flipped)[[1]], pages[[1]])
})

test_that("transposition preserves duplicate labels and grouped identifiers", {
  oc <- SimulationsSummaryList(A = oc_test_sims(), B = oc_test_sims())
  x <- report_oc_overview(
    oc,
    stats = c(first = "n_pat_mean", second = "n_dlt_mean"),
    labels = c(first = "Same", second = "Same"),
    scenario_labels = c(A = "Same scenario", B = "Same scenario"),
    row_groups = c(first = "Counts", second = "Counts")
  )
  flipped <- report_transpose(x)
  display <- report_table(flipped, "data.frame")
  expect_equal(display[[2]], c("1.7", "1.7"))
  expect_equal(display[[3]], c("1.0", "1.0"))
  expect_equal(attr(display, "display_labels")[-1], rep("Counts / Same", 2))
  expect_identical(report_transpose(flipped), x)
  expect_error(report_transpose(list()), "nonempty")
})
