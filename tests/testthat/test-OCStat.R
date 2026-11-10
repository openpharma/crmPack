test_that("summary collections cache named summaries with shared metadata", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(First = sims, Second = sims, target = c(0.1, 0.2))
  expect_s4_class(x, "SimulationsSummaryList")
  expect_true(validObject(x))
  expect_named(x@summaries, c("First", "Second"))
  expect_equal(x@summaries$First, summary(sims, target = c(0.1, 0.2)))
  expect_equal(x@simulations$First, sims)
  expect_equal(
    x@stop_categories$First,
    list(
      Toxicity = c("unsafe", "unsafe2"),
      `Max patients` = "size",
      Other = "precision"
    )
  )
  expect_equal(
    SimulationsSummaryList(list(First = sims))@simulations,
    list(First = sims)
  )
  expect_error(SimulationsSummaryList(sims), "names")
  expect_error(SimulationsSummaryList(A = sims, A = sims), "names")
  expect_error(SimulationsSummaryList(), "names")
  expect_error(
    SimulationsSummaryList(A = sims, target = c(.3, .2)),
    "increasing"
  )
  sims@truth <- NULL
  expect_error(SimulationsSummaryList(A = sims), "No truth")
})

test_that("default stopping categories count unions and explicit mappings override", {
  sims <- oc_test_sims()
  result <- report_stop_reasons(sims)
  expect_equal(result$label, c("Toxicity", "Max patients", "Other"))
  expect_equal(result$n, c(2, 2, 1))
  expect_equal(
    report_stop_reasons(sims, list())$label,
    colnames(sims@stop_report)
  )
  x <- SimulationsSummaryList(
    A = sims,
    stop_categories = list(Toxicity = "precision")
  )
  expect_equal(x@stop_categories$A, list(Toxicity = "precision"))
  registry <- oc_stat_registry(x)
  expect_equal(registry$prop_stop_tox$fun(sims, x@summaries$A, "A"), 100 / 3)
  expect_error(
    SimulationsSummaryList(
      A = sims,
      stop_categories = list(Toxicity = "unknown")
    ),
    "Unknown"
  )
  attr(sims@stop_report, "stopping_classes") <- NULL
  expect_equal(report_stop_reasons(sims)$label, colnames(sims@stop_report))
  x <- SimulationsSummaryList(A = sims)
  expect_true(is.na(oc_stat_registry(x)$prop_stop_tox$fun(
    sims,
    x@summaries$A,
    "A"
  )))
})

test_that("registry calculations and formats retain numeric components", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, target = c(0.1, 0.2))
  registry <- oc_stat_registry(x, sd = TRUE)
  evaluate <- function(name) {
    h_eval_oc_stat(registry[[name]], sims, x@summaries$A, "A")
  }
  expect_equal(evaluate("n_pat_mean")$value[[1]], 5 / 3)
  expect_equal(evaluate("n_pat_mean_sd")$value[[1]], c(5 / 3, sd(c(3, 2, 0))))
  expect_equal(evaluate("n_pat_mean_sd")$formatted, "1.7 (1.5)")
  expect_equal(evaluate("n_pat_minmax")$formatted, "[0, 3]")
  expect_equal(evaluate("n_pat_quantiles")$value[[1]], c(.4, 2.8))
  expect_equal(evaluate("n_dlt_mean")$value[[1]], 1)
  expect_equal(evaluate("n_dlt_mean_sd")$value[[1]], c(1, 1))
  expect_equal(evaluate("prop_stop_tox")$value[[1]], 200 / 3)
  expect_equal(evaluate("prop_stop_max_patients")$value[[1]], 200 / 3)
  expect_equal(evaluate("prop_mtd_correct")$value[[1]], 200 / 3)
  expect_equal(evaluate("prop_mtd_over")$value[[1]], 0)
  expect_equal(evaluate("prop_mtd_none")$value[[1]], 100 / 3)
  expect_equal(
    unlist(evaluate("dose_selection")$value),
    c(100 / 3, 100 / 3, 0, 100 / 3)
  )
  expect_equal(evaluate("n_pat_per_dose")$value[[1]], c(2 / 3, sd(c(2, 0, 0))))
  expect_equal(evaluate("n_dlt_per_dose")$value[[2]], c(2 / 3, sd(c(1, 1, 0))))
  expect_equal(unlist(evaluate("true_prob")$value), c(.1, .2, .4))
  expect_equal(evaluate("n_above_target_mean_sd")$value[[1]], c(0, 0))
  expect_true(all(is.na(evaluate("duration_mean_sd")$value[[1]])))
  expect_true("additional:score" %in% names(registry))
  expect_false("additional:curve" %in% names(registry))
  expect_true(is.na(evaluate("additional:score")$value[[1]]))
  sims@additional_stats[[3]]$score <- 3
  expect_equal(
    h_eval_oc_stat(
      registry[["additional:score"]],
      sims,
      x@summaries$A,
      "A"
    )$value[[1]],
    2
  )
  expect_error(oc_stat_registry(quantiles = c(.9, .1)), "increasing")
  expect_error(oc_stat_registry(sd = NA))
  # Every offered entry satisfies the evaluation contract.
  for (stat in registry) {
    expect_s3_class(h_eval_oc_stat(stat, sims, x@summaries$A, "A"), "tbl_df")
  }
})

test_that("custom statistics support vector cells, multiple rows and relabeling", {
  sims <- oc_test_sims()
  summary <- summary(sims)
  custom <- OCStat(
    "Custom",
    function(sims, summary, scenario) list(one = 1:2, two = c(3, 4)),
    function(value) paste(value, collapse = "/")
  )
  result <- h_eval_oc_stat(custom, sims, summary, "A")
  expect_equal(result$label, c("Custom one", "Custom two"))
  expect_equal(result$formatted, c("1/2", "3/4"))
  stat <- oc_stat_registry()$n_dlt_mean
  stat$label <- "Relabeled"
  expect_equal(h_eval_oc_stat(stat, sims, summary, "A")$label, "Relabeled")
  expect_error(OCStat("", identity))
  expect_error(OCStat("A", 1))
  invalid <- OCStat("A", function(sims, summary, scenario) "bad")
  expect_error(h_eval_oc_stat(invalid, sims, summary, "A"), "numeric vector")
  invalid$fun <- function(sims, summary, scenario) list(1)
  expect_error(h_eval_oc_stat(invalid, sims, summary, "A"), "names")
})

test_that("duration uses recorded units and absent derived values remain unavailable", {
  sims <- oc_test_sims()
  sims <- .DASimulations(sims, trial_duration = c(2, 5, 8))
  x <- SimulationsSummaryList(A = sims)
  registry <- oc_stat_registry(x)
  expect_equal(registry$duration_mean_sd$fun(sims, x@summaries$A, "A"), c(5, 3))
  general <- GeneralSimulations(
    data = sims@data,
    doses = sims@doses,
    seed = 1L,
    truth = sims@truth
  )
  x <- SimulationsSummaryList(A = general)
  expect_equal(
    oc_stat_registry(x)$prop_stop_tox$fun(general, x@summaries$A, "A"),
    NA_real_
  )
  expect_equal(
    registry[["additional:score"]]$fun(general, x@summaries$A, "A"),
    NA_real_
  )
})

test_that("legacy or unlabeled reports do not imply zero stopping percentages", {
  sims <- oc_test_sims()
  colnames(sims@stop_report)[1:2] <- NA_character_
  x <- SimulationsSummaryList(A = sims)
  expect_true(is.na(oc_stat_registry(x)$prop_stop_tox$fun(
    sims,
    x@summaries$A,
    "A"
  )))
  expect_equal(
    h_calc_report_label_percentage(oc_test_sims()@stop_report),
    c(unsafe = 200 / 3, unsafe2 = 100 / 3, size = 200 / 3, precision = 100 / 3)
  )
  sims <- oc_test_sims()
  sims@additional_stats <- list(
    list(other = 1),
    list(other = 2),
    list(other = 3)
  )
  x <- SimulationsSummaryList(A = oc_test_sims(), B = sims)
  registry <- oc_stat_registry(x)
  expect_true(is.na(registry[["additional:score"]]$fun(
    sims,
    x@summaries$B,
    "B"
  )))
  expect_equal(registry[["additional:other"]]$fun(sims, x@summaries$B, "B"), 2)
})

test_that("collections reject unsupported data and preserve placebo totals", {
  sims <- oc_test_sims()
  sims@data[[2]]@doseGrid <- c(1, 2, 3)
  expect_error(SimulationsSummaryList(A = sims), "same dose grid")
  expect_error(
    SimulationsSummaryList(A = .DefaultComboSimulations()),
    "GeneralSimulations"
  )
  expect_error(
    SimulationsSummaryList(A = .DefaultDualSimulations()),
    "dual-endpoint"
  )
  sims <- oc_test_sims()
  sims@truth <- function(dose) rep(2, length(dose))
  expect_error(SimulationsSummaryList(A = sims), "probability")
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
  registry <- oc_stat_registry(x)
  expect_equal(registry$n_pat_mean$fun(sims, x@summaries$A, "A"), 3)
  expect_equal(registry$n_dlt_mean$fun(sims, x@summaries$A, "A"), 1)
})
