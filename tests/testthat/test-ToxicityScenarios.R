#' Deterministic Toxicity Scenarios for Reporting Tests
#' @return Two true curves evaluated on three doses.
#' @keywords internal
toxicity_test_scenarios <- function(target = c(0.2, 0.35)) {
  ToxicityScenarios(
    list("Z low" = function(dose) dose / 100,
         "A high" = function(dose) dose / 50),
    dose_grid = c(5, 10, 20), target = target
  )
}

test_that("toxicity scenarios retain unrounded probabilities and input order", {
  x <- toxicity_test_scenarios()
  expect_s4_class(x, "CrmPackClass")
  expect_true(validObject(x))
  result <- tidy(x)
  expect_s3_class(result, "tbl_ToxicityScenarios")
  expect_identical(names(result), c("scenario", "dose", "prob"))
  expect_equal(result$scenario, rep(c("Z low", "A high"), each = 3))
  expect_equal(result$dose, rep(c(5, 10, 20), 2))
  expect_equal(result$prob, c(.05, .1, .2, .1, .2, .4))
  expect_length(toxicity_test_scenarios(NULL)@target, 0)
})

test_that("toxicity scenario validation rejects malformed inputs and curves", {
  fun <- function(dose) dose / 100
  expect_error(ToxicityScenarios(list(fun), 1:3), "unique, non-empty names")
  expect_error(ToxicityScenarios(list(a = fun, a = fun), 1:3), "unique")
  expect_error(ToxicityScenarios(list(a = 1), 1:3), "functions")
  for (grid in list(numeric(), c(2, 1), c(1, 1), c(1, Inf), c(NA, 1), -1)) {
    expect_error(ToxicityScenarios(list(a = fun), grid), "dose_grid")
  }
  for (target in list(.2, c(.3, .2), c(.2, .2), c(0, 2), c(NA, .3))) {
    expect_error(ToxicityScenarios(list(a = fun), 1:3, target), "target")
  }
  for (value in list(0.2, c(.1, NA, .2), c(.1, Inf, .2), c(-.1, .2, .3),
                     c(.1, .2, 1.1), letters[1:3], matrix(.2, 3))) {
    bad <- local({ result <- value; function(dose) result })
    expect_error(ToxicityScenarios(list(bad = bad), 1:3), "Scenario 'bad'.*probability")
  }
  expect_error(ToxicityScenarios(list(bad = function(dose) stop("broken")), 1:3),
               "Scenario 'bad' failed: broken")
  expect_true(validObject(ToxicityScenarios(list(a = function(dose) c(0, 1)), c(0, 1))))
  x <- toxicity_test_scenarios()
  x@scenarios[[1]] <- function(dose) rep(2, length(dose))
  expect_error(tidy(x), "probability")
})

test_that("toxicity tables preserve pairing and support both renderers", {
  x <- toxicity_test_scenarios()
  table <- report_table(x, digits = 2)
  expect_s3_class(table, "knitr_kable")
  expect_match(paste(table, collapse = "\n"), "Dose 5.*Dose 10.*Dose 20")
  expect_match(paste(table, collapse = "\n"), "Z low.*0.05.*0.10.*0.20")
  expect_match(paste(table, collapse = "\n"), "A high.*0.10.*0.20.*0.40")
  data <- tidy(x)
  reordered <- data[c(1, 5, 3, 4, 2, 6), ]
  expect_equal(report_table(reordered, digits = 2), table)
  expect_error(report_table(data[-1, ]), "exactly one row")
  expect_error(report_table(data[c(1, 1, 3:6), ]), "exactly one row")
  expect_error(report_table(x, digits = -1), "digits")
  expect_s3_class(knitr::knit_print(x), "knit_asis")
  expect_equal(knitr::knit_print(x), knitr::knit_print(data))
  expect_equal(table, report_table(data, digits = 2))
  skip_if_not_installed("gt")
  rendered <- report_table(x, format = "gt", digits = 2)
  expect_s3_class(rendered, "gt_tbl")
  expect_equal(rendered[["_data"]][[2]], c(.05, .1))
})

test_that("toxicity plots show the curves and optional target interval", {
  x <- toxicity_test_scenarios()
  p <- plot(x)
  built <- ggplot_build(p)
  expect_equal(built$data[[1]]$ymin, .2)
  expect_equal(built$data[[1]]$ymax, .35)
  expect_equal(built$data[[2]]$y, tidy(x)$prob)
  expect_equal(built$data[[3]]$x, rep(c(5, 10, 20), 2))
  expect_equal(levels(p$data$scenario), names(x@scenarios))
  expect_length(plot(toxicity_test_scenarios(NULL))$layers, 2)
  expect_s3_class(plot(ToxicityScenarios(list(a = function(dose) .2), 1)), "ggplot")
  vdiffr::expect_doppelganger("toxicity scenarios with target interval", p)
})

test_that("model-derived truths work without wrappers", {
  model <- LogisticLogNormal(mean = c(-.85, 1), cov = diag(2), ref_dose = 50)
  truth <- probFunction(model, alpha0 = -3, alpha1 = 1)
  grid <- c(5, 10, 20)
  x <- ToxicityScenarios(list(Model = truth), grid)
  expect_equal(tidy(x)$prob, truth(grid))
})

test_that("toxicity markdown table output is stable", {
  expect_snapshot(cat(paste(report_table(toxicity_test_scenarios(), digits = 2),
                            collapse = "\n")))
})

test_that("gt rendering gives an actionable missing-dependency error", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(report_table(toxicity_test_scenarios(), format = "gt"),
               "Package 'gt' is required")
})
