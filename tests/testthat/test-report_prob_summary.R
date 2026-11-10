test_that("probability summaries agree with direct calculations and interval endpoints", {
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 2), alpha1 = c(1, 1, 1)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  data <- Data(doseGrid = c(1, 2))
  p <- prob(1, model, samples)
  result <- report_prob_summary(
    samples,
    model,
    data,
    quantiles = c(0, 0.5, 1),
    intervals = c(0, 0.5, 1)
  )
  expect_s3_class(result, "tbl_ProbSummary")
  expect_equal(result$mean[1], mean(p))
  expect_equal(result$sd[1], sd(p))
  expect_equal(result$q50[1], median(p))
  expect_equal(result$q0[1], min(p))
  expect_equal(result$q100[1], max(p))
  expect_equal(result[["p_[0,0.5)"]][1], 1 / 3)
  expect_equal(result[["p_[0.5,1]"]][1], 2 / 3)
  expect_equal(
    rowSums(as.matrix(result[, c("p_[0,0.5)", "p_[0.5,1]")])),
    c(1, 1)
  )
  expect_named(
    report_prob_summary(
      samples,
      model,
      data,
      mean = FALSE,
      sd = FALSE,
      quantiles = numeric()
    ),
    "dose"
  )
  expect_error(
    report_prob_summary(samples, model, data, intervals = c(0, 1, 0.5)),
    "increasing"
  )
  expect_error(report_prob_summary(
    samples,
    model,
    data,
    quantiles = c(0.5, 0.5)
  ))
  expect_s3_class(report_table(result), "knitr_kable")
  expect_s3_class(knit_print.tbl_ProbSummary(result), "knit_asis")
  if (requireNamespace("gt", quietly = TRUE)) {
    expect_s3_class(report_table(result, "gt"), "gt_tbl")
  }
})

test_that("plot options use requested central statistic, interval and target", {
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 1), alpha1 = c(1, 1, 1)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  data <- Data(doseGrid = c(1, 2))
  plotted <- plot(
    samples,
    model,
    data,
    statistic = "median",
    level = 0.8,
    target = c(0.2, 0.35)
  )
  expected <- fit(
    samples,
    model,
    data,
    middle = median,
    quantiles = c(0.1, 0.9)
  )
  expect_equal(
    plotted$data$y,
    c(expected$middle, expected$lower, expected$upper) * 100
  )
  expect_true("80% Credible Interval" %in% levels(plotted$data$Type))
  expect_equal(length(plotted$layers), 2L)
  expect_equal(length(plot(samples, model, data, target = 0.3)$layers), 2L)
  expect_error(plot(samples, model, data, level = 1), "strictly")
  expect_error(plot(samples, model, data, target = c(0.5, 0.2)), "increasing")
})

test_that("random curves preserve joint draws and seeded RNG state", {
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 1), alpha1 = c(1, 2, 3)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  data <- Data(doseGrid = c(1, 2))
  set.seed(42)
  state <- .Random.seed
  plotted <- plot_prior_draws(samples, model, data, n = 2, seed = 5)
  expect_identical(.Random.seed, state)
  expect_identical(
    plotted$data,
    plot_prior_draws(samples, model, data, n = 2, seed = 5)$data
  )
  expect_equal(length(unique(plotted$data$draw)), 2L)
  for (draw in 1:2) {
    curve <- plotted$data[plotted$data$draw == draw, ]
    index <- which.min(abs(
      prob(1, model, samples) * 100 - curve$probability[1]
    ))
    expected <- vapply(
      curve$dose,
      function(dose) prob(dose, model, samples)[index],
      numeric(1)
    )
    expect_equal(curve$probability, expected * 100)
  }
  expect_error(plot_prior_draws(samples, model, data, n = 4), "must not exceed")
  expect_error(plot_prior_draws(samples, model, data, n = 0))
  expect_s3_class(ggplot2::ggplot_build(plotted), "ggplot_built")
})

test_that("probability endpoints and single-dose summaries retain their meaning", {
  samples <- h_as_samples(list(alpha0 = c(-1000, 1000), alpha1 = c(1, 1)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  data <- Data(doseGrid = 1)
  result <- report_prob_summary(samples, model, data, intervals = c(0, 0.5, 1))
  expect_equal(result[["p_[0,0.5)"]], 0.5)
  expect_equal(result[["p_[0.5,1]"]], 0.5)
  expect_equal(result$mean, 0.5)
  expect_equal(nrow(result), 1L)
  single <- h_as_samples(list(alpha0 = 0, alpha1 = 1))
  expect_true(is.na(report_prob_summary(single, model, data)$sd))
})

test_that("prior display follows interval, mean, SD, quantile order with native spanners", {
  samples <- h_as_samples(list(alpha0 = c(-2, 0, 2), alpha1 = c(1, 1, 1)))
  model <- LogisticNormal(mean = c(0, 1), cov = diag(2), ref_dose = 1)
  x <- report_prob_summary(
    samples,
    model,
    Data(doseGrid = c(1, 2)),
    intervals = c(0, .2, .35, 1)
  )
  original <- x
  expected <- c(
    "dose",
    "p_[0,0.2)",
    "p_[0.2,0.35)",
    "p_[0.35,1]",
    "mean",
    "sd",
    "q2.5",
    "q50",
    "q97.5"
  )
  expect_equal(names(report_table(x, "data.frame")), expected)
  expect_identical(x, original)
  skip_if_not_installed("gt")
  table <- report_table(x, "gt")
  expect_equal(names(table[["_data"]]), expected)
  expect_setequal(
    unlist(table[["_spanners"]]$spanner_label),
    c("Quantiles", "Probability of true DLT rate in ...")
  )
})
