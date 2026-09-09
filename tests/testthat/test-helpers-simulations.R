test_that("h_barplot_percentages produces consistent results", {
  set.seed(504409)

  plot <- h_barplot_percentages(
    rpois(n = 100, lambda = 2),
    "test"
  )

  expect_doppel("barplot-percentages", plot)
})

test_that("h_barplot_percentages supports fixed-width discrete bars", {
  plot <- h_barplot_percentages(
    x = c(0.001, 0.01, 1, 16),
    description = "test",
    x_is_discrete = TRUE
  )
  plot_data <- ggplot_build(plot)$data[[1L]]

  expect_true(plot$scales$get_scales("x")$is_discrete())
  expect_equal(plot_data$xmax - plot_data$xmin, rep(0.8, 4L))
})

test_that("h_barplot_percentages rotates discrete x-axis labels", {
  plot <- h_barplot_percentages(
    x = c(0.001, 0.01, 0.1, 1),
    description = "MTD estimate",
    x_is_discrete = TRUE,
    axis_text_angle = 30
  )

  expect_equal(plot$theme$axis.text.x$angle, 30)
  expect_equal(plot$theme$axis.text.x$hjust, 1)
})

test_that("h_histogram_percentages creates a percentage histogram", {
  plot <- h_histogram_percentages(
    x = seq(0, 100, length.out = 100),
    description = "test",
    bins = 10L
  )
  plot_data <- ggplot_build(plot)$data[[1L]]

  expect_s3_class(plot$layers[[1L]]$stat, "StatBin")
  expect_equal(sum(plot_data$y), 100)
})

test_that("barplot_percentages fails gracefully with bad input", {
  expect_error(
    h_barplot_percentages(
      as.character(sample(LETTERS, replace = TRUE, size = 100)),
      "test"
    ),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    h_barplot_percentages(rpois(n = 100, lambda = 2), "test", -3),
    "Assertion on 'xaxisround' failed: Element 1 is not >= 0."
  )
  expect_error(
    h_barplot_percentages(rpois(n = 100, lambda = 2), "test", "bad"),
    "Assertion on 'xaxisround' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    h_barplot_percentages(rpois(n = 100, lambda = 2), c("test", "oops")),
    "Assertion on 'description' failed: Must have length 1, but has length 2."
  )
  expect_error(
    h_barplot_percentages(rpois(n = 100, lambda = 2), 99),
    "Assertion on 'description' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    h_barplot_percentages(
      rpois(n = 100, lambda = 2),
      "test",
      x_is_discrete = 1
    ),
    "Assertion on 'x_is_discrete' failed: Must be of type 'logical flag'"
  )
})

test_that("h_histogram_percentages fails gracefully with bad input", {
  expect_error(
    h_histogram_percentages(letters, "test"),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    h_histogram_percentages(1:10, c("test", "oops")),
    "Assertion on 'description' failed: Must have length 1, but has length 2."
  )
  expect_error(
    h_histogram_percentages(1:10, "test", bins = 0L),
    "Assertion on 'bins' failed"
  )
})


test_that("aggregation of additional stats works correctly", {
  stats_list <- list(
    list(test1 = 1, test2 = 2),
    list(test1 = 3, test2 = 4),
    list(test1 = 5, test2 = 6)
  )
  expect_equal(h_summarize_add_stats(stats_list)[[1]], c("test1", "test2"))
  expect_equal(h_summarize_add_stats(stats_list)[[2]], list(3, 4))
})
