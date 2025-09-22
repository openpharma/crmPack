test_that("h_barplot_percentages produces consistent results", {
  set.seed(504409)

  plot <- h_barplot_percentages(
    rpois(n = 100, lambda = 2),
    "test"
  )

  expect_doppel("barplot-percentages", plot)
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
