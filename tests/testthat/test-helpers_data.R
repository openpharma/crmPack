test_that("h_plot_data_dataordinal works as expected", {
  data <- h_get_data()

  result <- h_plot_data_dataordinal(x = data)
  expect_doppel(
    "Helper result of data plot",
    result
  )

  result_backfill <- h_plot_data_dataordinal(
    x = data,
    mark_backfill = TRUE
  )
  expect_doppel(
    "Helper result of data plot with backfill",
    result_backfill
  )
})

test_that("h_plot_data_dataordinal handles single toxicity level (all no)", {
  data_all_no <- Data(
    x = c(1, 1, 1),
    y = c(0L, 0L, 0L),
    doseGrid = c(0.5, 1, 2),
    placebo = FALSE,
    ID = 1:3,
    cohort = c(1L, 1L, 1L),
    backfilled = c(FALSE, FALSE, FALSE)
  )

  result <- h_plot_data_dataordinal(x = data_all_no)
  expect_s3_class(result, "ggplot")
})

test_that("h_plot_data_dataordinal handles single toxicity level (all yes)", {
  data_all_yes <- Data(
    x = c(1, 1),
    y = c(1L, 1L),
    doseGrid = c(0.5, 1, 2),
    placebo = FALSE,
    ID = 1:2,
    cohort = c(1L, 1L),
    backfilled = c(FALSE, FALSE)
  )

  result <- h_plot_data_dataordinal(x = data_all_yes)
  expect_s3_class(result, "ggplot")
})
