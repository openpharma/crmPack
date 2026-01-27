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
