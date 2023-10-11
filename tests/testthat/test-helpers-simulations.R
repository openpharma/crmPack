test_that("barplot_percentages produces consistent results", {
  set.seed(504409)

  plot <- crmPack:::barplot_percentages(
    rpois(n = 100, lambda = 2),
    "test"
  )

  vdiffr::expect_doppelganger("barplot-percentages", plot)
})

test_that("barplot_percentages fails gracefully with bad input", {
  expect_error(
    crmPack:::barplot_percentages(
      as.character(sample(LETTERS, replace = TRUE, size = 100)),
      "test"
    ),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    crmPack:::barplot_percentages(rpois(n = 100, lambda = 2), "test", -3),
    "Assertion on 'xaxisround' failed: Element 1 is not >= 0."
  )
  expect_error(
    crmPack:::barplot_percentages(rpois(n = 100, lambda = 2), "test", "bad"),
    "Assertion on 'xaxisround' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    crmPack:::barplot_percentages(rpois(n = 100, lambda = 2), c("test", "oops")),
    "Assertion on 'description' failed: Must have length 1, but has length 2."
  )
  expect_error(
    crmPack:::barplot_percentages(rpois(n = 100, lambda = 2), 99),
    "Assertion on 'description' failed: Must be of type 'character', not 'double'."
  )
})
