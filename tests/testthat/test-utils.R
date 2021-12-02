# dapply ----

df <- data.frame(
  dose = c(0.1, 6, 6, 5, 0.1, 5, 6, 6),
  cohort = c("B", "B", "B", "A", "A", "A", "B", "B")
)

test_that("dapply returns valid object for sample data for max dose function", {
  result <- dapply(
    df,
    f = ~cohort,
    FUN = function(coh) {
      data.frame(my_cohort = coh$cohort[1], my_max = max(coh$dose))
    }
  )
  expected <- data.frame(my_cohort = c("A", "B"), my_max = c(5, 6))

  expect_identical(result, expected)
})

test_that("dapply returns valid objects for sample data for sort dose function", {
  result <- dapply(
    df,
    f = ~cohort,
    FUN = function(coh) {
      coh$dose <- sort(coh$dose, decreasing = TRUE)
      coh
    }
  )
  expected <- data.frame(
    dose = c(5, 5, 0.1, 6, 6, 6, 6, 0.1),
    cohort = c("A", "A", "A", "B", "B", "B", "B", "B")
  )

  expect_identical(result, expected)
})
