# validate_stopping_mtd_cv ----

test_that("validate_stopping_mtd_cv passes for valid object", {
  object <- StoppingMTDCV(target = 0.3, thresh_cv = 30)
  expect_true(validate_stopping_mtd_cv(object))
})

test_that("validate_stopping_mtd_cv returns expected messages for non-valid object", {
  object <- StoppingMTDCV()
  object@target <- c(1.3, -5)
  object@thresh_cv <- c(2.3, -7)

  expect_equal(
    validate_stopping_mtd_cv(object),
    c(
      "target must be probability > 0 and < 1",
      "thresh_cv must be percentage > 0"
    )
  )
})
