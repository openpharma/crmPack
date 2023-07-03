# ModelParamsNormal ----

## class ----

test_that(".ModelParamsNormal works as expected", {
  result <- expect_silent(
    .ModelParamsNormal(
      mean = c(1, 5),
      cov = diag(2),
      prec = diag(2)
    )
  )
  expect_valid(result, "ModelParamsNormal")
})

## constructor ----

test_that("ModelParamsNormal object can be created with user constructor", {
  result <- expect_silent(
    ModelParamsNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2)
    )
  )
  expect_valid(result, "ModelParamsNormal")
})

test_that("ModelParamsNormal object can be created also for larger dimensions", {
  result <- expect_silent(
    ModelParamsNormal(
      mean = c(1, 5, 4, 6),
      cov = diag(3, ncol = 4, nrow = 4)
    )
  )
  expect_valid(result, "ModelParamsNormal")
})
