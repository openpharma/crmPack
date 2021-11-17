# Data ----

test_that("Data can be created with default constructor Data.", {
  res <- expect_silent(Data())
  expect_type(res, "S4")
  expect_true(validObject(res))
})
