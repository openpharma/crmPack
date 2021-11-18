# Data ----

test_that("Data can be created with default constructor Data.", {
  res <- expect_silent(Data())
  expect_s4_class(res, "Data")
  expect_true(validObject(res))
})
