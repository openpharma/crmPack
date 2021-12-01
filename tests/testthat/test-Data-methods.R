# as.list ----

test_that("Coercion creates the expected list", {
  object <- Data()
  result <- as.list(object)

  expect_class(result, "list")
  expect_identical(slotNames(object), names(result))
})
