# validate_general_model ----

test_that("validate_general_model passes for valid object", {
  object <- h_get_general_model()
  expect_true(validate_general_model(object))
})

test_that("validate_general_model returns error msg. for non-valid object", {
  object <- h_get_general_model()
  object@datanames <- "y"

  expect_equal(
    validate_general_model(object),
    "Arguments of the init function must be data names"
  )
})
