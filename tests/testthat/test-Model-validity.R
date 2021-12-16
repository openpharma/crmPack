# validate_model_general ----

test_that("validate_model_general passes for valid object", {
  object <- .GeneralModel(
    datanames = "x",
    datamodel = function(x) {
      x
    },
    priormodel = function(x) {
      x
    },
    modelspecs = function(x) {
      x
    },
    init = function(x) {
      x
    },
    sample = "param1"
  )
  expect_true(validate_model_general(object))
})

test_that("validate_model_general returns error msg. for non-valid object", {
  object <- .GeneralModel(
    datanames = "x",
    datamodel = function(x) {
      x
    },
    priormodel = function(x) {
      x
    },
    modelspecs = function(x) {
      x
    },
    init = function(x) {
      x
    },
    sample = "param1"
  )
  object@datanames <- "y"

  expect_equal(
    validate_model_general(object),
    "Arguments of the init function must be data names"
  )
})
