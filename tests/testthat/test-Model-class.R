# AllModels-class ----

test_that(".AllModels works as expected", {
  result <- expect_silent(.AllModels(datanames = "x"))
  expect_valid(result, "AllModels")
})

# GeneralModel-class ----

test_that(".GeneralModel works as expected", {
  result <- expect_silent(.GeneralModel(
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
  ))
  expect_valid(result, "GeneralModel")
})
