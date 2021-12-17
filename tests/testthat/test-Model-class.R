# AllModels-class ----

test_that(".AllModels works as expected", {
  result <- expect_silent(.AllModels(datanames = "x"))
  expect_valid(result, "AllModels")
})

# GeneralModel-class ----

test_that(".GeneralModel works as expected", {
  result <- expect_silent(.GeneralModel(
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
    sample = "param1",
    datanames = "x"
  ))
  expect_valid(result, "GeneralModel")
})

# Model-class ----

test_that(".Model works as expected", {
  result <- expect_silent(.Model(
    dose = function(prob, param1) {
      prob
    },
    prob = function(dose, param1) {
      dose
    },
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
    sample = "param1",
    datanames = "x"
  ))
  expect_valid(result, "Model")
})
