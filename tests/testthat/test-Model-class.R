# AllModels-class ----

test_that(".AllModels works as expected", {
  result <- expect_silent(.AllModels(datanames = "x"))
  expect_valid(result, "AllModels")
})

# GeneralModel-class ----

test_that(".GeneralModel works as expected", {
  # nolint start
  result <- expect_silent(
    .GeneralModel(
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x"
    )
  )
  # nolint end
  expect_valid(result, "GeneralModel")
})

# Model-class ----

test_that(".Model works as expected", {
  # nolint start
  result <- expect_silent(
    .Model(
      dose = function(prob, param1) {},
      prob = function(dose, param1) {},
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x"
    )
  )
  # nolint end
  expect_valid(result, "Model")
})

# LogisticLogNormal-class ----

test_that(".LogisticLogNormal works as expected", {
  # nolint start
  result <- expect_silent(
    .LogisticLogNormal(
      dose = function(prob, param1) {},
      prob = function(dose, param1) {},
      datamodel = function(x) {},
      priormodel = function(x) {},
      modelspecs = function(x) {},
      init = function(x) {},
      sample = "param1",
      datanames = "x"
    )
  )
  # nolint end
  expect_valid(result, "LogisticLogNormal")
})

# LogisticLogNormal-constructor ----

test_that("LogisticLogNormal object can be created with user constructor", {
  result <- expect_silent(
    LogisticLogNormal(
      mean = c(1, 5),
      cov = diag(4, ncol = 2, nrow = 2),
      refDose = 2
    )
  )
  expect_valid(result, "LogisticLogNormal")
})
