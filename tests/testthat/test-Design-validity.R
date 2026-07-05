# RuleDesign ----

## v_rule_design ----

test_that("v_rule_design passes for valid object", {
  object <- RuleDesign(
    nextBest = NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(size = 3L),
    data = Data(doseGrid = 5:20),
    startingDose = 5
  )
  expect_true(v_rule_design(object))
})

test_that("v_rule_design returns message when startingDose is not a valid scalar", {
  err_msg <- "startingDose must be a number"
  err_msg2 <- "startingDose must be included in data@doseGrid"
  object <- RuleDesign(
    nextBest = NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(size = 3L),
    data = Data(doseGrid = 5:20),
    startingDose = 5
  )

  # Changing `startingDose` so that it is not a valid scalar number.
  object@startingDose <- c(5, 6)
  expect_equal(v_rule_design(object), err_msg)

  object@startingDose <- NA_real_
  expect_equal(v_rule_design(object), c(err_msg, err_msg2))

  object@startingDose <- -Inf
  expect_equal(v_rule_design(object), c(err_msg, err_msg2))

  object@startingDose <- Inf
  expect_equal(v_rule_design(object), c(err_msg, err_msg2))

  object@startingDose <- numeric(0)
  expect_equal(v_rule_design(object), c(err_msg, err_msg2))

  object@startingDose <- integer(0)
  expect_equal(v_rule_design(object), c(err_msg, err_msg2))
})

test_that("v_rule_design returns message when startingDose is not on doseGrid", {
  err_msg <- "startingDose must be included in data@doseGrid"
  object <- RuleDesign(
    nextBest = NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(size = 3L),
    data = Data(doseGrid = 5:20),
    startingDose = 5
  )

  # Changing `startingDose` so that it is not on doseGrid.
  object@startingDose <- 4
  expect_equal(v_rule_design(object), err_msg)

  object@startingDose <- 21
  expect_equal(v_rule_design(object), err_msg)

  object@startingDose <- 6.5
  expect_equal(v_rule_design(object), err_msg)
})

## v_design_grouped ----

test_that("v_design_grouped passes for valid object", {
  object <- .DesignGrouped()
  expect_true(v_design_grouped(object))
})

test_that("v_design_grouped identifies wrong flag slots as expected", {
  object <- .DesignGrouped()

  object@same_dose_for_all <- c(NA, TRUE)
  object@first_cohort_mono_only <- logical()
  object@same_dose_for_start <- logical()

  result <- v_design_grouped(object)
  expected <- c(
    "first_cohort_mono_only must be a flag",
    "same_dose_for_all must be a flag",
    "same_dose_for_start must be a flag"
  )
  expect_identical(result, expected)
})

## v_design_combo ----

test_that("v_design_combo passes for valid object", {
  object <- .DesignCombo()
  expect_true(v_design_combo(object))
})

test_that("v_design_combo validates startingDose format and membership", {
  object <- .DesignCombo()

  object@startingDose <- 10
  expect_identical(
    v_design_combo(object),
    c(
      "startingDose must be a numeric vector of length 2",
      "startingDose must have the same names as data@doseGrid"
    )
  )

  object@startingDose <- c(drug1 = 99, drug2 = 20)
  expect_identical(
    v_design_combo(object),
    "startingDose[1] must be included in data@doseGrid[[1]]"
  )

  object@startingDose <- c(drug1 = 10, drug2 = 99)
  expect_identical(
    v_design_combo(object),
    "startingDose[2] must be included in data@doseGrid[[2]]"
  )
})
