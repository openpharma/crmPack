# RuleDesign ----

test_that(".RuleDesign works as expected", {
  result <- expect_silent(.RuleDesign())
  expect_valid(result, "RuleDesign")
})

test_that("RuleDesign object can be created with user constructor", {
  next_best <- NextBestThreePlusThree()
  cohort_size <- CohortSizeConst(size = 5L)
  data <- Data(doseGrid = 2:40)

  result <- expect_silent(
    RuleDesign(next_best, cohort_size, data, 5)
  )
  expect_valid(result, "RuleDesign")
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, data)
  expect_identical(result@startingDose, 5)
})

test_that("RuleDesign user constructor arguments names are as expected", {
  expect_function(
    RuleDesign,
    args = c("nextBest", "cohort_size", "data", "startingDose"),
    ordered = TRUE
  )
})

test_that("RuleDesign object can be created with ThreePlusThreeDesign constructor", {
  dose_grid <- c(8, 10, 15, 25, 35, 50, 80)
  result <- expect_silent(
    ThreePlusThreeDesign(dose_grid)
  )
  expect_valid(result, "RuleDesign")
  expect_identical(result@nextBest, NextBestThreePlusThree())
  expect_identical(result@cohort_size, CohortSizeConst(size = 3L))
  expect_identical(result@data, Data(doseGrid = dose_grid))
  expect_identical(result@startingDose, 8)
})

# ThreePlusThreeDesign ----

test_that("ThreePlusThreeDesign constructor arguments names are as expected", {
  expect_function(
    ThreePlusThreeDesign,
    args = "doseGrid",
    ordered = TRUE
  )
})

# Design ----

test_that(".Design works as expected", {
  result <- expect_silent(.Design())
  expect_valid(result, "Design")
})

test_that("Design object can be created with user constructor", {
  empty_data <- Data(doseGrid = 2:50)
  model <- h_get_model_log_normal()
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()
  placebo_cohort_size <- CohortSizeConst(0L)
  next_best <- h_next_best_ncrm()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

  result <- expect_silent(
    Design(
      model,
      stopping,
      increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "Design")
  expect_identical(result@model, model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, placebo_cohort_size)
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)

  result <- expect_silent(
    Design(
      model,
      stopping,
      increments,
      CohortSizeConst(2L),
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_identical(result@pl_cohort_size, CohortSizeConst(2L))
})

test_that("Design user constructor arguments names are as expected", {
  expect_function(
    Design,
    args = c(
      "model",
      "stopping",
      "increments",
      "pl_cohort_size",
      "backfill",
      "..."
    ),
    ordered = TRUE
  )
})

# DualDesign ----

test_that(".DualDesign works as expected", {
  result <- expect_silent(.DualDesign())
  expect_valid(result, "DualDesign")
})

test_that("DualDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_dual_endpoint_rw()
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()
  next_best <- h_next_best_dual_endpoint()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

  result <- expect_silent(
    DualDesign(
      model,
      empty_data,
      stopping = stopping,
      increments = increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      startingDose = 3
    )
  )
  expect_valid(result, "DualDesign")
  expect_identical(result@model, model)
  expect_identical(result@data, empty_data)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@startingDose, 3)
})

test_that("DualDesign user constructor arguments names are as expected", {
  expect_function(
    DualDesign,
    args = c("model", "data", "..."),
    ordered = TRUE
  )
})

# TDsamplesDesign ----

test_that(".TDsamplesDesign works as expected", {
  result <- expect_silent(.TDsamplesDesign())
  expect_valid(result, "TDsamplesDesign")
})

test_that("TDsamplesDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  stopping <- StoppingMinPatients(nPatients = 30)
  increments <- h_increments_relative()
  next_best <- h_next_best_tdsamples()
  cohort_size <- CohortSizeConst(size = 3)

  result <- expect_silent(
    TDsamplesDesign(
      model,
      stopping,
      increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "TDsamplesDesign")
  expect_identical(result@model, model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, CohortSizeConst(0))
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)

  result <- expect_silent(
    TDsamplesDesign(
      model,
      stopping,
      increments,
      CohortSizeConst(2L),
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_identical(result@pl_cohort_size, CohortSizeConst(2L))
})

test_that("TDsamplesDesign user constructor arguments names are as expected", {
  expect_function(
    TDsamplesDesign,
    args = c("model", "stopping", "increments", "pl_cohort_size", "..."),
    ordered = TRUE
  )
})

# TDDesign ----

test_that(".TDDesign works as expected", {
  result <- expect_silent(.TDDesign())
  expect_valid(result, "TDDesign")
})

test_that("TDDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  stopping <- StoppingMinPatients(nPatients = 30)
  increments <- h_increments_relative()
  next_best <- NextBestTD(prob_target_drt = 0.35, prob_target_eot = 0.3)
  cohort_size <- CohortSizeConst(size = 3)

  result <- expect_silent(
    TDDesign(
      model,
      stopping,
      increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "TDDesign")
  expect_identical(result@model, model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, CohortSizeConst(0))
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)

  result <- expect_silent(
    TDDesign(
      model,
      stopping,
      increments,
      CohortSizeConst(2L),
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_identical(result@pl_cohort_size, CohortSizeConst(2L))
})

test_that("TDDesign user constructor arguments names are as expected", {
  expect_function(
    TDDesign,
    args = c("model", "stopping", "increments", "pl_cohort_size", "..."),
    ordered = TRUE
  )
})

# DualResponsesSamplesDesign ----

test_that(".DualResponsesSamplesDesign works as expected", {
  result <- expect_silent(.DualResponsesSamplesDesign())
  expect_valid(result, "DualResponsesSamplesDesign")
})

test_that("DualResponsesSamplesDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  eff_model <- h_get_eff_log_log(emptydata = TRUE)
  stopping <- StoppingMinPatients(nPatients = 30)
  increments <- h_increments_relative()
  next_best <- h_next_best_tdsamples()
  cohort_size <- CohortSizeConst(size = 3)

  result <- expect_silent(
    DualResponsesSamplesDesign(
      model = model,
      eff_model = eff_model,
      stopping = stopping,
      increments = increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "DualResponsesSamplesDesign")
  expect_identical(result@model, model)
  expect_identical(result@eff_model, eff_model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, CohortSizeConst(0))
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)
})

test_that("DualResponsesSamplesDesign user constructor arguments names are as expected", {
  expect_function(
    DualResponsesSamplesDesign,
    args = c("eff_model", "data", "..."),
    ordered = TRUE
  )
})

# DualResponsesDesign ----

test_that(".DualResponsesDesign works as expected", {
  result <- expect_silent(.DualResponsesDesign())
  expect_valid(result, "DualResponsesDesign")
})

test_that("DualResponsesDesign object can be created with user constructor", {
  empty_data <- DataDual(doseGrid = 2:50)
  model <- h_get_logistic_indep_beta(emptydata = TRUE)
  eff_model <- h_get_eff_log_log(emptydata = TRUE)
  stopping <- StoppingMinPatients(nPatients = 30)
  increments <- h_increments_relative()
  next_best <- h_next_best_tdsamples()
  cohort_size <- CohortSizeConst(size = 3)

  result <- expect_silent(
    DualResponsesDesign(
      model = model,
      eff_model = eff_model,
      stopping = stopping,
      increments = increments,
      nextBest = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      startingDose = 3
    )
  )
  expect_valid(result, "DualResponsesDesign")
  expect_identical(result@model, model)
  expect_identical(result@eff_model, eff_model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, CohortSizeConst(0))
  expect_identical(result@nextBest, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@startingDose, 3)
})

test_that("DualResponsesDesign user constructor arguments names are as expected", {
  expect_function(
    DualResponsesDesign,
    args = c("eff_model", "data", "..."),
    ordered = TRUE
  )
})

# DADesign ----

test_that(".DADesign works as expected", {
  result <- .DADesign()

  expect_true(inherits(result, "Design"))

  expect_valid(result, "DADesign")

  expect_true(identical(result@model, .DALogisticLogNormal()))
  expect_true(identical(result@data, DataDA(doseGrid = 1:2)))
  expect_true(identical(result@safetyWindow, .SafetyWindowConst()))
})

test_that("DADesign constructor works as expected", {
  model <- .DALogisticLogNormal()
  data <- DataDA(doseGrid = 1:10)
  safety_window <- .SafetyWindowConst()
  next_best <- .NextBestNCRM()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
  starting_dose <- 3
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()

  result <- expect_silent(
    DADesign(
      model = model,
      data = data,
      nextBest = next_best,
      safetyWindow = safety_window,
      cohort_size = cohort_size,
      startingDose = starting_dose,
      stopping = stopping,
      increments = increments
    )
  )

  expect_valid(result, "DADesign")
  expect_true(inherits(result, "Design"))
  expect_true(inherits(result, "DADesign"))

  expect_true(identical(result@model, model))
  expect_true(identical(result@data, data))
  expect_true(identical(result@safetyWindow, safety_window))
  expect_true(identical(result@nextBest, next_best))
  expect_true(identical(result@cohort_size, cohort_size))
  expect_true(identical(result@startingDose, starting_dose))
  expect_true(identical(result@stopping, stopping))
  expect_true(identical(result@increments, increments))
})

test_that("DADesign user constructor arguments names are as expected", {
  expect_function(
    Design,
    args = c("model", "stopping", "increments", "pl_cohort_size", "..."),
    ordered = TRUE
  )
})

# DesignGrouped ----

test_that(".DesignGrouped works as expected", {
  result <- .DesignGrouped()

  expect_true(inherits(result, "CrmPackClass"))
  expect_valid(result, "DesignGrouped")
})

test_that("DesignGrouped works as expected", {
  empty_data <- Data(doseGrid = 2:50)
  model <- .DefaultLogisticLogNormalGrouped()
  stopping <- h_stopping_target_prob()
  increments <- h_increments_relative()
  placebo_cohort_size <- CohortSizeConst(0L)
  next_best <- h_next_best_ncrm()
  cohort_size <- CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))

  result <- expect_silent(
    DesignGrouped(
      model = model,
      mono = Design(
        model,
        stopping,
        increments,
        nextBest = next_best,
        cohort_size = cohort_size,
        data = empty_data,
        startingDose = 3
      ),
      stop_mono_with_combo = TRUE
    )
  )

  expect_valid(result, "DesignGrouped")
  slots_except_stopping <- setdiff(slotNames(result@mono), "stopping")
  sapply(
    slots_except_stopping,
    function(x) expect_identical(slot(result@mono, x), slot(result@combo, x))
  )
  expect_class(result@mono@stopping, "StoppingAny")
  expect_class(result@combo@stopping, "StoppingTargetProb")
  expect_true(result@first_cohort_mono_only)
  expect_true(result@same_dose_for_all)
})

test_that(".DefaultDesignGrouped works as expected", {
  result <- .DefaultDesignGrouped()
  expect_valid(result, "DesignGrouped")
})

# RuleDesignOrdinal ----

test_that(".RuleDesignOrdinal works as expected", {
  result <- expect_silent(.RuleDesignOrdinal())
  expect_valid(result, "RuleDesignOrdinal")
})

test_that("RuleDesign object can be created with user constructor", {
  next_best <- NextBestOrdinal(
    1L,
    NextBestMTD(
      target = 0.2,
      derive = function(x) median(x, na.rm = TRUE)
    )
  )
  cohort_size <- CohortSizeOrdinal(1L, CohortSizeConst(size = 5L))
  data <- DataOrdinal(doseGrid = 2:40)

  result <- expect_silent(
    RuleDesignOrdinal(next_best, cohort_size, data, 5)
  )
  expect_valid(result, "RuleDesignOrdinal")
  expect_identical(result@next_best, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, data)
  expect_identical(result@starting_dose, 5)
})

test_that("RuleDesignOrdinal user constructor arguments names are as expected", {
  expect_function(
    RuleDesignOrdinal,
    args = c("next_best", "cohort_size", "data", "starting_dose"),
    ordered = TRUE
  )
})

# DesignOrdinal ----

test_that(".DesignOrdinal works as expected", {
  result <- expect_silent(.DesignOrdinal())
  expect_valid(result, "DesignOrdinal")
})

test_that("DesignOrdinal object can be created with user constructor", {
  empty_data <- DataOrdinal(doseGrid = 2:50)
  model <- .DefaultLogisticLogNormalOrdinal()
  stopping <- .DefaultStoppingOrdinal()
  increments <- .DefaultIncrementsOrdinal()
  placebo_cohort_size <- CohortSizeOrdinal(1L, CohortSizeConst(0L))
  next_best <- .DefaultNextBestOrdinal()
  cohort_size <- CohortSizeOrdinal(
    1L,
    CohortSizeRange(intervals = c(0, 30), cohort_size = c(1, 3))
  )

  result <- expect_silent(
    DesignOrdinal(
      model,
      stopping,
      increments,
      next_best = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      starting_dose = 3
    )
  )
  expect_valid(result, "DesignOrdinal")
  expect_identical(result@model, model)
  expect_identical(result@stopping, stopping)
  expect_identical(result@increments, increments)
  expect_identical(result@pl_cohort_size, placebo_cohort_size)
  expect_identical(result@next_best, next_best)
  expect_identical(result@cohort_size, cohort_size)
  expect_identical(result@data, empty_data)
  expect_identical(result@starting_dose, 3)

  result <- expect_silent(
    DesignOrdinal(
      model,
      stopping,
      increments,
      CohortSizeOrdinal(2L, CohortSizeConst(2L)),
      next_best = next_best,
      cohort_size = cohort_size,
      data = empty_data,
      starting_dose = 3
    )
  )
  expect_identical(
    result@pl_cohort_size,
    CohortSizeOrdinal(2L, CohortSizeConst(2L))
  )
})

test_that("Design user constructor arguments names are as expected", {
  expect_function(
    Design,
    args = c("model", "stopping", "increments", "pl_cohort_size", "..."),
    ordered = TRUE
  )
})
