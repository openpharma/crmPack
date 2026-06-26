#' @include("helper-HierarchicalModel.R")
#' @include("helper-McmcOptions.R")

skip_on_cran_but_not_ci()
options(testthat.progress.max_fails = 0)

local_scenario_mono_design <- function(data, n_patients = 10L) {
  Design(
    model = local_hierarchical_mono_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = n_patients),
    increments = IncrementsRelative(intervals = c(0), increments = c(1)),
    cohort_size = CohortSizeConst(3),
    data = data,
    startingDose = min(data@doseGrid)
  )
}

local_scenario_combo_design <- function(data, n_patients = 10L) {
  DesignCombo(
    model = local_hierarchical_combo_model(),
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = n_patients),
    increments = IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    ),
    cohort_size = CohortSizeConst(3),
    data = data,
    startingDose = vapply(data@doseGrid, min, numeric(1L))
  )
}

test_that("scenario-HierarchicalDesign returns the expected shortcut results", {
  data <- local_hierarchical_data()
  design <- HierarchicalDesign(
    DesignArm(
      name = "my_mono",
      active = TRUE,
      design = local_scenario_mono_design(data@arms$my_mono)
    ),
    DesignArm(
      name = "my_combo",
      active = TRUE,
      design = local_scenario_combo_design(data@arms$my_combo)
    ),
    exchangeable_parameters = local_hierarchical_parameter_pools()
  )
  mcmc_options <- h_get_mcmc_options(samples = 5)

  result <- scenario(design, data, mcmc_options)

  expect_named(
    result,
    c(
      "data",
      "samples",
      "fit",
      "dose_limit",
      "next_best",
      "next_dose",
      "cohort_size",
      "placebo_cohort_size",
      "stop",
      "stop_report",
      "stop_reason"
    )
  )
  expect_s4_class(result$data, "HierarchicalData")
  expect_s4_class(result$samples, "HierarchicalSamples")

  arm_names <- names(data@arms)
  expect_named(result$fit, arm_names)
  expect_named(result$dose_limit, arm_names)
  expect_named(result$next_best, arm_names)
  expect_named(result$next_dose, arm_names)
  expect_named(result$cohort_size, arm_names)
  expect_named(result$placebo_cohort_size, arm_names)
  expect_named(result$stop, arm_names)
  expect_named(result$stop_report, arm_names)
  expect_named(result$stop_reason, arm_names)

  expect_true(all(vapply(result$fit, is.data.frame, logical(1L))))
  expect_list(result$next_best$my_mono)
  expect_list(result$next_best$my_combo)
  expect_number(result$next_dose$my_mono, lower = 0, na.ok = TRUE)
  expect_named(result$next_dose$my_combo, data@arms$my_combo@drugNames)
  expect_numeric(result$next_dose$my_combo, len = 2, any.missing = TRUE)
  expect_count(result$cohort_size$my_mono, positive = TRUE, na.ok = TRUE)
  expect_count(result$cohort_size$my_combo, positive = TRUE, na.ok = TRUE)
  expect_null(result$placebo_cohort_size$my_mono)
  expect_null(result$placebo_cohort_size$my_combo)
  expect_true(all(vapply(result$stop, is.logical, logical(1L))))
  expect_true(all(vapply(result$stop_reason, is.character, logical(1L))))
})

test_that("scenario-HierarchicalDesign handles historical arms", {
  data <- HierarchicalData(
    arm_a = Data(
      x = c(10, 10, 20, 20),
      y = c(0L, 0L, 0L, 1L),
      doseGrid = c(10, 20, 30),
      ID = 1L:4L,
      cohort = c(1L, 1L, 2L, 2L)
    ),
    historical = Data(
      x = c(10, 20),
      y = c(0L, 1L),
      doseGrid = c(10, 20, 30),
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )
  design <- HierarchicalDesign(
    DesignArm(
      name = "arm_a",
      active = TRUE,
      design = local_scenario_mono_design(data@arms$arm_a)
    ),
    DesignArm(
      name = "historical",
      active = FALSE,
      design = local_scenario_mono_design(data@arms$historical)
    ),
    exchangeable_parameters = list(
      intercept = list(
        arm_a = "alpha0",
        historical = "alpha0"
      ),
      slope = list(
        arm_a = "alpha1",
        historical = "alpha1"
      )
    )
  )

  result <- scenario(design, data, h_get_mcmc_options(samples = 5))

  expect_true(result$stop$historical)
  expect_identical(
    result$stop_reason$historical,
    "Historical arm: not enrolling."
  )
  expect_null(result$next_best$historical)
  expect_null(result$next_dose$historical)
  expect_data_frame(result$fit$historical)
})

test_that("scenario-HierarchicalDesign handles arms that are not yet open", {
  data <- HierarchicalData(
    lead = Data(
      x = c(10, 10, 20, 20),
      y = c(0L, 0L, 0L, 1L),
      doseGrid = c(10, 20, 30),
      ID = 1L:4L,
      cohort = c(1L, 1L, 2L, 2L)
    ),
    pending = Data(
      doseGrid = c(10, 20, 30)
    )
  )
  design <- HierarchicalDesign(
    DesignArm(
      name = "lead",
      active = TRUE,
      design = local_scenario_mono_design(data@arms$lead)
    ),
    DesignArm(
      name = "pending",
      active = TRUE,
      design = local_scenario_mono_design(data@arms$pending),
      open_when = ArmFinishedCondition("lead")
    ),
    exchangeable_parameters = list(
      intercept = list(
        lead = "alpha0",
        pending = "alpha0"
      ),
      slope = list(
        lead = "alpha1",
        pending = "alpha1"
      )
    )
  )

  result <- scenario(design, data, h_get_mcmc_options(samples = 5))

  expect_null(result$next_best$pending)
  expect_null(result$next_dose$pending)
  expect_null(result$cohort_size$pending)
  expect_true(is.na(result$stop$pending))
  expect_identical(result$stop_reason$pending, "Arm is not currently open.")
  expect_data_frame(result$fit$pending)
})
