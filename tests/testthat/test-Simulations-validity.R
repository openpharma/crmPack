testthat::local_mocked_bindings(
  .DefaultSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultDualSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_dual_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoSimulations = function(...) {
    readRDS(testthat::test_path("fixtures", "default_pseudo_simulations.Rds"))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoDualSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_simulations.Rds"
    ))
  }
)

testthat::local_mocked_bindings(
  .DefaultPseudoDualFlexiSimulations = function(...) {
    readRDS(testthat::test_path(
      "fixtures",
      "default_pseudo_dual_flexi_simulations.Rds"
    ))
  }
)

# GeneralSimulations ----

test_that("v_general_simulations passes for valid object", {
  object <- .DefaultSimulations()
  expect_true(v_general_simulations(object))
})

test_that("v_general_simulations returns message for non-Data in data list", {
  object <- .DefaultSimulations()
  object@data <- list("not a Data object")
  err_msg <- "all data elements must be Data objects"
  expect_equal(v_general_simulations(object), err_msg)
})

test_that("v_general_simulations returns message for mismatched doses length", {
  object <- .DefaultSimulations()
  object@doses <- c(25, 50)
  err_msg <- "doses must have same length as the data list"
  expect_equal(v_general_simulations(object), err_msg)
})

# Simulations ----

test_that("v_simulations passes for valid object", {
  object <- .DefaultSimulations()
  expect_true(v_simulations(object))
})

test_that("v_simulations returns message for mismatched fit length", {
  object <- .DefaultSimulations()
  object@fit <- c(object@fit, object@fit)
  err_msg <- "fit must have same length as data"
  expect_equal(v_simulations(object), err_msg)
})

test_that("v_simulations returns message for mismatched stop_reasons length", {
  object <- .DefaultSimulations()
  object@stop_reasons <- c(object@stop_reasons, object@stop_reasons)
  err_msg <- "stop_reasons must have same length as data"
  expect_equal(v_simulations(object), err_msg)
})

test_that("v_simulations returns message for invalid stop_report matrix", {
  object <- .DefaultSimulations()
  object@stop_report <- matrix(NA, nrow = length(object@data), ncol = 1)
  err_msg <- "stop_report must be a matrix of mode logical in which the number"
  expect_match(v_simulations(object), err_msg)
})

# DualSimulations ----

test_that("v_dual_simulations passes for valid object", {
  object <- .DefaultDualSimulations()
  expect_true(v_dual_simulations(object))
})

test_that("v_dual_simulations returns message for mismatched fit_biomarker length", {
  object <- .DefaultDualSimulations()
  object@fit_biomarker <- c(object@fit_biomarker, object@fit_biomarker)
  err_msg <- "fit_biomarker list has to have same length as data"
  expect_equal(v_dual_simulations(object), err_msg)
})

test_that("v_dual_simulations returns message for mismatched rho_est length", {
  object <- .DefaultDualSimulations()
  object@rho_est <- c(object@rho_est, object@rho_est)
  err_msg <- "rho_est vector has to have same length as data"
  expect_equal(v_dual_simulations(object), err_msg)
})

test_that("v_dual_simulations returns message for mismatched sigma2w_est length", {
  object <- .DefaultDualSimulations()
  object@sigma2w_est <- c(object@sigma2w_est, object@sigma2w_est)
  err_msg <- "sigma2w_est has to have same length as data"
  expect_equal(v_dual_simulations(object), err_msg)
})

test_that("v_dual_simulations returns message for mismatched sigma2w_est length", {
  object <- .DefaultDualSimulations()
  object@sigma2w_est <- c(object@sigma2w_est, object@sigma2w_est)
  err_msg <- "sigma2w_est has to have same length as data"
  expect_equal(v_dual_simulations(object), err_msg)
})

# PseudoSimulations ----

test_that("v_pseudo_simulations passes for valid object", {
  object <- .DefaultPseudoSimulations()
  expect_true(v_pseudo_simulations(object))
})

test_that("v_pseudo_simulations returns message for mismatched stop_reasons length", {
  object <- .DefaultPseudoSimulations()
  object@stop_reasons <- c(object@stop_reasons, object@stop_reasons)
  err_msg <- "stopReasons must have same length as data"
  expect_equal(v_pseudo_simulations(object), err_msg)
})

# PseudoDualSimulations ----

test_that("v_pseudo_dual_simulations passes for valid object", {
  object <- .DefaultPseudoDualSimulations()
  expect_true(v_pseudo_dual_simulations(object))
})

test_that("v_pseudo_dual_simulations returns message for mismatched sigma2_est length", {
  object <- .DefaultPseudoDualSimulations()
  object@sigma2_est <- c(object@sigma2_est, object@sigma2_est)
  err_msg <- "sigma2_est has to have same length as data"
  expect_equal(v_pseudo_dual_simulations(object), err_msg)
})

# PseudoDualFlexiSimulations ----

test_that("v_pseudo_dual_flex_simulations passes for valid object", {
  object <- .DefaultPseudoDualFlexiSimulations()
  expect_true(v_pseudo_dual_flex_simulations(object))
})

test_that("v_pseudo_dual_flex_simulations returns message for mismatched sigma2_beta_w_est length", {
  object <- .DefaultPseudoDualFlexiSimulations()
  object@sigma2_beta_w_est <- c(
    object@sigma2_beta_w_est,
    object@sigma2_beta_w_est
  )
  err_msg <- "sigma2_beta_w_est has to have same length as data"
  expect_equal(v_pseudo_dual_flex_simulations(object), err_msg)
})

# DASimulations ----

test_that("v_da_simulations passes for valid object", {
  object <- expect_silent(.DASimulations())
  expect_true(v_da_simulations(object))
})

test_that("v_da_simulations returns message for mismatched trial_duration length", {
  object <- expect_silent(.DASimulations())
  object@trial_duration <- c(1, 2, 3)
  err_msg <- "trial_duration vector has to have same length as data"
  expect_equal(v_da_simulations(object), err_msg)
})
