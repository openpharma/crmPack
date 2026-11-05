# Extracted from test-Simulations-methods.R:1248

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "crmPack", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran_but_not_ci()
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

# test -------------------------------------------------------------------------
skip_on_cran()
emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25))
model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
    ref_dose = 56
  )
design <- Design(
    model = model,
    nextBest = NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    ),
    stopping = StoppingMinPatients(nPatients = 6),
    increments = IncrementsRelative(
      intervals = c(0, 20),
      increments = c(1, 0.33)
    ),
    cohort_size = CohortSizeConst(size = 3),
    data = emptydata,
    startingDose = 3
  )
myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)
options <- McmcOptions(
    burnin = 10,
    step = 2,
    samples = 20,
    rng_kind = "Mersenne-Twister",
    rng_seed = 123
  )
mySims <- simulate(
    design,
    args = NULL,
    truth = myTruth,
    nsim = 3,
    seed = 819,
    mcmcOptions = options,
    parallel = FALSE
  )
simSummary <- summary(mySims, truth = myTruth)
result_mean_fit <- plot(simSummary, type = "meanFit")
expect_s3_class(result_mean_fit, "ggplot")
expect_null(result_mean_fit$scales$get_scales("linetype")$name)
expect_null(result_mean_fit$scales$get_scales("colour")$name)
expect_null(result_mean_fit$scales$get_scales("fill")$name)
expect_s3_class(result_mean_fit$layers[[1L]]$geom, "GeomRect")
expect_equal(result_mean_fit$layers[[1L]]$data$ymin, 20)
expect_equal(result_mean_fit$layers[[1L]]$data$ymax, 35)
expect_identical(
    result_mean_fit$scales$get_scales("fill")$guide$params$order,
    1L
  )
expect_identical(result_mean_fit$guides$guides$linetype$params$order, 2L)
expect_identical(result_mean_fit$guides$guides$colour$params$order, 2L)
expect_doppel("plot_simSimsSummary_meanFit", result_mean_fit)
