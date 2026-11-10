test_that("simulation truth is stored, serialized, and used by summary", {
  design <- RuleDesign(
    nextBest = NextBestThreePlusThree(),
    cohort_size = CohortSizeConst(3L),
    data = Data(doseGrid = c(5, 10, 20)),
    startingDose = 5
  )
  truth <- function(dose) rep(0.3, length(dose))
  sims <- simulate(design, nsim = 2L, seed = 123, truth = truth)
  expect_identical(sims@truth, truth)
  expect_equal(summary(sims), summary(sims, truth = truth))
  override <- function(dose) rep(0.9, length(dose))
  expect_equal(
    summary(sims, truth = override)@tox_at_doses_selected,
    override(sims@doses)
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(sims, path)
  expect_equal(summary(readRDS(path)), summary(sims))
})

test_that("legacy simulation objects require an explicit truth", {
  sims <- readRDS(test_path("fixtures", "default_simulations.Rds"))
  expect_error(summary(sims), "No truth is stored.*Supply the truth explicitly")
  expect_s4_class(summary(sims, truth = plogis), "SimulationsSummary")
})

test_that("nested stopping reports preserve classes in column order", {
  rule <- StoppingAny(
    list(
      StoppingMinPatients(3L, report_label = "Patients"),
      StoppingAll(list(StoppingMinPatients(6L, report_label = "Patients")))
    ),
    report_label = "Either"
  )
  result <- stopTrial(rule, dose = 1, samples = NULL, model = NULL, data = Data(doseGrid = 1:3))
  report <- h_unpack_stopit(result, rule)
  expected <- setNames(
    c("StoppingAny", "StoppingMinPatients", "StoppingAll", "StoppingMinPatients"),
    names(report)
  )
  expect_identical(attr(report, "stopping_classes"), expected)
  bound <- h_bind_stop_reports(list(report, report))
  expect_identical(attr(bound, "stopping_classes"), expected)
  expect_identical(colnames(bound), names(expected))
  expect_identical(unname(bound[1L, ]), as.vector(report))
})

test_that("model-based simulations retain truth and stopping rule metadata", {
  design <- h_get_design_data()
  design@stopping <- StoppingMinPatients(3L, report_label = "Maximum patients")
  truth <- function(dose) plogis(dose - 3)
  sims <- simulate(
    design,
    truth = truth,
    nsim = 1L,
    seed = 819,
    mcmcOptions = h_get_mcmc_options(samples = 5),
    parallel = FALSE
  )
  expect_identical(sims@truth, truth)
  expect_identical(
    attr(sims@stop_report, "stopping_classes"),
    c(`Maximum patients` = "StoppingMinPatients")
  )
  expect_equal(summary(sims), summary(sims, truth = truth))
})

test_that("stored truth is validated", {
  expect_error(GeneralSimulations(list(), numeric(), 1L, truth = 42), "truth must be")
  expect_error(GeneralSimulations(list(), numeric(), 1L, truth = list(plogis)), "truth must be")
})

test_that("get_simulation_truth is a public accessor for simulation containers", {
  expect_identical(getExportedValue("crmPack", "get_simulation_truth"), get_simulation_truth)
  sims <- GeneralSimulations(list(), numeric(), 1L, truth = plogis)
  expect_identical(get_simulation_truth(sims), plogis)
  combo <- .ComboSimulations(truth = plogis)
  expect_identical(get_simulation_truth(combo), plogis)
  truths <- list(arm1 = plogis, combo = pnorm)
  hierarchical <- .HierarchicalSimulations(truth = truths)
  expect_identical(get_simulation_truth(hierarchical), truths)
  expect_error(get_simulation_truth(.GeneralSimulations()), "No truth is stored")
  legacy <- readRDS(test_path("fixtures", "default_simulations.Rds"))
  expect_error(get_simulation_truth(legacy), "No truth is stored")
  expect_error(get_simulation_truth(list(truth = plogis)), "Must inherit")
})

test_that("stopping child extraction handles leaves, composites and wrappers", {
  leaf <- StoppingMinPatients(3L)
  composite <- StoppingAny(list(leaf, StoppingMinCohorts(2L)))
  expect_identical(h_stop_rule_children(NULL), list())
  expect_identical(h_stop_rule_children(leaf), list())
  expect_identical(h_stop_rule_children(composite), composite@stop_list)
  expect_identical(
    h_stop_rule_children(StoppingOrdinal(1L, composite)),
    composite@stop_list
  )
})

test_that("stopping entries retain outcomes, duplicate labels and missing classes", {
  tree <- structure(
    TRUE,
    report_label = "Repeated",
    individual = list(structure(FALSE, report_label = "Repeated"))
  )
  leaf <- StoppingMinPatients(3L)
  composite <- StoppingAny(list(leaf))
  expected <- data.frame(
    stopped = c(TRUE, FALSE),
    label = c("Repeated", "Repeated"),
    rule_class = c("StoppingAny", "StoppingMinPatients")
  )
  expect_identical(h_stop_report_entries(tree, composite), expected)
  expected$rule_class <- c("StoppingOrdinal", "StoppingMinPatients")
  expect_identical(h_stop_report_entries(tree, StoppingOrdinal(1L, composite)), expected)
  expected$rule_class <- rep(NA_character_, 2L)
  expect_identical(h_stop_report_entries(tree), expected)
  expect_identical(h_unpack_stopit(tree), c(Repeated = TRUE, Repeated = FALSE))

  # Custom result trees may expose children without corresponding child rules.
  expected$rule_class <- c("StoppingMinPatients", NA_character_)
  expect_identical(h_stop_report_entries(tree, leaf), expected)
  expect_identical(h_stop_report_entries(TRUE)$label, NA_character_)
})


test_that("tidy GeneralSimulations omits truth metadata consistently", {
  sims <- .DefaultGeneralSimulations()
  expected <- tidy(sims)
  expect_false("truth" %in% names(expected))
  expect_true(all(vapply(expected, function(x) length(x) > 0L, logical(1L))))

  sims@truth <- plogis
  expect_identical(tidy(sims), expected)

  # Older saved objects may lack the truth slot altogether.
  attr(sims, "truth") <- NULL
  expect_identical(tidy(sims), expected)
})
