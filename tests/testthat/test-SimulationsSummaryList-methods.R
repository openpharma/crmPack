test_that("faceted selection plots include unused doses and absent recommendations", {
  sims <- oc_test_sims()
  other <- sims
  other@doses <- c(4, 4, 4)
  x <- SimulationsSummaryList(`Z first` = sims, `A second` = other)
  p <- plot(
    x,
    type = "doseSelected",
    scenario_labels = c(`Z first` = "First scenario")
  )
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$scenario), c("Z first", "A second"))
  expect_equal(levels(p$data$dose_label), c("1", "2", "4", "None"))
  expect_equal(p$data$value, c(100 / 3, 100 / 3, 0, 100 / 3, 0, 0, 100, 0))
  built <- ggplot_build(p)
  expect_equal(
    as.character(built$layout$layout$scenario),
    c("Z first", "A second")
  )
  expect_equal(built$data[[1]]$y, p$data$value)
  expect_equal(length(unique(built$data[[1]]$PANEL)), 2L)
  vdiffr::expect_doppelganger("scenario-faceted-dose-selection", p)
})

test_that("faceted patient plots use trial means, retaining placebo and zeros", {
  sims <- oc_test_sims()
  x <- SimulationsSummaryList(A = sims, B = sims)
  p <- plot(x, type = "nPatients", ncol = 1, axis_text_angle = 0)
  expect_equal(p$data$value, rep(c(2 / 3, 1, 0), 2))
  expect_equal(levels(p$data$dose_label), c("1", "2", "4"))
  expect_equal(p$labels$y, "Mean number of patients")
  expect_equal(ggplot_build(p)$layout$layout$ROW, c(1L, 2L))
  # Plotting reads accessors, without recomputing cached model summaries.
  x@simulations$A@fit <- list()
  expect_s3_class(plot(x, type = "nPatients"), "ggplot")
  vdiffr::expect_doppelganger("scenario-faceted-patient-counts", p)
})

test_that("facets handle scenario-specific grids with shared y scales", {
  sims <- oc_test_sims()
  general <- GeneralSimulations(
    data = list(
      Data(x = 8, y = 0, doseGrid = c(2, 8)),
      Data(doseGrid = c(2, 8))
    ),
    doses = c(8, NA_real_),
    seed = 1L,
    truth = function(dose) dose / 10
  )
  x <- SimulationsSummaryList(A = sims, B = general)
  p <- plot(x, scales = "free_x", scenario_labels = c(A = "Same", B = "Same"))
  built <- ggplot_build(p)
  expect_equal(length(built$layout$panel_scales_x[[1]]$get_breaks()), 4L)
  expect_equal(length(built$layout$panel_scales_x[[2]]$get_breaks()), 3L)
  expect_equal(built$layout$layout$SCALE_Y, c(1L, 1L))
  expect_equal(length(unique(p$data$scenario)), 2L)
  expect_equal(p$data$value[p$data$scenario == "B"], c(0, 50, 50))
  expect_error(plot(x, type = "unsupported"))
  expect_error(plot(x, ncol = 0))
  expect_error(
    plot(x, scenario_labels = c(unknown = "Unknown")),
    "Unknown display override"
  )
})
