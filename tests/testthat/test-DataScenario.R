#' Make a Deterministic Reporting Fixture
#' @param data Observations.
#' @return Evaluated scenario fixture.
#' @noRd
local_data_scenario <- function(
  data = Data(
    x = c(1, 2, 2),
    y = c(0, 0, 1),
    cohort = c(1L, 2L, 2L),
    doseGrid = 1:3
  )
) {
  h_data_scenario(
    data = data,
    samples = Samples(
      data = list(alpha = 1:3),
      options = McmcOptions(samples = 3)
    ),
    fit = data.frame(),
    dose_limit = 3,
    next_best = list(
      value = 3,
      probs = cbind(
        dose = 1:3,
        target = c(.1, .2, .3),
        overdose = c(.01, .02, .03)
      )
    ),
    next_dose = 3,
    cohort_size = 3L,
    placebo_cohort_size = NULL,
    stop = FALSE,
    stop_report = c(rule = FALSE),
    stop_reason = NULL
  )
}

test_that("single-agent flattening aggregates counts and preserves numeric doses", {
  x <- local_data_scenario()
  d <- tidy(x)
  expect_s3_class(d, "tbl_DataScenario")
  expect_equal(d$n_patients, c(1L, 2L))
  expect_equal(d$n_dlts, c(0L, 1L))
  expect_equal(d$current_dose, rep(list(c(dose = 2)), 2))
  expect_equal(d$current_overdose_prob, rep(.02, 2))
  expect_equal(d$target, rep(.3, 2))
  expect_error(tidy(x, probabilities = "underdosing"), "Available")
  x@next_best$overall$probs[, "dose"] <- 1:3 + 1e-10
  expect_equal(tidy(x)$overdose, rep(.03, 2))
})

test_that("empty and ambiguous latest cohorts are handled explicitly", {
  d <- tidy(local_data_scenario(Data(doseGrid = 1:3)))
  expect_equal(nrow(d), 1)
  expect_equal(d$n_patients, 0L)
  expect_true(is.na(d$current_dose_display))
  x <- local_data_scenario()
  x@data@cohort <- c(1L, 1L, 1L)
  expect_error(tidy(x), "Ambiguous")
})

test_that("combination doses retain both components and rule aliases", {
  data <- DataCombo(
    x = cbind(A = c(1, 2, 2), B = c(10, 10, 10)),
    y = c(0, 0, 1),
    doseGrid = list(A = 1:3, B = c(10, 20))
  )
  x <- local_data_scenario(data)
  x@next_dose$overall <- c(A = 3, B = 10)
  x@next_best$overall$probs <- data.frame(
    A = 1:3,
    B = 10,
    target_prob = c(.1, .2, .3),
    overdose_prob = c(.01, .02, .03),
    not_eligible = FALSE
  )
  d <- tidy(x)
  expect_equal(d$n_patients, c(1L, 2L))
  expect_equal(d$current_dose[[1]], c(A = 2, B = 10))
  expect_equal(d$observed_dose_display, c("1 + 10", "2 + 10"))
  expect_equal(d$target, rep(.3, 2))
  expect_equal(attr(d, "probabilities"), c("target", "overdose"))
})

test_that("assembly validates input and renders stop actions without changing raw data", {
  x <- local_data_scenario()
  y <- x
  y@stop$overall <- TRUE
  y@stop_reason$overall <- "Safety"
  d <- report_data_scenarios(list(First = x, Second = y))
  expect_s3_class(d, "tbl_DataScenarios")
  expect_equal(d$scenario, rep(c("First", "Second"), each = 2))
  expect_equal(d$target, rep(.3, 4))
  table <- paste(report_table(d), collapse = "\n")
  expect_match(table, "Stop: Safety", fixed = TRUE)
  expect_s3_class(knitr::knit_print(tidy(x)), "knit_asis")
  expect_error(report_data_scenarios(list(x)), "names|names|Names")
  expect_error(report_data_scenarios(list(A = list())), "DataScenario")
  y@next_best$overall$probs <- y@next_best$overall$probs[, c(
    "dose",
    "overdose"
  )]
  expect_false("target" %in% names(report_data_scenarios(list(A = x, B = y))))
  expect_error(
    report_data_scenarios(list(A = x, B = y), probabilities = "target"),
    "Available"
  )
})

test_that("validity rejects inconsistent arm lists and incompatible samples", {
  x <- local_data_scenario()
  names(x@fit) <- "wrong"
  expect_error(validObject(x), "arm names")
})

test_that("hierarchical flattening retains pending and historical arms", {
  mono <- local_data_scenario()
  data <- HierarchicalData(
    active = mono@data,
    pending = Data(doseGrid = 1:3),
    historical = mono@data
  )
  fields <- lapply(
    setdiff(slotNames(mono), c("data", "samples")),
    function(field) {
      stats::setNames(rep(slot(mono, field), 3), names(data@arms))
    }
  )
  names(fields) <- setdiff(slotNames(mono), c("data", "samples"))
  fields$next_best[c("pending", "historical")] <- list(NULL, NULL)
  fields$next_dose[c("pending", "historical")] <- list(NULL, NULL)
  fields$stop$pending <- NA
  fields$stop$historical <- TRUE
  fields$stop_reason$pending <- "Arm is not currently open."
  fields$stop_reason$historical <- "Historical arm: not enrolling."
  samples <- HierarchicalSamples(
    data = list(alpha = 1:3),
    options = McmcOptions(samples = 3),
    arm_samples = list(
      active = c(alpha = "alpha"),
      pending = c(alpha = "alpha"),
      historical = c(alpha = "alpha")
    )
  )
  x <- do.call(.DataScenario, c(list(data = data, samples = samples), fields))
  d <- tidy(x)
  expect_equal(unique(d$arm), names(data@arms))
  expect_equal(d$n_patients[d$arm == "pending"], 0L)
  expect_true(is.na(d$stop[d$arm == "pending"]))
  expect_true(all(d$stop[d$arm == "historical"]))
  expect_equal(d$target[d$arm == "active"], c(.3, .3))
  expect_true(all(is.na(d$target[d$arm != "active"])))
})

test_that("renderers accept labels and preserve raw probabilities", {
  x <- local_data_scenario()
  text <- paste(
    report_data_scenarios(
      x,
      format = "kable",
      probabilities = "overdose",
      probability_labels = c(overdose = "P(ODS)"),
      column_labels = c(n_patients = "N"),
      dose_digits = 1
    ),
    collapse = "\n"
  )
  expect_match(text, "P(ODS)", fixed = TRUE)
  expect_match(text, "N", fixed = TRUE)
  skip_if_not_installed("gt")
  table <- report_data_scenarios(x, format = "gt")
  expect_s3_class(table, "gt_tbl")
  html <- gt::as_raw_html(table)
  expect_match(html, "Observed data", fixed = TRUE)
  expect_match(
    html,
    "Interval probabilities of proposed next dose",
    fixed = TRUE
  )
})

test_that("placebo does not determine current active dose", {
  x <- local_data_scenario(Data(
    x = c(1, 0, 2, 0),
    y = c(0, 0, 1, 0),
    doseGrid = c(0, 1, 2, 3),
    cohort = c(1L, 1L, 2L, 2L),
    placebo = TRUE
  ))
  d <- tidy(x)
  expect_equal(d$current_dose[[1]], c(dose = 2))
  expect_equal(sum(d$n_patients), 4L)
})
