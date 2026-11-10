#' Deterministic Trials for Reporting Tests
#' @return A simulation object including an empty trial.
#' @keywords internal
report_test_sims <- function() {
  grid <- c(1, 2, 4)
  GeneralSimulations(
    data = list(
      Data(x = c(1, 1, 2), y = c(0, 1, 1), doseGrid = grid),
      Data(x = 2, y = 0, doseGrid = grid),
      Data(doseGrid = grid)
    ),
    doses = c(1, 2, NA_real_),
    seed = 1L,
    truth = function(dose) dose / 10
  )
}

test_that("dose selection includes unused doses and absent recommendations", {
  sims <- report_test_sims()
  result <- report_dose_selection(sims)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$dose, c(1, 2, 4, NA_real_))
  expect_equal(result$n, c(1L, 1L, 0L, 1L))
  expect_equal(result$percent, c(100 / 3, 100 / 3, 0, 100 / 3))
  sims@doses[1] <- 1 + 1e-12
  expect_equal(report_dose_selection(sims), result)
  sims@doses[1] <- 3
  expect_error(report_dose_selection(sims), "Selected doses")
})

test_that("per-dose counts include zeros from every trial", {
  sims <- report_test_sims()
  patients <- report_patients_per_dose(sims)
  expect_equal(patients$mean, c(2 / 3, 2 / 3, 0))
  expect_equal(patients$sd, c(sd(c(2, 0, 0)), sd(c(1, 1, 0)), 0))
  expect_equal(patients$min, c(0, 0, 0))
  expect_equal(patients$max, c(2, 1, 0))
  dlts <- report_dlts_per_dose(sims)
  expect_equal(dlts$mean, c(1 / 3, 1 / 3, 0))
  expect_equal(dlts$sd, c(sd(c(1, 0, 0)), sd(c(1, 0, 0)), 0))
  expect_equal(report_sample_size(sims)$mean, sum(patients$mean))
  sims@data <- sims@data[1]
  sims@doses <- 1
  expect_true(all(is.na(report_patients_per_dose(sims)$sd)))
  one <- GeneralSimulations(
    list(Data(doseGrid = 1)),
    doses = NA_real_,
    seed = 1L
  )
  expect_equal(report_patients_per_dose(one)$mean, 0)
})

test_that("sample size uses trial totals and configurable quantiles", {
  sims <- report_test_sims()
  result <- report_sample_size(sims, quantiles = c(0, 0.5, 1))
  expect_equal(
    unlist(result),
    c(
      mean = 4 / 3,
      sd = sd(c(3, 1, 0)),
      min = 0,
      max = 3,
      q0 = 0,
      q50 = 1,
      q100 = 3
    )
  )
  expect_named(
    report_sample_size(sims, numeric()),
    c("mean", "sd", "min", "max")
  )
  expect_error(report_sample_size(sims, c(-1, 1)))
})

test_that("MTD classification uses stored truth and inclusive target bounds", {
  sims <- report_test_sims()
  result <- report_mtd_classification(sims, c(0.1, 0.2))
  expect_equal(result$n, c(2L, 0L, 0L, 1L))
  sims@doses <- c(1, 2, 4)
  expect_equal(
    report_mtd_classification(sims, c(0.15, 0.3))$n,
    c(1L, 1L, 1L, 0L)
  )
  sims@truth <- NULL
  expect_error(report_mtd_classification(sims, c(0.1, 0.2)), "No truth")
  sims@truth <- function(dose) stop("No selected doses")
  sims@doses[] <- NA_real_
  expect_equal(
    report_mtd_classification(sims, c(0.1, 0.2))$n,
    c(0L, 0L, 0L, 3L)
  )
  expect_error(report_mtd_classification(sims, c(0.2, 0.1)), "increasing")
})

test_that("stop reporting counts overlapping rules by union within categories", {
  sims <- .DefaultSimulations()
  sims@stop_report <- cbind(
    unsafe = c(TRUE, TRUE, FALSE),
    toxic = c(TRUE, FALSE, FALSE),
    max = c(FALSE, TRUE, TRUE)
  )
  attr(sims@stop_report, "stopping_classes") <- c(
    unsafe = "StoppingLowestDoseHSRBeta",
    toxic = "StoppingTDCIRatio",
    max = "StoppingMaxPatients"
  )
  result <- report_stop_reasons(sims, list())
  summary <- .SimulationsSummary()
  summary@stop_report <- sims@stop_report
  expect_equal(report_stop_reasons(summary, list()), result)
  expect_equal(result$label, c("unsafe", "toxic", "max"))
  expect_equal(
    result$rule_class,
    unname(attr(sims@stop_report, "stopping_classes"))
  )
  expect_equal(result$percent, c(2, 1, 2) / 3 * 100)
  grouped <- report_stop_reasons(sims, list(Toxicity = c("unsafe", "toxic")))
  expect_equal(grouped$label, c("Toxicity", "max"))
  expect_equal(grouped$n, c(2, 2))
  expect_error(report_stop_reasons(sims, list(Unknown = "missing")), "Unknown")
  expect_error(report_stop_reasons(report_test_sims()), "stop_report")
})

test_that("reporting rejects empty trials lists and inconsistent dose grids", {
  sims <- report_test_sims()
  sims@data[[2]]@doseGrid <- c(1, 2, 3)
  expect_error(report_patients_per_dose(sims), "same dose grid")
  sims@data <- list()
  sims@doses <- numeric()
  expect_error(report_dose_selection(sims), "at least one")
})

test_that("placebo counts remain available in per-dose and total summaries", {
  data <- Data(
    x = c(0.001, 1, 1),
    y = c(0, 0, 1),
    doseGrid = c(0.001, 1, 2),
    placebo = TRUE,
    ID = 1:3,
    cohort = rep(1L, 3)
  )
  sims <- GeneralSimulations(list(data), doses = 1, seed = 1L)
  expect_equal(report_patients_per_dose(sims)$mean, c(1, 2, 0))
  expect_equal(report_dlts_per_dose(sims)$mean, c(0, 1, 0))
  expect_equal(report_sample_size(sims)$mean, 3)
})

test_that("stop reports without labels or class metadata remain supported", {
  sims <- .DefaultSimulations()
  sims@stop_report <- matrix(
    c(TRUE, FALSE, TRUE, TRUE),
    ncol = 2,
    dimnames = list(NULL, c("one", NA_character_))
  )
  result <- report_stop_reasons(sims)
  expect_equal(result$label, "one")
  expect_equal(result$n, 1)
  expect_true(is.na(result$rule_class))
  colnames(sims@stop_report) <- NULL
  expect_equal(nrow(report_stop_reasons(sims)), 0L)
  expect_named(
    report_stop_reasons(sims),
    c("label", "rule_class", "n", "percent")
  )
  sims@stop_report <- matrix(
    c(TRUE, NA),
    ncol = 1,
    dimnames = list(NULL, "one")
  )
  expect_true(is.na(report_stop_reasons(sims)$percent))
})
