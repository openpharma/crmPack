test_that("DADesign examination counts eligible patients and follows the latest cohort", {
  design <- h_get_design_da()
  design@data <- DataDA(doseGrid = 1:4, Tmax = 42)
  design@startingDose <- 1
  design@cohort_size <- CohortSizeConst(3)
  design@safetyWindow <- SafetyWindowConst(c(7, 7), 7, 7)
  design@stopping <- StoppingMinPatients(12)
  evaluated <- list()

  local_mocked_bindings(
    mcmc = function(data, ...) {
      evaluated[[length(evaluated) + 1L]] <<- data
      NULL
    },
    nextBest = function(data, ...) {
      # Escalate, then de-escalate and stay: old recommendations must not win.
      if (data@nObs > 12) stop("Examination did not stop at 12 patients")
      list(value = if (data@nObs == 3) 2 else 1)
    }
  )

  expect_no_warning(result <- examine(design))
  expect_type(result$cohort, "integer")
  expect_type(result$DLT_cohorts, "character")
  expect_equal(result$cohort[result$DLTs == 0], 1:4)
  expect_equal(result$DLT_cohorts, vapply(evaluated, function(data) {
    paste(sort(data@cohort[data@y == 1]), collapse = ", ")
  }, character(1)))
  second <- subset(result, cohort == 2 & DLTs == 3)
  expect_equal(second$DLT_cohorts, c("1, 1, 2", "2, 2, 2"))
  expect_equal(result$dose[result$DLTs == 0], c(1, 2, 1, 1))
  expect_equal(tail(result$stop[result$DLTs == 0], 1), TRUE)
  expect_equal(result$DLTs, vapply(evaluated, function(data) sum(data@y), numeric(1)))
  expect_type(result$DLT_scenario, "character")
  expect_setequal(result$DLT_scenario, c("no additional DLTs", "late DLTs", "early DLTs"))
  expect_equal(result$DLT_scenario == "no additional DLTs", result$DLTs == 0)
  # At day 42 the first cohort has patients completing their DLT windows.
  # Counts must use eligibility at this decision, not the previous one.
  expect_equal(max(result$DLTs[vapply(evaluated, function(data) data@nObs == 6, logical(1))]), 5)
  expect_equal(sum(result$DLTs == 1 & result$DLT_scenario == "late DLTs"), 4)
  expect_equal(sum(result$DLTs == 1 & result$DLT_scenario == "early DLTs"), 4)
  expect_true(all(vapply(evaluated, function(data) isTRUE(validObject(data)), logical(1))))
})

test_that("DADesign examination initializes the no-increment counter", {
  design <- h_get_design_da()
  design@stopping <- StoppingMinPatients(100)
  local_mocked_bindings(
    mcmc = function(...) NULL,
    nextBest = function(data, ...) list(value = tail(data@x, 1))
  )
  expect_warning(result <- examine(design, maxNoIncrement = 1L), "Stopping because 1 times")
  expect_equal(sum(result$DLTs == 0), 1)
})


test_that("DADesign examination preserves existing cohort indices and excludes observed DLTs", {
  design <- h_get_design_da()
  design@data <- DataDA(
    doseGrid = c(1, 3, 6, 10), Tmax = 60,
    x = c(1, 3, 3), y = c(1, 0, 0),
    cohort = c(1L, 2L, 2L), t0 = c(0, 0, 0), u = c(1, 0, 0)
  )
  design@cohort_size <- CohortSizeConst(1)
  design@stopping <- StoppingMinPatients(4)
  evaluated <- list()
  local_mocked_bindings(
    mcmc = function(data, ...) {
      evaluated[[length(evaluated) + 1L]] <<- data
      NULL
    },
    nextBest = function(...) list(value = 6)
  )
  expect_no_warning(result <- examine(design))
  expect_true(all(result$cohort == 3L))
  expect_equal(result$DLT_cohorts, c("", "2", "3", "2, 2", "2, 3", "2, 2, 3", "2, 2, 3"))
  expect_equal(result$DLTs, vapply(evaluated, function(data) sum(data@y) - 1, numeric(1)))
})
