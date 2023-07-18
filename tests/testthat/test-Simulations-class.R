test_that("GeneralSimulations generator function works as expected", {
  result <- expect_silent(.GeneralSimulations())
  expect_valid(result, "GeneralSimulations")
})

test_that("GeneralSimulations object can be created with the user constructor", {
  data <- list(
    Data(
      x = 1:2,
      y = 0:1,
      doseGrid = 1:2,
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    Data(
      x = 3:4,
      y = 0:1,
      doseGrid = 3:4,
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )

  doses <- c(1, 2)

  seed <- as.integer(123)

  result <- expect_silent(
    GeneralSimulations(
      data,
      doses,
      seed
    )
  )

  expect_valid(result, "GeneralSimulations")
  expect_identical(result@data, data)
  expect_identical(result@doses, doses)
  expect_identical(result@seed, seed)
})

test_that("GeneralSimulations user constructor arguments names are as expected", {
  expect_function(
    GeneralSimulations,
    args = c("data", "doses", "seed"),
    ordered = TRUE
  )
})
# Simulations-class ----
test_that("Simulations generator function works as expected", {
  result <- expect_silent(.Simulations())
  expect_valid(result, "Simulations")
})

test_that("Simulations object can be created with the user constructor", {
  fit <- list(
    c(0.1, 0.2),
    c(0.3, 0.4)
  )
  stop_reasons <- list("A", "B")

  stop_reasons <- matrix(c(TRUE, FALSE), nrow = 2)

  data <- list(
    Data(
      x = 1:2,
      y = 0:1,
      doseGrid = 1:2,
      ID = 1L:2L,
      cohort = 1L:2L
    ),
    Data(
      x = 3:4,
      y = 0:1,
      doseGrid = 3:4,
      ID = 1L:2L,
      cohort = 1L:2L
    )
  )

  doses <- c(1, 2)

  seed <- as.integer(123)

  result <- expect_silent(
    Simulations(
      fit = fit,
      stop_reasons = stop_reasons,
      stop_report = stop_report,
      data,
      doses,
      seed
    )
  )

  expect_valid(result, "Simulations")
  expect_identical(result@fit, fit)
  expect_identical(result@stop_reasons, stop_reasons)
})

test_that("Simulations user constructor arguments names are as expected", {
  expect_function(
    Simulations,
    args = c("fit", "stop_reasons", "stop_report", "..."),
    ordered = TRUE
  )
})
