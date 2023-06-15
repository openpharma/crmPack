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
