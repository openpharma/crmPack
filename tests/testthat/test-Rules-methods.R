# maxDose-IncrementsNumDoseLevels ----

# Sample data to test maxDose of IncrementsNumDoseLevels method.
my_data <- Data(x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 12, 12, 12, 16, 16, 16, 10, 10, 10 ),
                y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0),
                cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
                doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from=10, to=80, by=2))
                )

test_that("IncrementsNumDoseLevels works correctly if basislevel 'lastGiven' is defined", {
  Increments <- IncrementsNumDoseLevels(
    maxLevels = 2, 
    basisLevel = "lastGiven"
    )
  result <- maxDose(
    Increments,
    data=my_data
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel='lastGiven'.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel is not defined and default is used", {
  Increments <- IncrementsNumDoseLevels(
    maxLevels = 2
  )
  result <- maxDose(
    Increments,
    data=my_data
  )
  expect_equal(result, 14) # maxDose is 14 if basislevel not defined, then reference value is used.
})

test_that("IncrementsNumDoseLevels works correctly if basislevel 'maxGiven' is defined", {
  Increments <- IncrementsNumDoseLevels(
    maxLevels = 2, 
    basisLevel = "maxGiven"
  )
  result <- maxDose(
    Increments,
    data=my_data
  )
  expect_equal(result, 20) # maxDose is 20 if basislevel='maxGiven'.
})