# GeneralData-class ----

test_that(".GeneralData works as expected", {
  result <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  expect_valid(result, "GeneralData")
})

# Data-class ----

test_that(".Data works as expected", {
  result <- expect_silent(.Data())
  expect_valid(result, "Data")
})

# Data-constructor ----

test_that("Data object can be created with user constructor Data", {
  result <- expect_silent(Data())
  expect_valid(result, "Data")
})

test_that("Data object can be created with custom values", {
  plcb <- 0.01
  result <- expect_silent(
    Data(
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = TRUE,
    )
  )
  expect_valid(result, "Data")
})

test_that("Data constructor handles default IDs as expected", {
  plcb <- 0.05
  expect_message(
    result <-
      Data(
        x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
        y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
        cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
        doseGrid = c(plcb, seq(25, 300, 25)),
        placebo = TRUE,
      ),
    regexp = "Used default patient IDs!"
  )
  expect_valid(result, "Data")
})

test_that("Data constructor works as expected without cohort and no placebo", {
  plcb <- 0.001
  expect_message(
    result <-
      Data(
        x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
        y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
        ID = 1:12,
        doseGrid = c(plcb, seq(25, 300, 25)),
        placebo = FALSE,
      ),
    regexp = "Used best guess cohort indices!"
  )
  expect_valid(result, "Data")
})

# DataDual-class ----

test_that(".DataDual works as expected", {
  result <- expect_silent(.DataDual())
  expect_valid(result, "DataDual")
})

# DataDual-constructor ----

test_that("DataDual object can be created with user constructor DataDual", {
  result <- expect_silent(DataDual())
  expect_valid(result, "DataDual")
})

test_that("DataDual object can be created with custom values", {
  plcb <- 0.01
  result <- expect_silent(
    DataDual(
      w = c(13, 77, 86, 26, 27, 36, 37, 97, 21, 49, 87, 48),
      x = c(plcb, 25, 25, 25, plcb, 50, 50, 50, plcb, 100, 100, 100),
      y = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
      ID = 1:12,
      cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      doseGrid = c(plcb, seq(25, 300, 25)),
      placebo = TRUE,
    )
  )
  expect_valid(result, "DataDual")
})

# DataParts-class ----

test_that(".DataParts works as expected", {
  result <- expect_silent(.DataParts())
  expect_valid(result, "DataParts")
})

# DataParts-constructor ----

test_that("DataParts object can be created with user constructor DataParts", {
  result <- expect_silent(DataParts())
  expect_valid(result, "DataParts")
})

test_that("DataParts object can be created with custom values", {
  result <- expect_silent(
    DataParts(
      part = c(1L, 1L, 1L),
      nextPart = 1L,
      part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10),
      x = c(0.1, 0.5, 1.5),
      y = c(0, 0, 0),
      ID = 1:3,
      cohort = 1:3,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  expect_valid(result, "DataParts")
})

# DataMixture-class ----

test_that(".DataMixture works as expected", {
  result <- expect_silent(.DataMixture())
  expect_valid(result, "DataMixture")
})

# DataMixture-constructor ----

test_that("DataMixture object can be created with user constructor", {
  result <- expect_silent(DataMixture())
  expect_valid(result, "DataMixture")
})

test_that("DataMixture object can be created with custom values", {
  result <- expect_silent(
    DataMixture(
      xshare = c(12, 14, 16, 18.0),
      yshare = c(0L, 1L, 1L, 1L),
      nObsshare = 4L,
      x = c(0.1, 0.5, 1.5),
      y = c(0, 0, 0),
      ID = 1:3,
      cohort = 1:3,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  expect_valid(result, "DataMixture")
})

# DataDA-class ----

test_that(".DataDA works as expected", {
  result <- expect_silent(.DataDA())
  expect_valid(result, "DataDA")
})

# DataDA-constructor ----

test_that("DataDA object can be created with user constructor DataDA", {
  result <- expect_silent(DataDA())
  expect_valid(result, "DataDA")
})

test_that("DataDA object can be created with custom values", {
  result <- expect_silent(
    DataDA(
      u = c(42, 30, 15),
      t0 = c(0, 15, 30),
      Tmax = 60,
      x = c(0.1, 0.5, 1.5),
      y = c(0, 0, 0),
      ID = 1:3,
      cohort = 1:3,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  expect_valid(result, "DataDA")
})

# DataGrouped-class ----

test_that(".DataGrouped works as expected", {
  result <- expect_silent(.DataGrouped())
  expect_valid(result, "DataGrouped")
})

# DataGrouped-constructor ----

test_that("DataGrouped object can be created with user constructor DataGrouped", {
  result <- expect_silent(DataGrouped())
  expect_valid(result, "DataGrouped")
})

test_that("DataGrouped object can be created with custom values", {
  result <- expect_silent(
    DataGrouped(
      group = c("mono", "combo", "mono"),
      x = c(0.1, 0.5, 1.5),
      y = c(0, 0, 0),
      ID = 1:3,
      cohort = 1:3,
      doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
    )
  )
  expect_valid(result, "DataGrouped")
})

test_that("DataGrouped default constructor works as expected", {
  result <- expect_silent(.DefaultDataGrouped())
  expect_valid(result, "DataGrouped")
})
