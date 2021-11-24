# validate_subjects ----

test_that("validate_subjects returns TRUE for valid object", {
  object <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  expect_true(validate_subjects(object))
})

test_that("validate_subjects returns error message for non-valid object", {
  object <- expect_silent(.GeneralData(ID = 1:3, cohort = 3:5, nObs = 3L))
  object@nObs <- 4L
  expect_equal(
    validate_subjects(object),
    c(
      "ID must be of type integer and length nObs",
      "cohort must be of type integer and length nObs"
    )
  )
})

# validate_data ----

test_that("validate_data returns TRUE for valid object", {
  object <- h_get_data()
  expect_true(validate_data(object))
})

test_that(
  paste(
    c(
      "validate_data returns error message for non-valid object",
      "(only placebo in cohort 3)"
    ),
    collapse = " "
  ),
  {
    object <- h_get_data()
    object@x[which(object@cohort == 3L)] <- object@doseGrid[1] # placebo

    expect_equal(
      validate_data(object),
      c(
        "x must be equal to doseGrid[xLevel] (tolerance 1e-10)",
        "A cohort with only placebo is not allowed"
      )
    )
  }
)

test_that(
  paste(
    c(
      "validate_data returns error message for non-valid object",
      "(multiple doses in cohort 1)"
    ),
    collapse = " "
  ),
  {
    object <- h_get_data()
    cohort_1 <- which(object@cohort == 1L)
    object@x[cohort_1] <- sample(x = object@doseGrid[-1], size = length(cohort_1))

    expect_equal(
      validate_data(object),
      c(
        "x must be equal to doseGrid[xLevel] (tolerance 1e-10)",
        "There must be only one dose level, other than placebo, per cohort"
      )
    )
  }
)

test_that(
  paste(
    c(
      "validate_data returns error message for non-valid object",
      "(wrong first xLevel)"
    ),
    collapse = " "
  ),
  {
    object <- h_get_data()
    object@xLevel[1] <- 2L

    expect_equal(
      validate_data(object),
      c("x must be equal to doseGrid[xLevel] (tolerance 1e-10)")
    )
  }
)
