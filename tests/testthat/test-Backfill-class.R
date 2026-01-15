# Opening ----

## OpeningMinDose ----

test_that(".OpeningMinDose works as expected", {
  result <- expect_silent(.OpeningMinDose())
  expect_valid(result, "OpeningMinDose")
})

test_that("OpeningMinDose object can be created with user constructor", {
  result <- expect_silent(OpeningMinDose(min_dose = 10))
  expect_valid(result, "OpeningMinDose")
  expect_identical(result@min_dose, 10)
})

test_that("OpeningMinDose object can be created with default min_dose", {
  result <- expect_silent(OpeningMinDose())
  expect_valid(result, "OpeningMinDose")
  expect_identical(result@min_dose, 0)
})

test_that(".DefaultOpeningMinDose works as expected", {
  expect_equal(
    .DefaultOpeningMinDose(),
    OpeningMinDose(min_dose = 0)
  )
})

## OpeningMinCohorts ----

test_that(".OpeningMinCohorts works as expected", {
  result <- expect_silent(.OpeningMinCohorts())
  expect_valid(result, "OpeningMinCohorts")
})

test_that("OpeningMinCohorts object can be created with user constructor", {
  result <- expect_silent(OpeningMinCohorts(min_cohorts = 3))
  expect_valid(result, "OpeningMinCohorts")
  expect_identical(result@min_cohorts, 3L)
})

test_that("OpeningMinCohorts object can be created with default min_cohorts", {
  result <- expect_silent(OpeningMinCohorts())
  expect_valid(result, "OpeningMinCohorts")
  expect_identical(result@min_cohorts, 2L)
})

test_that(".DefaultOpeningMinCohorts works as expected", {
  expect_equal(
    .DefaultOpeningMinCohorts(),
    OpeningMinCohorts(min_cohorts = 2L)
  )
})

## OpeningNone ----

test_that(".OpeningNone works as expected", {
  result <- expect_silent(.OpeningNone())
  expect_valid(result, "OpeningNone")
})

test_that("OpeningNone object can be created with user constructor", {
  result <- expect_silent(OpeningNone())
  expect_valid(result, "OpeningNone")
})

test_that(".DefaultOpeningNone works as expected", {
  expect_equal(
    .DefaultOpeningNone(),
    OpeningNone()
  )
})

## OpeningMinResponses ----

test_that(".OpeningMinResponses works as expected", {
  result <- expect_silent(.OpeningMinResponses())
  expect_valid(result, "OpeningMinResponses")
})

test_that("OpeningMinResponses object can be created with user constructor", {
  result <- expect_silent(OpeningMinResponses(
    min_responses = 3,
    include_lower_doses = TRUE
  ))
  expect_valid(result, "OpeningMinResponses")
  expect_identical(result@min_responses, 3L)
  expect_identical(result@include_lower_doses, TRUE)
})

test_that("OpeningMinResponses object can be created with default parameters", {
  result <- expect_silent(OpeningMinResponses())
  expect_valid(result, "OpeningMinResponses")
  expect_identical(result@min_responses, 1L)
  expect_identical(result@include_lower_doses, FALSE)
})

test_that(".DefaultOpeningMinResponses works as expected", {
  expect_equal(
    .DefaultOpeningMinResponses(),
    OpeningMinResponses(min_responses = 1L, include_lower_doses = FALSE)
  )
})

## OpeningAll ----

test_that("OpeningAll can be created with constructor", {
  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 3)
  result <- expect_silent(OpeningAll(opening1, opening2))
  expect_valid(result, "OpeningAll")
  expect_length(result@open_list, 2)
})

test_that("OpeningAll can be created with & operator", {
  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 3)
  result <- expect_silent(opening1 & opening2)
  expect_valid(result, "OpeningAll")
  expect_length(result@open_list, 2)
})

test_that(".DefaultOpeningAll works as expected", {
  result <- expect_silent(.DefaultOpeningAll())
  expect_valid(result, "OpeningAll")
  expect_length(result@open_list, 2)
})

## OpeningAny ----

test_that("OpeningAny can be created with constructor", {
  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 3)
  result <- expect_silent(OpeningAny(opening1, opening2))
  expect_valid(result, "OpeningAny")
  expect_length(result@open_list, 2)
})

test_that("OpeningAny can be created with | operator", {
  opening1 <- OpeningMinDose(min_dose = 10)
  opening2 <- OpeningMinCohorts(min_cohorts = 3)
  result <- expect_silent(opening1 | opening2)
  expect_valid(result, "OpeningAny")
  expect_length(result@open_list, 2)
})

test_that(".DefaultOpeningAny works as expected", {
  result <- expect_silent(.DefaultOpeningAny())
  expect_valid(result, "OpeningAny")
  expect_length(result@open_list, 2)
})

# Recruitment ----

## RecruitmentUnlimited ----

test_that(".RecruitmentUnlimited works as expected", {
  result <- expect_silent(.RecruitmentUnlimited())
  expect_valid(result, "RecruitmentUnlimited")
})

test_that("RecruitmentUnlimited object can be created", {
  result <- expect_silent(RecruitmentUnlimited())
  expect_valid(result, "RecruitmentUnlimited")
})

test_that(".DefaultRecruitmentUnlimited works as expected", {
  result <- expect_silent(.DefaultRecruitmentUnlimited())
  expect_valid(result, "RecruitmentUnlimited")
})

## RecruitmentRatio ----

test_that(".RecruitmentRatio works as expected", {
  result <- expect_silent(.RecruitmentRatio())
  expect_valid(result, "RecruitmentRatio")
})

test_that("RecruitmentRatio object can be created with user constructor", {
  result <- expect_silent(RecruitmentRatio(ratio = 0.5))
  expect_valid(result, "RecruitmentRatio")
  expect_identical(result@ratio, 0.5)
})

test_that("RecruitmentRatio object can be created with default ratio", {
  result <- expect_silent(RecruitmentRatio())
  expect_valid(result, "RecruitmentRatio")
  expect_identical(result@ratio, 1)
})

test_that(".DefaultRecruitmentRatio works as expected", {
  result <- expect_silent(.DefaultRecruitmentRatio())
  expect_valid(result, "RecruitmentRatio")
  expect_identical(result@ratio, 1)
})

# Backfill ----

test_that(".Backfill works as expected", {
  result <- expect_silent(.Backfill())
  expect_valid(result, "Backfill")
})

test_that("Backfill object can be created with default parameters", {
  result <- expect_silent(Backfill())
  expect_valid(result, "Backfill")
  expect_identical(result@total_size, 1e6L)
  expect_identical(result@priority, "highest")
  expect_is(result@opening, "OpeningMinDose")
  expect_is(result@recruitment, "RecruitmentUnlimited")
})

test_that("Backfill object can be created with custom opening and recruitment", {
  opening <- OpeningMinCohorts(min_cohorts = 3)
  recruitment <- RecruitmentRatio(ratio = 0.5)
  result <- expect_silent(Backfill(
    opening = opening,
    recruitment = recruitment
  ))
  expect_valid(result, "Backfill")
  expect_identical(result@opening, opening)
  expect_identical(result@recruitment, recruitment)
})

test_that("Backfill object can be created with custom total_size", {
  result <- expect_silent(Backfill(total_size = 100L))
  expect_valid(result, "Backfill")
  expect_identical(result@total_size, 100L)
})

test_that("Backfill object can be created with different priorities", {
  result_high <- expect_silent(Backfill(priority = "highest"))
  expect_identical(result_high@priority, "highest")

  result_low <- expect_silent(Backfill(priority = "lowest"))
  expect_identical(result_low@priority, "lowest")

  result_rand <- expect_silent(Backfill(priority = "random"))
  expect_identical(result_rand@priority, "random")
})

test_that(".DefaultBackfill works as expected", {
  result <- expect_silent(.DefaultBackfill())
  expect_valid(result, "Backfill")
  expect_identical(result@total_size, 1e6L)
  expect_identical(result@priority, "highest")
})
