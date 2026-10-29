# NextBestSuggestion-class ----

test_that("NextBestSuggestion works as expected", {
  result <- expect_silent(NextBestSuggestion(value = 44))
  expect_valid(result, "NextBestSuggestion")

  testthat::expect_error(NextBestSuggestion(value = -1), "value must be a positive real or `NA`")
})

# NextBestSuggestionProbs-class ----

test_that("NextBestSuggestionProbs works as expected", {
  result <- expect_silent(
    NextBestSuggestionProbs(
      value = 44,
      probs = list(target = 0.3, overdose = 0.1)
    )
  )
  expect_valid(result, "NextBestSuggestionProbs")

  testthat::expect_error(
    NextBestSuggestionProbs(value = -1),
    "Must be of type 'numeric', not 'NULL'"
  )
  testthat::expect_error(
    NextBestSuggestionProbs(value = 1, probs = list()),
    "Must be of type 'numeric', not 'NULL'"
  )
  testthat::expect_error(
    NextBestSuggestionProbs(value = 1, probs = list(0.3, 0.1)),
    "Assertion on 'probs' failed"
  )
  testthat::expect_error(
    NextBestSuggestionProbs(value = 1, probs = list(a = 4,  b = 0.1)),
    "Probability must be within \\[0, 1\\] bounds but it is not"
  )
  testthat::expect_error(
    NextBestSuggestionProbs(value = 1, probs = list(a = 0.5,  a = 0.1)),
    "element 2 is duplicated"
  )
})

