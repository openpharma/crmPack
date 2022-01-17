# h_jags_join_models ----

test_that("h_jags_join_models works as expected", {
  model1 <- function(x) {
    x <- x - 2
    x <- x^2
  }
  model2 <- function(x) {
    x^3
  }
  result <- h_jags_join_models(model1, model2)
  expected <- function(x) {
    x <- x - 2
    x <- x^2
    x^3
  }

  expect_identical(result, expected)
})

test_that("h_jags_join_models works as expected for empty model2", {
  model1 <- function(x) {
    x - 2
  } # nolintr
  model2 <- function(x) { } # nolintr
  result <- h_jags_join_models(model1, model2)
  expected <- model1

  expect_identical(result, expected)
})

test_that("h_jags_join_models throws the error for non-braced expression", {
  model1 <- function(x) x^2
  model2 <- function(x) x^3

  expect_error(
    h_jags_join_models(model1, model2),
    "Assertion on 'body\\(model1\\)' failed: Must inherit from class '\\{', but has class 'call'." # nolintr
  )
})

# h_jags_write_model ----

test_that("h_jags_write_model works as expected", {
  my_model <- function() {
    alpha0 <- mean(1:10)
    alpha1 <- 600000
  }

  model_file <- h_jags_write_model(my_model, digits = 5)
  expect_snapshot(readLines(model_file))
  unlink(model_file)
})

test_that("h_jags_write_model works as expected for truncation", {
  my_model <- function() {
    alpha0 <- dnorm(4) %_% I(4)
    alpha1 <- 600000
  }

  model_file <- tempfile("crmPack-testthat-h_jags_write_model-trunc.jags")
  h_jags_write_model(my_model, model_file, 5)
  expect_snapshot(readLines(model_file))
  unlink(model_file)
})
