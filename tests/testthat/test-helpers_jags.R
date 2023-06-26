# h_jags_add_dummy ----

test_that("h_jags_add_dummy works as expected for single observation", {
  data <- Data(x = 0.1, y = 0, doseGrid = c(0.1, 0.5), ID = 1, cohort = 1)
  result <- h_jags_add_dummy(data, where = c("x", "y"))
  slot(data, "x") <- c(0.1, 0)
  slot(data, "y") <- c(0L, 0L)

  expect_identical(result, data)
})

test_that("h_jags_add_dummy works as expected for single observation (dummy)", {
  data <- Data(x = 0.1, y = 0, doseGrid = c(0.1, 0.5), ID = 1, cohort = 1)
  result <- h_jags_add_dummy(data, where = "x", dummy = 5)
  slot(data, "x") <- c(0.1, 5)

  expect_identical(result, data)
})

test_that("h_jags_add_dummy works as expected for non-single observation", {
  data <- Data(
    x = c(0.1, 0.5),
    y = c(0, 1),
    doseGrid = c(0.1, 0.5),
    ID = 1:2,
    cohort = 1:2
  )

  result <- h_jags_add_dummy(data, where = c("x", "y"))
  expect_identical(result, data)
})

test_that("h_jags_add_dummy throws the error for wrong slot name", {
  data <- Data(x = 0.1, y = 0, doseGrid = c(0.1, 0.5), ID = 1, cohort = 1)
  expect_error(
    h_jags_add_dummy(data, where = "wrong_slot_name"),
    "Assertion on 'where' failed: Must be a subset of.*"
  )
})

# h_jags_get_model_inits ----

test_that("h_jags_get_model_inits works as expected when no params to init", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()

  result <- h_jags_get_model_inits(model = model, data = data)
  expected <- list(theta = c(0, 1))
  expect_identical(result, expected)
})

test_that("h_jags_get_model_inits works as expected with params to init", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@init <- function(x, y) {
    list(p1 = x^2, p2 = x * y, p3 = c(0, 1))
  }

  result <- h_jags_get_model_inits(model = model, data = data)
  expected <- list(p1 = data@x^2, p2 = data@x * data@y, p3 = c(0, 1))
  expect_identical(result, expected)
})

test_that("h_jags_get_model_inits throws the error when init does not return list", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@init <- function(x, y) {
    c(p1 = 3)
  }
  expect_error(
    h_jags_get_model_inits(model = model, data = data),
    "Assertion on 'inits' failed: Must be of type 'list', not 'double'."
  )
})

test_that("h_jags_get_model_inits truncates zero-length elements", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@init <- function(x, y) {
    list(p1 = 3, p2 = "", p3 = numeric(0))
  }

  result <- h_jags_get_model_inits(model = model, data = data)
  expected <- list(p1 = 3, p2 = "")
  expect_identical(result, expected)
})

# h_jags_get_data ----

test_that("h_jags_get_data works as expected", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()

  result <- h_jags_get_data(model, data, from_prior = FALSE)
  expected_prior <- c(
    h_slots(data, c("nObs", "y", "x")),
    model@modelspecs(from_prior = TRUE) # nolintr
  )
  expected <- c(
    h_slots(data, c("nObs", "y", "x")),
    model@modelspecs(from_prior = FALSE) # nolintr
  )
  expect_identical(result[setdiff(names(result), "ref_dose")], expected_prior)
  expect_identical(result, expected)
})

test_that("h_jags_get_data works as expected (datanames and datanames_prior redundant)", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@datanames_prior <- "nObs"

  result <- h_jags_get_data(model, data, from_prior = FALSE)
  expected <- c(
    h_slots(data, c("nObs", "y", "x")),
    model@modelspecs(from_prior = FALSE) # nolintr
  )
  expect_identical(result, expected)
})

test_that("h_jags_get_data works as expected (from prior)", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()

  result <- h_jags_get_data(model, data, from_prior = TRUE)
  expected <- model@modelspecs(from_prior = TRUE) # nolintr
  expect_identical(result, expected)
})

test_that("h_jags_get_data works as expected (from prior and datanames_prior)", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@datanames_prior <- "nGrid"

  result <- h_jags_get_data(model, data, from_prior = TRUE)
  expected <- c(
    h_slots(data, "nGrid"),
    model@modelspecs(from_prior = TRUE) # nolintr
  )
  expect_identical(result, expected)
})

test_that("h_jags_get_data works with arguments to modelspecs", {
  data <- h_get_data()
  data@y <- c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L)
  model <- h_get_logistic_log_normal()
  e <- environment(model@modelspecs)
  model@modelspecs <- function(x, y) {
    list(ref_dose = ref_dose / sum(y), cov = cov, mean = mean)
  }
  environment(model@modelspecs) <- e

  result <- h_jags_get_data(model, data, from_prior = FALSE)
  expected <- c(
    h_slots(data, c("nObs", "y", "x")),
    list(
      ref_dose = as.numeric(model@ref_dose) / 5,
      cov = model@params@cov,
      mean = model@params@mean
    )
  )
  expect_identical(result, expected)
})

test_that("h_jags_get_data throws the error when `modelspecs` does not return list", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()
  model@modelspecs <- function(x, y) {
    c(p1 = 3)
  }
  expect_error(
    h_jags_get_data(model = model, data = data, from_prior = FALSE),
    "Assertion on 'modelspecs' failed: Must be of type 'list', not 'double'."
  )
})

test_that("h_jags_get_data removes ref_dose from modelspecs when sample from from_prior only", {
  data <- h_get_data()
  model <- h_get_logistic_log_normal()

  result <- h_jags_get_data(model, data, from_prior = TRUE)
  expect_identical(result, h_slots(model@params, c("mean", "prec")))
})

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
  my_model <- .GeneralModel(datamodel=function() {
    alpha0 <- mean(1:10)
    alpha1 <- 600000
  })

  model_file <- h_jags_write_model(my_model, from_prior = FALSE, digits = 5)
  expect_snapshot(readLines(model_file))
  unlink(model_file)
})

test_that("h_jags_write_model works as expected for truncation", {
  my_model <- function() {
    alpha0 <- dnorm(4) %_% I(4)
    alpha1 <- 600000
  }

  model_file <- tempfile("crmPack-testthat-h_jags_write_model-trunc.jags")
  h_jags_write_model(my_model, from_prior = FALSE, model_file, 5)
  expect_snapshot(readLines(model_file))
  unlink(model_file)
})

# h_jags_extract_samples ----

test_that("h_jags_extract_samples works as expected for vector", {
  x <- array(1:6, dim = c(1, 2, 3))
  class(x) <- "mcarray"

  result <- h_jags_extract_samples(x)
  expected <- x[, , 1L]
  expect_identical(result, expected)
})

test_that("h_jags_extract_samples works as expected for matrix", {
  x <- array(1:12, dim = c(2, 2, 3))
  class(x) <- "mcarray"

  result <- h_jags_extract_samples(x)
  expected <- t(x[, , 1L])
  expect_identical(result, expected)
})
