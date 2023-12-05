# set_seed ----

test_that("set_seed returns correct value if seed is a value", {
  seed <- 1.909
  seed_int <- 1

  RNGkind("default")
  rng_state <- set_seed(seed)
  attr(seed_int, "kind") <- list("Mersenne-Twister", "Inversion", "Rejection")
  expect_equal(rng_state, seed_int)

  RNGkind("Super-Duper")
  rng_state <- set_seed(seed)
  attr(seed_int, "kind") <- list("Super-Duper", "Inversion", "Rejection")
  expect_equal(rng_state, seed_int)

  RNGkind("default")
})

test_that("set_seed returns correct value if seed is NULL", {
  seed <- NULL

  RNGkind("default")
  rng_state <- set_seed(seed)
  expect_equal(rng_state, .Random.seed)

  RNGkind("Super-Duper")
  rng_state <- set_seed(seed)
  expect_equal(rng_state, .Random.seed)

  RNGkind("default")
})

# get_result_list ----

test_that("get_result_list returns correct value", {
  res <- get_result_list(mean, 2, NULL, FALSE, 5)
  expect_equal(res, list(1, 2))

  res <- get_result_list(length, 2, NULL, FALSE, 5)
  expect_equal(res, list(1, 1))

  expect_error(get_result_list(length, 2, NULL, 5, 5))
  expect_error(get_result_list(length, 2, NULL, FALSE, 0))
})

# h_add_dlts ----

test_that("h_add_dlts works as expected", {
  data <- h_get_data()
  cohort_size <- CohortSizeConst(3)

  set.seed(123)
  result <- expect_silent(h_add_dlts(
    data = data,
    dose = data@doseGrid[3],
    truth = plogis,
    cohort_size = cohort_size,
    first_separate = FALSE
  ))
  expect_valid(result, "Data")
  expect_equal(tail(result@x, 3), rep(data@doseGrid[3], 3))
  expect_true(data@nObs + 3 == result@nObs)
})

test_that("h_add_dlts works as expected when first separate patient has a DLT", {
  data <- h_get_data()
  cohort_size <- CohortSizeConst(3)

  set.seed(123)
  result <- expect_silent(h_add_dlts(
    data = data,
    dose = data@doseGrid[3],
    truth = function(dose, ...) 1, # Make sure the first patient has a DLT.
    cohort_size = cohort_size,
    first_separate = TRUE
  ))
  expect_valid(result, "Data")
  expect_true(tail(result@y, 1) == 1)
  expect_equal(tail(result@x, 1), data@doseGrid[3])
  expect_true(data@nObs + 1 == result@nObs)
})

test_that("h_add_dlts works as expected when first separate patient does not have a DLT", {
  data <- h_get_data()
  cohort_size <- CohortSizeConst(3)

  set.seed(123)
  result <- expect_silent(h_add_dlts(
    data = data,
    dose = data@doseGrid[3],
    truth = function(dose, ...) 0, # Make sure the first patient does not have a DLT.
    cohort_size = cohort_size,
    first_separate = TRUE
  ))
  expect_valid(result, "Data")
  expect_equal(tail(result@y, 3), rep(0, 3))
  expect_equal(tail(result@x, 3), rep(data@doseGrid[3], 3))
  expect_true(data@nObs + 3 == result@nObs)
})


test_that("h_simulations_ouptput_format returns object as expected", {

  data_test <- new("Data", nGrid = 3L, doseGrid = c(1,3,5))

  dose <- 20

  fit <- data.frame(middle = c(0.2, 0.7), lower = c(0.1, 0.5), upper = c(0.3,0.4))

  stop <- list(list("Number of cohorts is 10 and thus reached the prespecified minimum number 3"))

  report_results <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  names(report_results) <- c(NA, NA, NA, NA, NA)

  additional_stats <- list()

  resultList_test <- list(list(
                          data = data_test,
                          dose = dose,
                          fit = fit,
                          stop = stop,
                          report_results = report_results,
                          additional_stats = additional_stats))

  simulations_output <- h_simulations_ouptput_format(resultList_test)

  expect_equal(simulations_output$dataList[[1]], data_test)
  expect_equal(simulations_output$recommendedDoses, dose)
  expect_equal(simulations_output$fitList[[1]], fit)
  expect_equal(simulations_output$stop_matrix, do.call(rbind,lapply(resultList_test, "[[", "report_results")))
  })


test_that("h_this_truth returns correct results for given dose", {
  args <- NULL
  args <- as.data.frame(args)
  nArgs <- max(nrow(args), 1L)
  iterSim <- 5
  this_args <- args[(iterSim - 1) %% nArgs + 1, , drop = FALSE]

  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov =
      matrix(c(1, -0.5, -0.5, 1),
        nrow = 2
      ),
    ref_dose = 56
  )

  my_truth <- probFunction(model, alpha0 = 7, alpha1 = 8)

  result <- h_this_truth(30, this_args, my_truth)
  expect_equal(result, 0.8815056)
})
