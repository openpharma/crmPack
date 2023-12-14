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


test_that("h_determine_dlts returns correctly updated data object for default condition", {
  
  data <- new("Data", nGrid = 3L, doseGrid = c(2,3,5))
  dose <- 3
  prob <- 0
  size <- 1
  
  
  result <- h_determine_dlts(data = data,
                             dose = dose,
                             prob = prob ,
                             cohort_size = size,
                             first_separate = FALSE)
  
  expected_result <- data <- new("Data", x = 3, y = 0L, nGrid = 3L, doseGrid = c(2,3,5),
                                 xLevel = 2L, placebo = FALSE, ID = 1L, cohort = 1L,
                                 nObs = 1L)
  
  expect_equal(result, expected_result)
  
})


test_that("h_determine_dlts returns correctly updated data object for 
          first_separate = TRUE & DLT for first patient", {

  data <- new("Data", nGrid = 3L, doseGrid = c(2,3,5))
  dose <- 3
  prob <- 1
  size <- 2


  result <- h_determine_dlts(data = data,
                             dose = dose,
                             prob = prob ,
                             cohort_size = size,
                             first_separate = TRUE)

  expected_result <- data <- new("Data", x = 3, y = 1L, nGrid = 3L, doseGrid = c(2,3,5),
                                 xLevel = 2L, placebo = FALSE, ID = 1L, cohort = 1L,
                                 nObs = 1L)

  expect_equal(result, expected_result)

})


test_that("h_determine_dlts returns correctly updated data object for first_separate = TRUE 
          for no DLT for first patient", {
  
  data <- new("Data", nGrid = 3L, doseGrid = c(2,3,5))
  dose <- 3
  prob <- 0
  size <- 2
  
  
  result <- h_determine_dlts(data = data,
                             dose = dose,
                             prob = prob ,
                             cohort_size = size,
                             first_separate = TRUE)
  
  expected_result <- data <- new("Data", x = c(3, 3), y = c(0L, 0L), nGrid = 3L, doseGrid = c(2,3,5),
                                 xLevel = c(2L, 2L), placebo = FALSE, ID = c(1L, 2L), cohort = c(1L, 1L),
                                 nObs = 2L)
  
  expect_equal(result, expected_result)
  
})


test_that("h_determine_dlts returns correctly updated data object placebo = TRUE", {
            
            data <- new("Data", nGrid = 3L, doseGrid = c(0.0001, 2,3), placebo = TRUE)
            dose <- 3
            prob <- 0
            size <- 1
            size_pl <- 1
            prob_pl <- 0
            
            
            result <- h_determine_dlts(data = data,
                                       dose = dose,
                                       prob = prob ,
                                       prob_placebo = prob_pl,
                                       cohort_size = size,
                                       cohort_size_placebo = size_pl, 
                                       dose_grid = data@doseGrid[1], 
                                       first_separate = FALSE)
            
            expected_result <- data <- new("Data", x = 3, y = 0L, nGrid = 3L, doseGrid = c(0.0001, 2,3),
                                           xLevel = 3L, placebo = TRUE, ID = 1L, cohort = 1L,
                                           nObs = 1L)
            
            expect_equal(result, expected_result)
            
          })
