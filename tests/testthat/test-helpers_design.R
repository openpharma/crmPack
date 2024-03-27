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



test_that("h_simulations_output_format returns object as expected", {
  data_test <- new("Data", nGrid = 3L, doseGrid = c(1, 3, 5))
  dose <- 20
  fit <- data.frame(middle = c(0.2, 0.7), lower = c(0.1, 0.5), upper = c(0.3, 0.4))
  stop <- list(list("Number of cohorts is 10 and thus reached the prespecified minimum number 3"))
  report_results <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  names(report_results) <- c(NA, NA, NA, NA, NA)
  additional_stats <- list()

  result_list_test <- list(list(
    data = data_test,
    dose = dose,
    fit = fit,
    stop = stop,
    report_results = report_results,
    additional_stats = additional_stats
  ))

  simulations_output <- h_simulations_output_format(result_list_test)

  expect_equal(simulations_output$dataList[[1]], data_test)
  expect_equal(simulations_output$recommendedDoses, dose)
  expect_equal(simulations_output$fitList[[1]], fit)
  expect_equal(simulations_output$stop_matrix, do.call(rbind, lapply(result_list_test, "[[", "report_results")))
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
  data <- new("Data", nGrid = 3L, doseGrid = c(2, 3, 5))
  dose <- 3
  prob <- 0
  size <- 1


  result <- h_determine_dlts(
    data = data,
    dose = dose,
    prob = prob,
    cohort_size = size,
    first_separate = FALSE
  )

  expected_result <- data <- new("Data",
    x = 3, y = 0L, nGrid = 3L, doseGrid = c(2, 3, 5),
    xLevel = 2L, placebo = FALSE, ID = 1L, cohort = 1L,
    nObs = 1L
  )

  expect_valid(result, "Data")
  expect_equal(result, expected_result)
})


test_that("h_determine_dlts returns correctly updated data object for
          first_separate = TRUE & DLT for first patient", {
  data <- new("Data", nGrid = 3L, doseGrid = c(2, 3, 5))
  dose <- 3
  prob <- 1
  size <- 2


  result <- h_determine_dlts(
    data = data,
    dose = dose,
    prob = prob,
    cohort_size = size,
    first_separate = TRUE
  )

  expected_result <- data <- new("Data",
    x = 3, y = 1L, nGrid = 3L, doseGrid = c(2, 3, 5),
    xLevel = 2L, placebo = FALSE, ID = 1L, cohort = 1L,
    nObs = 1L
  )

  expect_s4_class(result, "Data")
  expect_equal(result, expected_result)
})


test_that("h_determine_dlts returns correctly updated data object for first_separate = TRUE
          for no DLT for first patient", {
  data <- new("Data", nGrid = 3L, doseGrid = c(2, 3, 5))
  dose <- 3
  prob <- 0
  size <- 2


  result <- h_determine_dlts(
    data = data,
    dose = dose,
    prob = prob,
    cohort_size = size,
    first_separate = TRUE
  )

  expected_result <- data <- new("Data",
    x = c(3, 3), y = c(0L, 0L), nGrid = 3L, doseGrid = c(2, 3, 5),
    xLevel = c(2L, 2L), placebo = FALSE, ID = c(1L, 2L), cohort = c(1L, 1L),
    nObs = 2L
  )

  expect_s4_class(result, "Data")
  expect_equal(result, expected_result)
})


test_that("h_determine_dlts returns correctly updated data object for placebo = TRUE", {
  data <- new("Data", nGrid = 3L, doseGrid = c(0.0001, 2, 3), placebo = TRUE)
  dose <- 3
  prob <- 0
  size <- 1
  size_pl <- 1
  prob_pl <- 0


  result <- h_determine_dlts(
    data = data,
    dose = dose,
    prob = prob,
    prob_placebo = prob_pl,
    cohort_size = size,
    cohort_size_placebo = size_pl,
    dose_grid = data@doseGrid[1],
    first_separate = FALSE
  )

  expected_result <- data <- new("Data",
    x = c(0.0001, 3), y = c(0L, 0L), nGrid = 3L, doseGrid = c(0.0001, 2, 3),
    xLevel = c(1L, 3L), placebo = TRUE, ID = c(1L, 2L), cohort = c(1L, 1L),
    nObs = 2L
  )

  expect_s4_class(result, "Data")
  expect_equal(result, expected_result)
})

test_that("simulate for the class design works end to end", {
  emptydata <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

  model <- LogisticLogNormal(
    mean = c(-0.85, 1),
    cov =
      matrix(c(1, -0.5, -0.5, 1),
        nrow = 2
      ),
    ref_dose = 56
  )

  myNextBest <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  )

  mySize1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
  )
  mySize2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
  )
  mySize <- maxSize(mySize1, mySize2)

  myStopping1 <- StoppingMinCohorts(nCohorts = 3)
  myStopping2 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  )
  myStopping3 <- StoppingMinPatients(nPatients = 20)
  myStopping <- (myStopping1 & myStopping2) | myStopping3

  myIncrements <- IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  )

  design <- Design(
    model = model,
    nextBest = myNextBest,
    stopping = myStopping,
    increments = myIncrements,
    cohort_size = mySize,
    data = emptydata,
    startingDose = 3
  ) # check if default design constructor is enough to test simulate method

  myTruth <- probFunction(model, alpha0 = 7, alpha1 = 8)

  options <- McmcOptions(
    burnin = 100,
    step = 2,
    samples = 1000,
    rng_kind = "Mersenne-Twister",
    rng_seed = 1234
  )
  time <- system.time(mySims <- simulate(design,
    args = NULL,
    truth = myTruth,
    nsim = 1,
    seed = 819,
    mcmcOptions = options,
    parallel = FALSE,
    derive = list(
      max_mtd = max,
      mean_mtd = mean,
      median_mtd = median
    ),
  ))[3]

  expected <-
    new(
      "Simulations",
      fit = list(structure(
        list(
          middle = c(
            0.0278840989442296,
            0.0585187075243448,
            0.0861763463009416,
            0.152046506082125,
            0.215028653301285,
            0.27436876755974,
            0.329137865843746,
            0.463222935947319,
            0.529835624620552,
            0.658215004013923,
            0.709356365120325
          ),
          lower = c(
            0.000124727518807684,
            0.00169457982279485,
            0.00571810889381897,
            0.0262840977019272,
            0.0631561484944847,
            0.108053895434153,
            0.14935814438918,
            0.254733947607829,
            0.299313351022304,
            0.377782206208248,
            0.41692555950274
          ),
          upper = c(
            0.173968745159563,
            0.236079098676,
            0.285833540365002,
            0.35945722660184,
            0.41825266097964,
            0.461892665717615,
            0.517432747132171,
            0.666615508629213,
            0.750262264008957,
            0.886377462207185,
            0.928390287149569
          )
        ),
        row.names = c(NA, 11L),
        class = "data.frame"
      )),
      stop_report = structure(
        c(
          TRUE, TRUE,
          TRUE, TRUE, FALSE
        ),
        dim = c(1L, 5L),
        dimnames = list(
          NULL,
          c(
            NA_character_,
            NA_character_,
            NA_character_,
            NA_character_,
            NA_character_
          )
        )
      ),
      stop_reasons = list(
        list(
          list(
            "Number of cohorts is 9 and thus reached the prespecified minimum number 3",
            "Probability for target toxicity is 53 % for dose 20 and thus above the required 50 %"
          ),
          "Number of patients is 19 and thus below the prespecified minimum number 20"
        )
      ),
      additional_stats = list(
        list(
          max_mtd = 70.6519400150433,
          mean_mtd = 21.3728034044661,
          median_mtd = 20.2435513052115
        )
      ),
      data = list(
        new(
          "Data",
          x = c(
            3, 5, 10, 20, 20, 20, 20, 25,
            25, 25, 25, 25, 25, 25, 25, 25, 15, 15, 15
          ),
          y = c(
            0L, 0L,
            0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 0L,
            0L, 0L
          ),
          doseGrid = c(
            1, 3, 5, 10, 15, 20, 25, 40, 50, 80,
            100
          ),
          nGrid = 11L,
          xLevel = c(
            2L, 3L, 4L, 6L, 6L, 6L, 6L,
            7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 5L, 5L, 5L
          ),
          placebo = FALSE,
          ID = 1:19,
          cohort = c(
            1L, 2L, 3L, 4L, 5L, 5L, 5L, 6L,
            6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L
          ),
          nObs = 19L
        )
      ),
      doses = 20,
      seed = 819L
    )

  expect_equal(mySims, expected)

  expect_class(mySims, "Simulations")

  expect_equal(any(sapply(mySims@fit[[1]], is.numeric)), TRUE) # check if all elements in mySims@fit are numeric

  expect_equal(length(mySims@stop_report), 5) # check for length

  expect_logical(mySims@stop_report) # check for stop_report to be logical vector
})
