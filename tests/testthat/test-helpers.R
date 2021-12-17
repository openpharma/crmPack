# h_all_equivalent ----

test_that("h_all_equivalent returns TRUE for equivalent objects", {
  target <- structure(
    c(1, 2, 3.1),
    names = letters[1:3],
    some_attr = "some_attr"
  )
  current <- structure(
    c(1, 2, 3.1),
    names = letters[4:6],
    some_attr = "some_attr1"
  )

  result <- h_all_equivalent(target, current)
  expect_true(result)
})

test_that("h_all_equivalent returns TRUE for equivalent objects", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.3)
  # Mean relative difference: 0.2 < tolerance = 0.3
  expect_true(result)
})

test_that("h_all_equivalent returns FALSE for non-equivalent objects", {
  target <- c(1, 2, 3)
  current <- c(1, 2, 3.6)

  result <- h_all_equivalent(target, current, tolerance = 0.1)
  # Mean relative difference: 0.2 > tolerance = 0.1
  expect_false(result)
})

# h_plot_data_df ----

test_that("h_plot_data_df valid object for sample Data object with placebo", {
  data <- h_get_data()
  result <- h_plot_data_df(data)
  expected <- data.frame(
    patient = 1:12,
    ID = paste(" ", 1:12),
    cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
    dose = c(0, 25, 25, 25, 0, 50, 50, 50, 0, 100, 100, 100),
    toxicity = c(rep("No", 10), "Yes", "No")
  )

  expect_identical(result, expected)
})

test_that("h_plot_data_df returns valid object: Data with placebo and blind.", {
  data <- h_get_data()
  result <- h_plot_data_df(data, blind = TRUE)
  expected <- data.frame(
    patient = 1:12,
    ID = paste(" ", 1:12),
    cohort = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
    dose = rep(c(25, 50, 100), each = 4),
    toxicity = c(rep("No", 8), "Yes", rep("No", 3))
  )

  expect_identical(result, expected)
})

# h_plot_data_cohort_lines ----

test_that("h_plot_data_cohort_lines works as expected", {
  data <- h_get_data()
  data@placebo <- TRUE
  df <- h_plot_data_df(data)

  result <- ggplot(df, aes(x = patient, y = dose)) +
    geom_point() +
    h_plot_data_cohort_lines(df$cohort, placebo = data@placebo)

  vdiffr::expect_doppelganger("h_plot_data_cohort_lines with placego", result)
})

test_that("h_plot_data_cohort_lines works as expected when no placebo", {
  data <- h_get_data()
  data@placebo <- FALSE
  df <- h_plot_data_df(data)

  result <- ggplot(df, aes(x = patient, y = dose)) +
    geom_point() +
    h_plot_data_cohort_lines(df$cohort, placebo = data@placebo)

  vdiffr::expect_doppelganger(
    "h_plot_data_cohort_lines without placebo",
    result
  )
})

test_that("h_plot_data_cohort_lines works as expected for single cohort", {
  data <- h_get_data()
  data@placebo <- TRUE
  data@cohort <- rep(1L, data@nObs)
  df <- h_plot_data_df(data)

  result <- ggplot(df, aes(x = patient, y = dose)) +
    geom_point() +
    h_plot_data_cohort_lines(df$cohort, placebo = data@placebo)

  vdiffr::expect_doppelganger(
    "h_plot_data_cohort_lines for single cohort",
    result
  )
})

# h_check_fun_formals ----

test_that("h_check_fun_formals returns TRUE for valid arguments", {
  # nolint start
  result <- c(
    a = h_check_fun_formals(function() {}, mandatory = NULL, allowed = NULL),
    b = h_check_fun_formals(function() {}, mandatory = NULL, allowed = "a"),
    c = h_check_fun_formals(function(a) {}, mandatory = NULL, allowed = "a"),
    d = h_check_fun_formals(function(m) {}, mandatory = "m", allowed = NULL),
    e = h_check_fun_formals(function(m) {}, mandatory = "m", allowed = "a"),
    f = h_check_fun_formals(function(m, a) {}, mandatory = "m", allowed = "a")
  )
  # nolint end

  result <- all(result)
  expect_true(result)
})

test_that("h_check_fun_formals returns FALSE for non-valid arguments", {
  # nolint start
  result <- c(
    a = h_check_fun_formals(function(x) {}, mandatory = NULL, allowed = NULL),
    b = h_check_fun_formals(function(x) {}, mandatory = NULL, allowed = "a"),
    c = h_check_fun_formals(function(a, x) {}, mandatory = NULL, allowed = "a"),
    d = h_check_fun_formals(function() {}, mandatory = "m", allowed = NULL),
    e = h_check_fun_formals(function(x) {}, mandatory = "m", allowed = NULL),
    f = h_check_fun_formals(function(m, x) {}, mandatory = "m", allowed = NULL),
    g = h_check_fun_formals(function() {}, mandatory = "m", allowed = "a"),
    h = h_check_fun_formals(function(a) {}, mandatory = "m", allowed = "a"),
    i = h_check_fun_formals(function(x) {}, mandatory = "m", allowed = "a"),
    j = h_check_fun_formals(function(x, a) {}, mandatory = "m", allowed = "a"),
    k = h_check_fun_formals(function(m, x) {}, mandatory = "m", allowed = "a"),
    l = h_check_fun_formals(function(m, a, x) {}, mandatory = "m", allowed = "a")
  )
  # nolint end

  result <- any(result)
  expect_false(result)
})
