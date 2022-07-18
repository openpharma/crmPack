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

# h_slots ----

test_that("h_slots returns two slots as expected", {
  object <- h_get_data()
  result <- h_slots(object, c("placebo", "nGrid"))
  expected <- list(placebo = TRUE, nGrid = 13L)

  expect_identical(result, expected)
})

test_that("h_slots returns two slots as expected (simplification ignored)", {
  object <- h_get_data()
  result <- h_slots(object, c("placebo", "nGrid"), simplify = TRUE)
  expected <- list(placebo = TRUE, nGrid = 13L)

  expect_identical(result, expected)
})

test_that("h_slots returns one slot as expected", {
  object <- h_get_data()
  result <- h_slots(object, "placebo")
  expected <- list(placebo = TRUE)

  expect_identical(result, expected)
})

test_that("h_slots returns one slot expected (with simplification)", {
  object <- h_get_data()
  result <- h_slots(object, "placebo", simplify = TRUE)

  expect_identical(result, TRUE)
})

test_that("h_slots throws the error for non-existing slots", {
  object <- h_get_data()
  expect_error(
    h_slots(object, c("placebo", "not_existing_slot_name")),
    "Assertion on 'all\\(names %in% slotNames\\(object\\)\\)' failed: Must be TRUE." # nolintr
  )
})

test_that("h_slots returns empty list for empty request", {
  object <- h_get_data()
  result1 <- h_slots(object, character(0))
  result2 <- h_slots(object, NULL)

  expect_identical(result1, list())
  expect_identical(result2, list())
})

# h_format_number ----

test_that("h_format_number works as expected", {
  result <- c(
    h_format_number(0.0001),
    h_format_number(20000, digits = 3),
    h_format_number(20000, prefix = "P", suffix = "S")
  )
  expected <- c("1.00000E-04", "2.000E+04", "P2.00000E+04S")

  expect_identical(result, expected)
})

test_that("h_format_number works as expected when no change", {
  result <- c(
    h_format_number(1),
    h_format_number(1, digits = 3),
    h_format_number(1, prefix = "P", suffix = "S")
  )
  expected <- c(1, 1, 1)

  expect_identical(result, expected)
})

# h_rapply ----

test_that("h_rapply works as expected", {
  my_model <- function() {
    alpha0 <- mean(1:10)
    alpha1 <- 600000
  }
  # Replace format of numbers using `formatC` function.
  result <- h_rapply(
    x = body(my_model),
    fun = formatC,
    classes = c("integer", "numeric"),
    digits = 3,
    format = "E"
  )
  expected_fun <- function() {
    alpha0 <- mean("1.000E+00":"1.000E+01")
    alpha1 <- "6.000E+05"
  }
  expected <- body(expected_fun)

  expect_identical(result, expected)
})

# h_null_if_na ----

test_that("h_null_if_na works as expected", {
  expect_null(h_null_if_na(NA))
  expect_null(h_null_if_na(NA_integer_))
  expect_null(h_null_if_na(NA_real_))
  expect_null(h_null_if_na(NA_character_))
})

test_that("h_null_if_na throws an error for non-atomic argument", {
  expect_error(
    h_null_if_na(mean),
    "Assertion on 'x' failed: Must be of type 'atomic', not 'closure'."
  )
})

test_that("h_null_if_na throws an error for non-scalar, atomic argument", {
  expect_error(
    h_null_if_na(c(5, NA)),
    "Assertion on 'x' failed: Must have length 1, but has length 2."
  )
  expect_error(
    h_null_if_na(c(NA, NA)),
    "Assertion on 'x' failed: Must have length 1, but has length 2."
  )
})

# h_null_if_scalar ----

test_that("h_null_if_scalar returns NULL as expected", {
  expect_null(h_null_if_scalar(2))
  expect_null(h_null_if_scalar(array(data = 1:12, dim = c(1, 3, 4))))
})

test_that("h_null_if_scalar returns 1L as expected", {
  expect_identical(h_null_if_scalar(c(1, 3)), 1L)
  expect_identical(h_null_if_scalar(array(data = 1:24, dim = c(2, 3, 4))), 1L)
})

# h_is_positive_definite ----

test_that("h_is_positive_definite returns TRUE for 2x2 positive-definite matrix", {
  m <- matrix(c(5, 2, 2, 5), ncol = 2)
  expect_true(h_is_positive_definite(m))
})

test_that("h_is_positive_definite returns TRUE for 3x3 positive-definite matrix", {
  m <- matrix(c(5, 2, 3, 2, 3, 2, 3, 2, 5), ncol = 3)
  expect_true(h_is_positive_definite(m, 3))
})

test_that("h_is_positive_definite returns FALSE for matrix with NA", {
  m <- matrix(c(5, 2, 1, NA), ncol = 2)
  expect_false(h_is_positive_definite(m))
})

test_that("h_is_positive_definite returns FALSE for non-square matrix", {
  m <- matrix(c(-5, 2, 2, 85, 2, 4), ncol = 2)
  expect_false(h_is_positive_definite(m))
})

test_that("h_is_positive_definite returns FALSE for non-symmetric matrix", {
  m <- matrix(c(5, 2, 1, 5), ncol = 2)
  expect_false(h_is_positive_definite(m))
})

test_that("h_is_positive_definite returns FALSE for not a pos-def matrix", {
  m <- matrix(c(-5, 2, 2, 85), ncol = 2)
  expect_false(h_is_positive_definite(m))
})

# h_test_named_numeric ----

test_that("h_test_named_numeric returns TRUE as expected", {
  x <- c(a = 1, b = 2)
  expect_true(h_test_named_numeric(x, subset.of = c("a", "b", "c")))
  expect_true(h_test_named_numeric(x, must.include = "a"))
  expect_true(h_test_named_numeric(x, must.include = "b"))
  expect_true(h_test_named_numeric(x, permutation.of = c("a", "b")))
  expect_true(h_test_named_numeric(x, permutation.of = c("b", "a")))
  expect_true(h_test_named_numeric(x, identical.to = c("a", "b")))
  expect_true(h_test_named_numeric(x, disjunct.from = c("c", "d", "e")))
})

test_that("h_test_named_numeric returns TRUE as expected for duplicated names", {
  x <- c(a = 1, b = 2, b = 3)
  expect_true(h_test_named_numeric(x, len = 3, subset.of = c("a", "b", "c")))
  expect_true(h_test_named_numeric(x, len = 3, identical.to = c("a", "b", "b")))
  expect_true(h_test_named_numeric(x, len = 3, disjunct.from = c("c", "d", "e")))
})

test_that("h_test_named_numeric returns FALSE as expected", {
  x <- c(a = 1, b = 2)
  expect_false(h_test_named_numeric(x, subset.of = c("a", "c")))
  expect_false(h_test_named_numeric(x, must.include = "c"))
  expect_false(h_test_named_numeric(x, permutation.of = c("a", "c")))
  expect_false(h_test_named_numeric(x, identical.to = c("b", "a")))
  expect_false(h_test_named_numeric(x, disjunct.from = c("b", "a")))
  expect_false(h_test_named_numeric(c(a = TRUE, b = FALSE)))
  expect_false(h_test_named_numeric(c(a = "1", b = "2")))
})

# h_in_range ----

test_that("h_in_range returns expected vector of flags for finite interval", {
  x <- c(0.5, -4, 0, -1, 2, 5, 10, Inf, NA, -Inf)
  interval <- c(-1, 5)

  expect_identical(
    h_in_range(c(0.9, -0.4, 0, 0.2, 1, -3, 4, Inf, NA, -Inf)),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, FALSE),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, c(FALSE, TRUE)),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, c(TRUE, FALSE)),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, FALSE)
  )
})

test_that("h_in_range returns expected vector of flags for non-finite bound", {
  x <- c(0.5, -4, 0, -1, 2, 5, 10, Inf, NA, -Inf)
  interval <- c(-1, Inf)

  expect_identical(
    h_in_range(x, interval),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, FALSE),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, c(FALSE, TRUE)),
    c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, NA, FALSE)
  )
  expect_identical(
    h_in_range(x, interval, c(TRUE, FALSE)),
    c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA, FALSE)
  )
})

test_that("h_in_range returns expected matrix of flags", {
  mat <- matrix(c(2, 5, 3, 10, 4, 9, 1, 8, 7), nrow = 3)
  interval <- c(1, 5)

  expect_identical(
    h_in_range(mat, interval),
    matrix(c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE), nrow = 3)
  )
  expect_identical(
    h_in_range(mat, interval, FALSE),
    matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), nrow = 3)
  )
  expect_identical(
    h_in_range(mat, interval, c(FALSE, TRUE)),
    matrix(c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), nrow = 3)
  )
  expect_identical(
    h_in_range(mat, interval, c(TRUE, FALSE)),
    matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE), nrow = 3)
  )
})

test_that("h_in_range throws the error message as expected", {
  x <- 1:3

  expect_error(
    h_in_range(c("a", "b")),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    h_in_range(x, c("a", "b")),
    "Assertion on 'range' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    h_in_range(x, c(1, 4, 5)),
    "Assertion on 'range' failed: Must have length 2, but has length 3."
  )
  expect_error(
    h_in_range(x, c(1, NA)),
    "Assertion on 'range' failed: Contains missing values \\(element 2\\)."
  )
  expect_error(
    h_in_range(x, c(3, 1)),
    "Assertion on 'range' failed: Must be sorted."
  )
  expect_error(
    h_in_range(x, bounds_closed = c(TRUE, FALSE, FALSE, FALSE)),
    "Assertion on 'bounds_closed' failed: Must have length <= 2, but has length 4."
  )
  expect_error(
    h_in_range(x, bounds_closed = c(TRUE, NA)),
    "Assertion on 'bounds_closed' failed: Contains missing values \\(element 2\\)."
  )
})

test_that("h_find_interval works as expected", {
  expect_identical(h_find_interval(-Inf, c(2, 4, 6)), -Inf)
  expect_identical(h_find_interval(1, c(2, 4, 6)), -Inf)
  expect_equal(h_find_interval(2, c(2, 4, 6)), 1)
  expect_equal(h_find_interval(3, c(2, 4, 6)), 1)
  expect_equal(h_find_interval(4, c(2, 4, 6)), 2)
  expect_equal(h_find_interval(5, c(2, 4, 6)), 2)
  expect_equal(h_find_interval(6, c(2, 4, 6)), 3)
  expect_equal(h_find_interval(7, c(2, 4, 6)), 3)
  expect_equal(h_find_interval(Inf, c(2, 4, 6)), 3)
})

test_that("h_find_interval works as expected for custom replacement", {
  expect_identical(h_find_interval(-Inf, c(2, 4, 6), replacement = -1), -1)
  expect_identical(h_find_interval(1, c(2, 4, 6), replacement = -1), -1)
  expect_equal(h_find_interval(2, c(2, 4, 6)), 1)
})
