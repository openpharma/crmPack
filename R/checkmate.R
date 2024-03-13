# assertions ----

#' Additional Assertions for `checkmate`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional assertion functions that can be used together with
#' the `checkmate` functions. These are described in individual help pages
#' linked below.
#'
#' @return Depending on the function prefix.
#' - `assert_` functions return the object invisibly if successful, and otherwise
#'   throw an error message.
#' - `check_` functions return `TRUE` if successful, otherwise a string with the
#'   error message.
#' - `test_` functions just return `TRUE` or `FALSE`.
#'
#' @seealso [assert_probabilities()], [assert_probability()],
#'   [assert_probability_range()], [assert_length()].
#'
#' @name assertions
NULL

# check equality ----

#' Check if All Arguments Are Equal
#'
#' @description `r lifecycle::badge("experimental")`
#' Elements of `...` must be numeric vectors or scalars.
#'
#' This function performs an element-by-element comparison of the first object
#' provided in `...` with every other object in `...` and returns `TRUE` if all
#' comparisons are equal within a given tolerance and `FALSE` otherwise.
#'
#' @param ... (`numeric`)\cr vectors to be compared.
#' @param tol (`numeric`)\cr the maximum difference to be tolerated when
#' judging equality.
#'
#' @note If there are any missing or infinite values in `...`, this function
#'   returns `FALSE`, regardless of the values of other elements in `...`.
#'
#' @note If elements in `...` are not all of the same length, `FALSE` is returned.
#'
#' @return `TRUE` if all element-by-element differences are less than `tolerance`
#' in magnitude, `FALSE` otherwise.
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_equal(1:2, 1:2) # TRUE
#' check_equal(1:2, 2:3) # "Not all equal"
#' check_equal(Inf, Inf) # "Not all equal"
#' check_equal(0.01, 0.02) # "Not all equal"
#' check_equal(0.01, 0.02, tol = 0.05) # TRUE
#' check_equal(1, c(1, 1)) # "Not all equal"
check_equal <- function(..., tol = sqrt(.Machine$double.eps)) {
  dot_args <- list(...)

  sapply(dot_args, assert_numeric)

  tmp <- sapply(dot_args, length)
  if (min(tmp) != max(tmp)) {
    return("Not all of same length")
  }
  if (any(sapply(dot_args, is.na))) {
    return("Some entries NA")
  }
  if (any(sapply(dot_args, is.infinite))) {
    return("Not all entries finite")
  }
  if (!all(sapply(dot_args, test_numeric))) {
    return("Not all numeric")
  }

  all_ok <- test_true(
    all(
      sapply(
        2:length(dot_args),
        function(z) abs(dot_args[[1]] - dot_args[[z]]) < tol
      )
    )
  )
  if (all_ok) {
    return(TRUE)
  }
  "Not all equal"
}

#' Assert That All Arguments Are Equal
#'
#' @description `r lifecycle::badge("experimental")`
#' Elements of `...` must be numeric vectors or scalars.
#'
#' This function performs an element-by-element comparison of the first object
#' provided in `...` with every other object in `...` and throws an error if they
#' are not.
#'
#' @param ... (`numeric`)\cr vectors to be compared
#' @param tol (`numeric`)\cr the maximum difference to be tolerated when
#' judging equality
#'
#' @note If there are any missing or infinite values in `...`, this function
#'   throws an error, regardless of the values of other elements in `...`.
#'
#' @note If elements in `...` are not all of the same length, an error is thrown.
#'
#' @return `list(...)`, invisibly.
#' @seealso [`assertions`] for more details.
#' @inheritParams checkmate::assert_numeric
#'
#' @export
#' @rdname check_equal
#' @examples
#' assert_equal(1:2, 1:2) # no error
#' assert_equal(0.01, 0.02, tol = 0.05) # no error
# nolint start
assert_equal <- function(..., tol = sqrt(.Machine$double.eps), .var.name = vname(x), add = NULL) {
  # assert_equal <- makeAssertionFunction(check_equal) fails with error "Error
  # in `checkmate::makeAssertion(..., res, .var.name, add)`: unused argument
  # (add)", possibly because of the use of ... in check_equal.
  res <- check_equal(..., tol = tol)
  makeAssertion(list(...), res, .var.name, add)
}
# nolint end

# assert_probabilities ----

#' Check if an argument is a probability vector
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if every element in a given numerical vector or matrix represents a
#' probability, that is a number within (0, 1) interval, that can optionally be
#' closed at any side.
#'
#' @note If there are any missing or non-finite values in `x`, this function
#'   returns `FALSE`, regardless of the values of other elements in `x`.
#'
#' @param x (`numeric`)\cr vector or matrix with numerical values to check.
#' @param bounds_closed (`logical`)\cr should bounds be closed? This can be a
#'   scalar or vector of length two. If it is a scalar, then its value applies
#'   equally to lower bound \eqn{0} and upper bound \eqn{1}. If this is a vector
#'   with two flags, the first flag corresponds to the lower bound \eqn{0}
#'   only, and the second to the upper bound \eqn{1} only.
#' @inheritParams checkmate::check_numeric
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' x <- c(0, 0.2, 0.1, 0.3, 1)
#' check_probabilities(x)
#' check_probabilities(x, bounds_closed = FALSE)
#' check_probabilities(x, bounds_closed = c(FALSE, TRUE))
check_probabilities <- function(x, bounds_closed = TRUE, len = NULL, unique = FALSE, sorted = FALSE) {
  assert_numeric(x)
  assert_logical(bounds_closed, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_number(len, null.ok = TRUE)
  assert_flag(sorted)

  result <- check_numeric(
    x,
    finite = TRUE, any.missing = FALSE, len = len, unique = unique, sorted = sorted
  )

  if (isTRUE(result)) {
    in_bounds <- all(h_in_range(x, range = c(0L, 1L), bounds_closed = bounds_closed))
    if (!in_bounds) {
      result <- paste(
        "Probability must be within",
        ifelse(bounds_closed[1], "[0,", "(0,"),
        ifelse(tail(bounds_closed, 1), "1]", "1)"),
        "bounds but it is not"
      )
    }
  }

  result
}

#' @rdname check_probabilities
#' @inheritParams check_probabilities
#' @export
assert_probabilities <- makeAssertionFunction(check_probabilities)

#' @rdname check_probabilities
#' @inheritParams check_probabilities
#' @export
test_probabilities <- makeTestFunction(check_probabilities)

#' @rdname check_probabilities
#' @inheritParams check_probabilities
#' @export
expect_probabilities <- makeExpectationFunction(check_probabilities)

# assert_probability ----

#' Check if an argument is a single probability value
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if a given value represents a probability, that is a number within
#' (0, 1) interval, that can optionally be closed at any side.
#'
#' @param x (`number`)\cr a single value to check.
#' @inheritParams check_probabilities
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_probability(0.5)
#' check_probability(0, bounds_closed = FALSE)
#' check_probability(0, bounds_closed = c(FALSE, TRUE))
check_probability <- function(x, bounds_closed = TRUE) {
  check_probabilities(x = x, bounds_closed = bounds_closed, len = 1)
}

#' @rdname check_probability
#' @inheritParams check_probability
#' @export
assert_probability <- makeAssertionFunction(check_probability)

#' @rdname check_probability
#' @inheritParams check_probability
#' @export
test_probability <- makeTestFunction(check_probability)

#' @rdname check_probability
#' @inheritParams check_probability
#' @export
expect_probability <- makeExpectationFunction(check_probability)

# assert_probability_range ----

#' Check if an argument is a probability range
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if a given numerical interval represents a probability range, that is
#' a sub-interval of (0, 1) interval, that can optionally be closed at any side.
#'
#' @param x (`number`)\cr an interval to check.
#' @inheritParams check_probabilities
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' x <- c(0, 0.2)
#' check_probability_range(x)
#' check_probability_range(rev(x))
#' check_probability_range(x, bounds_closed = FALSE)
#' check_probability_range(x, bounds_closed = c(FALSE, TRUE))
check_probability_range <- function(x, bounds_closed = TRUE) {
  check_probabilities(x = x, bounds_closed = bounds_closed, len = 2, sorted = TRUE)
}

#' @rdname check_probability_range
#' @inheritParams check_probability_range
#' @export
assert_probability_range <- makeAssertionFunction(check_probability_range)

#' @rdname check_probability_range
#' @inheritParams check_probability_range
#' @export
test_probability_range <- makeTestFunction(check_probability_range)

#' @rdname check_probability_range
#' @inheritParams check_probability_range
#' @export
expect_probability_range <- makeExpectationFunction(check_probability_range)

# assert_length ----

#' Check if vectors are of compatible lengths
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Two vectors are of compatible size if and only if: \cr
#' 1. At least one vector has size 1 \cr
#' 2. or both vectors are of the same size. \cr
#'
#' @param x (`any`)\cr the first vector, any object for which [length()]
#'   function is defined.
#' @param len (`count`)\cr the length of the second vector.
#' @inheritParams checkmate::check_numeric
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_length(1:5, 1)
#' check_length(1:5, 6)
#' check_length(1:5, 5)
#' check_length(10, 1)
#' check_length(10, 9)
check_length <- function(x, len) {
  x_len <- length(x)
  assert_true(x_len >= 1L)
  assert_count(len)

  if (x_len == 1L || len == 1L || x_len == len) {
    TRUE
  } else {
    paste(
      "x is of length",
      x_len,
      "which is not allowed; the allowed lengths are: 1 or",
      len,
      collapse = ""
    )
  }
}

#' @rdname check_length
#' @inheritParams check_length
#' @export
assert_length <- makeAssertionFunction(check_length)

#' @rdname check_length
#' @inheritParams check_length
#' @export
test_length <- makeTestFunction(check_length)

# assert_range ----

#' Check that an argument is a numerical range
#'
#' @description `r lifecycle::badge("stable")`
#'
#' An argument `x` is a numerical range if and only if (all conditions must be met):
#' 1. Is an object of type: `integer` or `double`.
#' 2. Is a vector or length two such that the value of the first number is not
#' less than the second number. Equalness is allowed if and only if `unique` flag
#' is set to `TRUE`.
#' 3. Lower bound of the interval is greater than or equal to `lower` and
#' upper bound of the interval is less than or equal to `upper`.
#' 4. It contains only finite (given that `finite` is `TRUE`) and non-missing values.
#'
#' @inheritParams checkmate::check_numeric
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_range(c(1, 5))
#' check_range(c(-5, 1))
#' check_range(c(4, 1))
#' check_range(c(1, 1))
#' check_range(c(1, 1), unique = FALSE)
#' check_range(1:3)
check_range <- function(x, lower = -Inf, upper = Inf, finite = FALSE, unique = TRUE) {
  assert_number(lower)
  assert_number(upper)
  assert_flag(finite)
  assert_flag(unique)

  result <- check_numeric(
    x,
    lower = lower,
    upper = upper,
    finite = finite,
    any.missing = FALSE,
    len = 2,
    unique = unique,
    sorted = TRUE
  )

  if (!isTRUE(result)) {
    result <- paste("x must be a valid numerical range.", result)
  }
  result
}

#' @rdname check_range
#' @inheritParams check_range
#' @export
assert_range <- makeAssertionFunction(check_range)

#' @rdname check_range
#' @inheritParams check_range
#' @export
test_range <- makeTestFunction(check_range)

#' @rdname check_range
#' @inheritParams check_range
#' @export
expect_range <- makeExpectationFunction(check_range)

# assert_format ----

#' Check that an argument is a valid format specification
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams checkmate::check_numeric
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_format("%5.2f")
check_format <- function(x, len = NULL, min.len = NULL, max.len = NULL) {
  assert_number(len, lower = 1, null.ok = TRUE)
  assert_number(min.len, lower = 1, null.ok = TRUE)
  assert_number(max.len, lower = 1, null.ok = TRUE)

  result <- check_character(
    x,
    len = len,
    min.len = min.len,
    max.len = max.len,
    any.missing = FALSE,
    # https://stackoverflow.com/questions/446285/validate-sprintf-format-from-input-field-with-regex
    pattern = "%(?:\\d+\\$)?[+-]?(?:[ 0]|'.{1})?-?\\d*(?:\\.\\d+)?[bcdeEufFgGosxX]",
  )

  if (!isTRUE(result)) {
    result <- paste("x must be a valid format specifier.", result)
  }
  result
}

#' @rdname check_format
#' @inheritParams check_format
#' @export
assert_format <- makeAssertionFunction(check_format)

#' @rdname check_format
#' @inheritParams check_format
#' @export
test_format <- makeTestFunction(check_format)

#' @rdname check_format
#' @inheritParams check_format
#' @export
expect_format <- makeExpectationFunction(check_format)
