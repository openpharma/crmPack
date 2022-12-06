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
#'   [assert_probability_range()].
#'
#' @name assertions
NULL

# assert_probabilities ----

#' Check if an argument is a probability vector
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if every element in a given numerical vector or matrix represents a
#' probability, that is a number within (0, 1) interval, that can optionally be
#' closed at any side.
#'
#' @note If there are any missing or non-finite values in `x`, this functions
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

  is_valid <- check_numeric(
    x,
    finite = TRUE, any.missing = FALSE, len = len, unique = unique, sorted = sorted
  )

  if (isTRUE(is_valid)) {
    in_bounds <- all(h_in_range(x, range = c(0L, 1L), bounds_closed = bounds_closed))
    if (!in_bounds) {
      is_valid <- paste(
        "Probability must be within",
        ifelse(bounds_closed[1], "[0,", "(0,"),
        ifelse(tail(bounds_closed, 1), "1]", "1)"),
        "bounds but it is not"
      )
    }
  }

  is_valid
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

#' Check if an argument is one of the given lengths
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check that `x` is one of the lengths specified by the `len` parameter.
#'
#' @param x (`any`)\cr any object for which [base::length()] function is defined.
#' @param len (`integer`)\cr a vector of allowable lengths.
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' x <- 1:5
#' check_length(x, len = 5)
#' check_length(x, len = c(1, 5))
#' check_length(x, len = c(2, 10))
check_length <- function(x, len) {
  assert_numeric(len, lower = 0L, finite = TRUE, any.missing = FALSE, min.len = 1L)
  len <- safeInteger(len)

  is_xlen_ok <- any(length(x) == len)
  if (is_xlen_ok) {
    TRUE
  } else {
    paste(
      "x is of length",
      length(x),
      "which is not allowed; the allowed lengths are:",
      paste(len, collapse = ", "),
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

#' @rdname check_length
#' @inheritParams check_length
#' @export
expect_length <- makeExpectationFunction(check_length)
