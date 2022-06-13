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
#' @seealso [assert_probability()], [assert_prob()], [assert_prob_range()].
#'
#' @name assertions
NULL

# assert_probability ----

#' Check if an Argument is a Probability Vector
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if every element in a given numerical vector represent a probability,
#' that is a number within (0, 1) interval, that can optionally be closed at any
#' side.
#'
#' @param x (`numeric`)\cr vector with numerical values to check.
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
#' check_probability(x)
#' check_probability(x, bounds_closed = FALSE)
#' check_probability(x, bounds_closed = c(FALSE, TRUE))
check_probability <- function(x, bounds_closed = TRUE, len = NULL, sorted = FALSE) {
  assert_logical(bounds_closed, min.len = 1, max.len = 2, any.missing = FALSE)

  is_valid <- check_numeric(
    x,
    finite = TRUE,
    any.missing = FALSE,
    len = len,
    sorted = sorted
  )

  if (length(bounds_closed) == 1L) {
    bounds_closed <- rep(bounds_closed, 2L)
  }

  if (isTRUE(is_valid)) {
    lwr <- ifelse(bounds_closed[1], all(x >= 0), all(x > 0))
    upr <- ifelse(bounds_closed[2], all(x <= 1), all(x < 1))
    in_bounds <- lwr && upr

    if (!in_bounds) {
      is_valid <- paste(
        "Probability must be within",
        ifelse(bounds_closed[1], "[0,", "(0,"),
        ifelse(bounds_closed[2], "1]", "1)"),
        "bounds but it is not"
      )
    }
  }

  is_valid
}

#' @rdname check_probability
#' @inheritParams check_probability
#' @export
assert_probability <- makeAssertionFunction(check_probability)

#' @rdname check_probability
#' @export
test_probability <- makeTestFunction(check_probability)

#' @rdname check_probability
#' @inheritParams check_probability
#' @export
expect_probability <- makeExpectationFunction(check_probability)

# assert_prob ----

#' Check if an Argument is a Single Probability Value
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if a given value represents a probability, that is a number within
#' (0, 1) interval, that can optionally be closed at any side.
#'
#' @param x (`number`)\cr a single value to check.
#' @inheritParams check_probability
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' check_prob(0.5)
#' check_prob(0, bounds_closed = FALSE)
#' check_prob(0, bounds_closed = c(FALSE, TRUE))
check_prob <- function(x, bounds_closed = TRUE) {
  check_probability(x = x, bounds_closed = bounds_closed, len = 1)
}

#' @rdname check_prob
#' @inheritParams check_prob
#' @export
assert_prob <- makeAssertionFunction(check_prob)

#' @rdname check_prob
#' @export
test_prob <- makeTestFunction(check_prob)

#' @rdname check_prob
#' @inheritParams check_prob
#' @export
expect_prob <- makeExpectationFunction(check_prob)

# assert_prob_range ----

#' Check if an Argument is a Probability Range
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Check if a given numerical interval represents a probability range, that is
#' a sub-interval of (0, 1) interval, that can optionally be closed at any side.
#'
#' @param x (`number`)\cr an interval to check.
#' @inheritParams check_probability
#'
#' @return `TRUE` if successful, otherwise a string with the error message.
#'
#' @seealso [`assertions`] for more details.
#'
#' @export
#' @examples
#' x <- c(0, 0.2)
#' check_prob_range(x)
#' check_prob_range(rev(x))
#' check_prob_range(x, bounds_closed = FALSE)
#' check_prob_range(x, bounds_closed = c(FALSE, TRUE))
check_prob_range <- function(x, bounds_closed = TRUE) {
  check_probability(x = x, bounds_closed = bounds_closed, len = 2, sorted = TRUE)
}

#' @rdname check_prob_range
#' @inheritParams check_prob_range
#' @export
assert_prob_range <- makeAssertionFunction(check_prob_range)

#' @rdname check_prob_range
#' @export
test_prob_range <- makeTestFunction(check_prob_range)

#' @rdname check_prob_range
#' @inheritParams check_prob_range
#' @export
expect_prob_range <- makeExpectationFunction(check_prob_range)
