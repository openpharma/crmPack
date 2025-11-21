# Check if an argument is a probability vector

**\[stable\]**

Check if every element in a given numerical vector or matrix represents
a probability, that is a number within (0, 1) interval, that can
optionally be closed at any side.

## Usage

``` r
check_probabilities(
  x,
  bounds_closed = TRUE,
  len = NULL,
  unique = FALSE,
  sorted = FALSE
)

assert_probabilities(
  x,
  bounds_closed = TRUE,
  len = NULL,
  unique = FALSE,
  sorted = FALSE,
  .var.name = checkmate::vname(x),
  add = NULL
)

test_probabilities(
  x,
  bounds_closed = TRUE,
  len = NULL,
  unique = FALSE,
  sorted = FALSE
)

expect_probabilities(
  x,
  bounds_closed = TRUE,
  len = NULL,
  unique = FALSE,
  sorted = FALSE,
  info = NULL,
  label = vname(x)
)
```

## Arguments

- x:

  (`numeric`)  
  vector or matrix with numerical values to check.

- bounds_closed:

  (`logical`)  
  should bounds be closed? This can be a scalar or vector of length two.
  If it is a scalar, then its value applies equally to lower bound \\0\\
  and upper bound \\1\\. If this is a vector with two flags, the first
  flag corresponds to the lower bound \\0\\ only, and the second to the
  upper bound \\1\\ only.

- len:

  \[`integer(1)`\]  
  Exact expected length of `x`.

- unique:

  \[`logical(1)`\]  
  Must all values be unique? Default is `FALSE`.

- sorted:

  \[`logical(1)`\]  
  Elements must be sorted in ascending order. Missing values are
  ignored.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

- info:

  \[`character(1)`\]  
  Extra information to be included in the message for the testthat
  reporter. See
  [`expect_that`](https://testthat.r-lib.org/reference/expect_that.html).

- label:

  \[`character(1)`\]  
  Name of the checked object to print in messages. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

## Value

`TRUE` if successful, otherwise a string with the error message.

## Note

If there are any missing or non-finite values in `x`, this function
returns `FALSE`, regardless of the values of other elements in `x`.

## See also

[`assertions`](https://openpharma.github.io/crmPack/reference/assertions.md)
for more details.

## Examples

``` r
x <- c(0, 0.2, 0.1, 0.3, 1)
check_probabilities(x)
#> [1] TRUE
check_probabilities(x, bounds_closed = FALSE)
#> [1] "Probability must be within (0, 1) bounds but it is not"
check_probabilities(x, bounds_closed = c(FALSE, TRUE))
#> [1] "Probability must be within (0, 1] bounds but it is not"
```
