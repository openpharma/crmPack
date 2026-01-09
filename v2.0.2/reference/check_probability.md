# Check if an argument is a single probability value

**\[stable\]**

Check if a given value represents a probability, that is a number within
(0, 1) interval, that can optionally be closed at any side.

## Usage

``` r
check_probability(x, bounds_closed = TRUE)

assert_probability(
  x,
  bounds_closed = TRUE,
  .var.name = checkmate::vname(x),
  add = NULL
)

test_probability(x, bounds_closed = TRUE)

expect_probability(x, bounds_closed = TRUE, info = NULL, label = vname(x))
```

## Arguments

- x:

  (`number`)  
  a single value to check.

- bounds_closed:

  (`logical`)  
  should bounds be closed? This can be a scalar or vector of length two.
  If it is a scalar, then its value applies equally to lower bound \\0\\
  and upper bound \\1\\. If this is a vector with two flags, the first
  flag corresponds to the lower bound \\0\\ only, and the second to the
  upper bound \\1\\ only.

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

## See also

[`assertions`](https://openpharma.github.io/crmPack/reference/assertions.md)
for more details.

## Examples

``` r
check_probability(0.5)
#> [1] TRUE
check_probability(0, bounds_closed = FALSE)
#> [1] "Probability must be within (0, 1) bounds but it is not"
check_probability(0, bounds_closed = c(FALSE, TRUE))
#> [1] "Probability must be within (0, 1] bounds but it is not"
```
