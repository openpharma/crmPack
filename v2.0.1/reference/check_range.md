# Check that an argument is a numerical range

**\[stable\]**

An argument `x` is a numerical range if and only if (all conditions must
be met):

1.  Is an object of type: `integer` or `double`.

2.  Is a vector or length two such that the value of the first number is
    not less than the second number. Equalness is allowed if and only if
    `unique` flag is set to `TRUE`.

3.  Lower bound of the interval is greater than or equal to `lower` and
    upper bound of the interval is less than or equal to `upper`.

4.  It contains only finite (given that `finite` is `TRUE`) and
    non-missing values.

## Usage

``` r
check_range(x, lower = -Inf, upper = Inf, finite = FALSE, unique = TRUE)

assert_range(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  unique = TRUE,
  .var.name = checkmate::vname(x),
  add = NULL
)

test_range(x, lower = -Inf, upper = Inf, finite = FALSE, unique = TRUE)

expect_range(
  x,
  lower = -Inf,
  upper = Inf,
  finite = FALSE,
  unique = TRUE,
  info = NULL,
  label = vname(x)
)
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- lower:

  \[`numeric(1)`\]  
  Lower value all elements of `x` must be greater than or equal to.

- upper:

  \[`numeric(1)`\]  
  Upper value all elements of `x` must be lower than or equal to.

- finite:

  \[`logical(1)`\]  
  Check for only finite values? Default is `FALSE`.

- unique:

  \[`logical(1)`\]  
  Must all values be unique? Default is `FALSE`.

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
check_range(c(1, 5))
#> [1] TRUE
check_range(c(-5, 1))
#> [1] TRUE
check_range(c(4, 1))
#> [1] "x must be a valid numerical range. Must be sorted"
check_range(c(1, 1))
#> [1] "x must be a valid numerical range. Contains duplicated values, position 2"
check_range(c(1, 1), unique = FALSE)
#> [1] TRUE
check_range(1:3)
#> [1] "x must be a valid numerical range. Must have length 2, but has length 3"
```
