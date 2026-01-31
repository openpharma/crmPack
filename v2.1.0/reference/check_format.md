# Check that an argument is a valid format specification

**\[stable\]**

## Usage

``` r
check_format(x, len = NULL, min.len = NULL, max.len = NULL)

assert_format(
  x,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  .var.name = checkmate::vname(x),
  add = NULL
)

test_format(x, len = NULL, min.len = NULL, max.len = NULL)

expect_format(
  x,
  len = NULL,
  min.len = NULL,
  max.len = NULL,
  info = NULL,
  label = vname(x)
)
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- len:

  \[`integer(1)`\]  
  Exact expected length of `x`.

- min.len:

  \[`integer(1)`\]  
  Minimal length of `x`.

- max.len:

  \[`integer(1)`\]  
  Maximal length of `x`.

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
check_format("%5.2f")
#> [1] TRUE
```
