# Check that an argument is a named vector of type numeric

**\[stable\]**

A simple helper function that tests whether an object is a named
numerical vector.

## Usage

``` r
h_test_named_numeric(
  x,
  subset.of = NULL,
  must.include = NULL,
  permutation.of = NULL,
  identical.to = NULL,
  disjunct.from = NULL,
  lower = 0 + .Machine$double.xmin,
  finite = TRUE,
  any.missing = FALSE,
  len = 2,
  ...
)
```

## Arguments

- x:

  (`any`)  
  object to check.

- subset.of:

  \[`character`\]  
  Names provided in `x` must be subset of the set `subset.of`.

- must.include:

  \[`character`\]  
  Names provided in `x` must be a superset of the set `must.include`.

- permutation.of:

  \[`character`\]  
  Names provided in `x` must be a permutation of the set
  `permutation.of`. Duplicated names in `permutation.of` are stripped
  out and duplicated names in `x` thus lead to a failed check. Use this
  argument instead of `identical.to` if the order of the names is not
  relevant.

- identical.to:

  \[`character`\]  
  Names provided in `x` must be identical to the vector `identical.to`.
  Use this argument instead of `permutation.of` if the order of the
  names is relevant.

- disjunct.from:

  \[`character`\]  
  Names provided in `x` must may not be present in the vector
  `disjunct.from`.

- lower:

  \[`numeric(1)`\]  
  Lower value all elements of `x` must be greater than or equal to.

- finite:

  \[`logical(1)`\]  
  Check for only finite values? Default is `FALSE`.

- any.missing:

  \[`logical(1)`\]  
  Are vectors with missing values allowed? Default is `TRUE`.

- len:

  \[`integer(1)`\]  
  Exact expected length of `x`.

- ...:

  further parameters passed to
  [`checkmate::test_numeric()`](https://mllg.github.io/checkmate/reference/checkNumeric.html).

## Value

`TRUE` if `x` is a named vector of type numeric, otherwise `FALSE`.

## Note

This function is based on
[`checkmate::test_numeric()`](https://mllg.github.io/checkmate/reference/checkNumeric.html)
and
[`checkmate::test_names()`](https://mllg.github.io/checkmate/reference/checkNames.html)
functions.

## Examples

``` r
h_test_named_numeric(1:2, permutation.of = c("a", "b"))
#> [1] FALSE
h_test_named_numeric(c(a = 1, b = 2), permutation.of = c("a", "b"))
#> [1] TRUE
h_test_named_numeric(c(a = 1, b = 2), permutation.of = c("b", "a"))
#> [1] TRUE
```
