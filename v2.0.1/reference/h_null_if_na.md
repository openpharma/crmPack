# Getting `NULL` for `NA`

**\[stable\]**

A simple helper function that replaces `NA` object by `NULL` object.

## Usage

``` r
h_null_if_na(x)
```

## Arguments

- x:

  (`any`)  
  atomic object of length `1`. For the definition of "atomic", see
  [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html).

## Value

`NULL` if `x` is `NA`, otherwise, `x`.

## Examples

``` r
h_null_if_na(NA)
#> NULL
```
