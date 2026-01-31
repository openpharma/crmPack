# Helper Function for Value Matching with Tolerance

**\[stable\]**

This is a modified version of
[`match()`](https://rdrr.io/r/base/match.html) that supports tolerance.

## Usage

``` r
match_within_tolerance(x, table, tolerance = 1e-10)
```

## Arguments

- x:

  (`numeric`)  
  the values to be matched.

- table:

  (`numeric`)  
  the values to be matched against.

- tolerance:

  (`number`)  
  the numerical tolerance for comparison.

## Value

An integer vector of the same length as `x` giving the position in
`table` of the first match, or an empty integer vector if `table` is
empty. `NA` is returned for values in `x` that have no match.

## Examples

``` r
match_within_tolerance(c(0.1, 0.2, 0.3), c(0.10000001, 0.5, 0.3))
#> [1] NA NA  3
match_within_tolerance(1.5, numeric(0))
#> integer(0)
```
