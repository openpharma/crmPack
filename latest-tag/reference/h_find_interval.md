# Find Interval Numbers or Indices and Return Custom Number For 0.

**\[stable\]**

A simple wrapper of
[`base::findInterval()`](https://rdrr.io/r/base/findInterval.html)
function that invokes
[`base::findInterval()`](https://rdrr.io/r/base/findInterval.html),
takes its output and replaces all the elements with \\0\\ value to a
custom number as specified in `replacement` argument.

## Usage

``` r
h_find_interval(..., replacement = -Inf)
```

## Arguments

- ...:

  further arguments passed to
  [`base::findInterval()`](https://rdrr.io/r/base/findInterval.html)
  function.

- replacement:

  (`number`)\
  a custom number to be used as a replacement for \\0\\. Default to
  `-Inf`.

## Examples

``` r
h_find_interval(1, c(2, 4, 6))
#> [1] -Inf
h_find_interval(3, c(2, 4, 6))
#> [1] 1
h_find_interval(1, c(2, 4, 6), replacement = -1)
#> [1] -1
```
