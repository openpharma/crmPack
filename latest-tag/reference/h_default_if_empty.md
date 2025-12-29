# Getting the default value for an empty object

**\[stable\]**

A simple helper function that sets a default value for an empty or
missing object, that is an object for which
[`length()`](https://rdrr.io/r/base/length.html) function returns `0L`
or it has length 1 and [`is.na()`](https://rdrr.io/r/base/NA.html)
returns `TRUE`.

## Usage

``` r
h_default_if_empty(x, default)
```

## Arguments

- x:

  (`any`)  
  an object to handle. It can be any object for which
  [`length()`](https://rdrr.io/r/base/length.html) function is defined.

- default:

  (`any`)  
  a default value for `x` object.

## Examples

``` r
h_default_if_empty(character(0), default = "default label")
#> [1] "default label"
h_default_if_empty("custom label", default = "default label")
#> [1] "custom label"
h_default_if_empty(NA, default = "default label")
#> [1] "default label"
```
