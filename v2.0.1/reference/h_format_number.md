# Conditional Formatting Using C-style Formats

**\[experimental\]**

This helper function conditionally formats a number with
[`formatC()`](https://rdrr.io/r/base/formatc.html) function using `"E"`
format and specific number of digits as given by the user. A number is
formatted if and only if its absolute value is less than `0.001` or
greater than `10000`. Otherwise, the number is not formatted.
Additionally, custom prefix or suffix can be appended to character
string with formatted number, so that the changes are marked.

## Usage

``` r
h_format_number(x, digits = 5, prefix = "", suffix = "")
```

## Arguments

- x:

  (`number`)  
  a number to be formatted.

- digits:

  (`function`)  
  the desired number of significant digits.

- prefix:

  (`string`)  
  a prefix to be added in front of the formatted number.

- suffix:

  (`string`)  
  a suffix to be appended after the formatted number.

## Value

Either formatted `x` as `string` or unchanged `x` if the formatting
condition is not met.

## Note

This function was primarily designed as a helper for
[`h_jags_write_model()`](https://openpharma.github.io/crmPack/reference/h_jags_write_model.md)
function.

## Examples

``` r
h_format_number(50000)
#> [1] "5.00000E+04"
h_format_number(50000, prefix = "P", suffix = "S")
#> [1] "P5.00000E+04S"
```
