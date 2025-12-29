# Recursively Apply a Function to a List

**\[experimental\]**

This helper function recursively iterates through a "list-like" object
and it checks whether an element is of a given class. If it so, then it
replaces that element by the result of an execution of a given function.
Otherwise, and if the element is of length greater than 1 (i.e. not a
scalar), it replaces that element by the result of `h_rapply()`,
recursively called for that element. In the remaining case, that is, the
element is not of a given class and is a scalar, then that element
remains unchanged.

## Usage

``` r
h_rapply(x, fun, classes, ...)
```

## Arguments

- x:

  (`any`)  
  "list-like" object for which subsetting operator
  [`[[`](https://rdrr.io/r/base/Extract.html) is defined.

- fun:

  (`function`)  
  a function of one "principal" argument, passing further arguments via
  `...`.

- classes:

  (`character`)  
  class names.

- ...:

  further arguments passed to function `fun`.

## Value

"list-like" object of similar structure as `x`.

## Note

This helper function is conceptually similar the same as
[`rapply()`](https://rdrr.io/r/base/rapply.html) function. However, it
differs from [`rapply()`](https://rdrr.io/r/base/rapply.html) in two
major ways. First, the `h_rapply()` is not limited to objects of type
`list` or `expression` only. It can be any "list-like" object of any
type for which subsetting operator
[`[[`](https://rdrr.io/r/base/Extract.html) is defined. This can be, for
example, an object of type `language`, often obtained from the
[`body()`](https://rdrr.io/r/base/body.html) function. The second
difference is that the flexibility of
[`rapply()`](https://rdrr.io/r/base/rapply.html) on how the result is
structured is not available with `h_rapply()` for the user. That is,
with `h_rapply()` each element of `x`, which has a class included in
`classes`, is replaced by the result of applying `fun` to the element.
This behavior corresponds to
[`rapply()`](https://rdrr.io/r/base/rapply.html) when invoked with fixed
`how = replace`. This function was primarily designed as a helper for
[`h_jags_write_model()`](https://openpharma.github.io/crmPack/reference/h_jags_write_model.md)
function.

## Examples

``` r
# Some model function.
my_model <- function() {
  alpha0 <- mean(1:10)
  alpha1 <- 600000
}

# Replace format of numbers using `formatC` function.
h_rapply(
  x = body(my_model),
  fun = formatC,
  classes = c("integer", "numeric"),
  digits = 3,
  format = "E"
)
#> {
#>     alpha0 <- mean("1.000E+00":"1.000E+01")
#>     alpha1 <- "6.000E+05"
#> }
```
