# Check if All Arguments Are Equal

**\[experimental\]** Elements of `...` must be numeric vectors or
scalars.

This function performs an element-by-element comparison of the first
object provided in `...` with every other object in `...` and returns
`TRUE` if all comparisons are equal within a given tolerance and `FALSE`
otherwise.

**\[experimental\]** Elements of `...` must be numeric vectors or
scalars.

This function performs an element-by-element comparison of the first
object provided in `...` with every other object in `...` and throws an
error if they are not.

## Usage

``` r
check_equal(..., tol = sqrt(.Machine$double.eps))

assert_equal(
  ...,
  tol = sqrt(.Machine$double.eps),
  .var.name = vname(x),
  add = NULL
)
```

## Arguments

- ...:

  (`numeric`)  
  vectors to be compared

- tol:

  (`numeric`)  
  the maximum difference to be tolerated when judging equality

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

`TRUE` if all element-by-element differences are less than `tolerance`
in magnitude, `FALSE` otherwise.

`list(...)`, invisibly.

## Note

If there are any missing or infinite values in `...`, this function
returns `FALSE`, regardless of the values of other elements in `...`.

If elements in `...` are not all of the same length, `FALSE` is
returned.

If there are any missing or infinite values in `...`, this function
throws an error, regardless of the values of other elements in `...`.

If elements in `...` are not all of the same length, an error is thrown.

## See also

[`assertions`](https://openpharma.github.io/crmPack/reference/assertions.md)
for more details.

[`assertions`](https://openpharma.github.io/crmPack/reference/assertions.md)
for more details.

## Examples

``` r
check_equal(1:2, 1:2) # TRUE
#> [1] TRUE
check_equal(1:2, 2:3) # "Not all equal"
#> [1] "Not all equal"
check_equal(Inf, Inf) # "Not all equal"
#> [1] "Not all entries finite"
check_equal(0.01, 0.02) # "Not all equal"
#> [1] "Not all equal"
check_equal(0.01, 0.02, tol = 0.05) # TRUE
#> [1] TRUE
check_equal(1, c(1, 1)) # "Not all equal"
#> [1] "Not all of same length"
assert_equal(1:2, 1:2) # no error
assert_equal(0.01, 0.02, tol = 0.05) # no error
```
