# Check which elements are in a given range

**\[stable\]**

A simple helper function that tests whether elements of a given vector
or matrix are within specified interval.

## Usage

``` r
h_in_range(x, range = c(0, 1), bounds_closed = TRUE)
```

## Arguments

- x:

  (`numeric`)  
  vector or matrix with elements to test.

- range:

  (`numeric`)  
  an interval, i.e. sorted two-elements vector.

- bounds_closed:

  (`logical`)  
  should bounds in the `range` be treated as closed? This can be a
  scalar or vector of length two. If it is a scalar, then its value
  applies to lower bound `range[1]` and upper bound `range[2]`. If this
  is a vector with two flags, the first flag corresponds to the lower
  bound only, and the second to the upper bound only.

## Value

A logical vector or matrix of length equal to the length of `x`, that
for every element of `x`, indicates whether a given element of `x` is in
the `range`.

## Examples

``` r
x <- 1:4
h_in_range(x, range = c(1, 3))
#> [1]  TRUE  TRUE  TRUE FALSE
h_in_range(x, range = c(1, 3), bounds_closed = FALSE)
#> [1] FALSE  TRUE FALSE FALSE
h_in_range(x, range = c(1, 3), bounds_closed = c(FALSE, TRUE))
#> [1] FALSE  TRUE  TRUE FALSE
mat <- matrix(c(2, 5, 3, 10, 4, 9, 1, 8, 7), nrow = 3)
h_in_range(mat, range = c(1, 5))
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE  TRUE
#> [2,] TRUE  TRUE FALSE
#> [3,] TRUE FALSE FALSE
```
