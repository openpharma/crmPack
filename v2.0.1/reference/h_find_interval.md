# Find Interval Numbers or Indices and Return Custom Number For 0.

**\[stable\]**

A simple wrapper of
[`findInterval()`](https://rdrr.io/r/base/findInterval.html) function
that invokes
[`findInterval()`](https://rdrr.io/r/base/findInterval.html), takes its
output and replaces all the elements with \\0\\ value to a custom number
as specified in `replacement` argument.

## Usage

``` r
h_find_interval(..., replacement = -Inf)
```

## Arguments

- ...:

  Arguments passed on to
  [`base::findInterval`](https://rdrr.io/r/base/findInterval.html)

  `x`

  :   numeric.

  `vec`

  :   numeric, sorted (weakly) increasingly, of length `N`, say.

  `rightmost.closed`

  :   logical; if true, the rightmost interval, `vec[N-1] .. vec[N]` is
      treated as *closed*, see below.

  `all.inside`

  :   logical; if true, the returned indices are coerced into
      `1,...,N-1`, i.e., `0` is mapped to `1` and `N` to `N-1`.

  `left.open`

  :   logical; if true all the intervals are open at left and closed at
      right; in the formulas below, \\\le\\ should be swapped with
      \\\<\\ (and \\\>\\ with \\\ge\\), and `rightmost.closed` means
      ‘leftmost is closed’. This may be useful, e.g., in survival
      analysis computations.

  `checkSorted`

  :   logical indicating if `vec` should be checked, i.e.,
      [`is.unsorted`](https://rdrr.io/r/base/is.unsorted.html)`(vec)` is
      asserted to be false. Setting this to `FALSE` skips the check
      gaining speed, but may return nonsense results in case `vec` is
      not sorted.

  `checkNA`

  :   logical indicating if each `x[i]` should be checked as with
      [`is.na`](https://rdrr.io/r/base/NA.html)`(.)`. Setting this to
      `FALSE` in case of `NA`'s in `x[]` may result in platform
      dependent nonsense.

- replacement:

  (`number`)  
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
