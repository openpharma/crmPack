# Apply a Function to Subsets of Data Frame.

**\[experimental\]**

`dapply` splits the data `df` into the subsets defined by `f`, and
applies function `FUN` to each of the subset. All the results are
row-binded and returned as `data.frame` object.

## Usage

``` r
dapply(df, f, FUN, ...)
```

## Arguments

- df:

  (`data frame`)  
  data set to be divided into groups.

- f:

  (`factor` or `formula` or `list`)  
  a factor in the sense that `as.factor(f)` defines the grouping, or a
  `list` of such factors in which case their interaction is used for the
  grouping. `f` can also be a formula of the form `~ g1 + ... + gk` to
  split by the interaction of the variables `g1, ..., gk`. This
  parameter is passed directly into
  [`split()`](https://rdrr.io/r/base/split.html) function.

- FUN:

  (`function`)  
  the function to be applied to each subset of `df` defined by `f`.

- ...:

  parameters passed to [`lapply()`](https://rdrr.io/r/base/lapply.html),
  which is used when applying a function `FUN` over groups defined by
  `f`.

## Value

The [`data.frame`](https://rdrr.io/r/base/data.frame.html) object with
results from `FUN`.

## Examples

``` r
df <- data.frame(
  dose = c(0.1, 6, 6, 5, 0.1, 5, 6, 6),
  cohort = c("B", "B", "B", "A", "A", "A", "B", "B")
)

dapply(
  df,
  f = ~cohort,
  FUN = function(coh) {
    data.frame(my_cohort = coh$cohort[1], my_max = max(coh$dose))
  }
)
#>   my_cohort my_max
#> 1         A      5
#> 2         B      6

dapply(
  df,
  f = ~cohort,
  FUN = function(coh) {
    coh$dose <- sort(coh$dose, decreasing = TRUE)
    coh
  }
)
#>   dose cohort
#> 1  5.0      A
#> 2  5.0      A
#> 3  0.1      A
#> 4  6.0      B
#> 5  6.0      B
#> 6  6.0      B
#> 7  6.0      B
#> 8  0.1      B
```
