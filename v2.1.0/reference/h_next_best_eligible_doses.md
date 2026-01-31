# Get Eligible Doses from the Dose Grid.

**\[experimental\]**

Helper function that gets the eligible doses from the dose grid. The
eligible doses are the doses which do not exceed a given `doselimit`.
For placebo design, if safety allows (i.e. if there is at least one
non-placebo dose which does not exceed the dose limit), the placebo dose
is then excluded from the eligible doses.

## Usage

``` r
h_next_best_eligible_doses(dose_grid, doselimit, placebo, levels = FALSE)
```

## Arguments

- dose_grid:

  (`numeric`)  
  all possible doses.

- doselimit:

  (`number`)  
  the maximum allowed next dose.

- placebo:

  (`flag`)  
  if `TRUE` the first dose level in the `dose_grid` is considered as
  placebo.

- levels:

  (`flag`)  
  if `TRUE` the levels of eligible doses are returned, otherwise, the
  doses (default).

## Value

A numeric vector with eligible doses or eligible dose levels if `levels`
flag is `TRUE`.

## Examples

``` r
dose_grid <- c(0.001, seq(25, 200, 25))
h_next_best_eligible_doses(dose_grid, 79, TRUE)
#> [1] 25 50 75
h_next_best_eligible_doses(dose_grid, 24, TRUE)
#> [1] 0.001
```
