# Preparing Cohort Lines for Data Plot

**\[experimental\]**

This helper function prepares a `ggplot` geom with reference lines
separating different cohorts on the plot of `Data` class object. Lines
are either vertical or horizontal of green color and longdash type.

## Usage

``` r
h_plot_data_cohort_lines(cohort, placebo, vertical = TRUE)
```

## Arguments

- cohort:

  (`integer`)  
  the cohort indices.

- placebo:

  (`flag`)  
  is placebo included in the doses? If it so, this function returns
  `NULL` object as in this case all doses in a given cohort are equal
  and there is no need to separate them.

- vertical:

  (`flag`)  
  should the line be vertical? Otherwise it is horizontal.

## Details

The geom object is returned if and only if `placebo` is equal to `TRUE`
and there are more than one unique values in `cohort`. Otherwise, this
function returns `NULL` object.
