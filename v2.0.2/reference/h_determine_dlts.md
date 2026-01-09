# Helper function to determine the dlts including first separate and placebo condition

Helper function to determine the dlts including first separate and
placebo condition

## Usage

``` r
h_determine_dlts(
  data,
  dose,
  prob,
  prob_placebo,
  cohort_size,
  cohort_size_placebo,
  dose_grid,
  first_separate
)
```

## Arguments

- data:

  (`Data`)  
  what data to start from.

- dose:

  (`number`)  
  current dose.

- prob:

  (`function`)  
  defines the true probability for a DLT at a dose.

- prob_placebo:

  (`function`)  
  defines the true probability for a DLT at a placebo condition.

- cohort_size:

  (`number`)  
  the cohort size to use.

- cohort_size_placebo:

  (`number`)  
  the cohort size to use for placebo condition.

- dose_grid:

  (`numeric`)  
  the dose_grid as specified by the user.

- first_separate:

  (`flag`)  
  whether the first patient is enrolled separately.

## Value

updated data object
