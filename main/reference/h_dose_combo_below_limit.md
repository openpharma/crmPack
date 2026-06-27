# Check if the Doses in a Dose Matrix are Below the Dose Limit.

**\[experimental\]**

Helper function that checks if the doses in a dose matrix are below the
dose limit.

## Usage

``` r
h_dose_combo_below_limit(dose_matrix, dose_limit)
```

## Arguments

- dose_matrix:

  (`matrix`)\
  a matrix with the doses of the two compounds (columns).

- dose_limit:

  (`matrix`)\
  a matrix with two columns: the first column contains the doses for
  compound 1 and the second column contains the corresponding dose
  limits for compound 2.

## Value

A logical vector indicating whether the doses in each row of the dose
matrix are below the corresponding dose limits.
