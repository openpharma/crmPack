# Helper Function to Blind Plot Data

Helper Function to Blind Plot Data

## Usage

``` r
h_blind_plot_data(df, blind, has_placebo, pbo_dose)
```

## Arguments

- df:

  (`GeneralData`)  
  The data to be blinded

- blind:

  (`flag`)  
  Should the data be blinded?

- has_placebo:

  (`flag`)  
  Does the data contain a placebo dose?

- pbo_dose:

  (`positive_number`)  
  The dose to be taken as placebo. Ignored if `has_placebo` is `FALSE`

## Value

The blinded data
