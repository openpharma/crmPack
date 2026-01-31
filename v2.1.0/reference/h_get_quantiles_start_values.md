# Get Starting Values for Quantiles Optimization

Get Starting Values for Quantiles Optimization

## Usage

``` r
h_get_quantiles_start_values(parstart, median, dosegrid, refDose, logNormal)
```

## Arguments

- parstart:

  (`numeric` or `NULL`)  
  starting parameter values.

- median:

  (`numeric`)  
  median values.

- dosegrid:

  (`numeric`)  
  dose grid.

- refDose:

  (`number`)  
  reference dose.

- logNormal:

  (`flag`)  
  use log-normal prior?

## Value

Numeric vector of starting values.
