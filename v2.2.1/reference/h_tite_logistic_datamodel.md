# Build the common JAGS likelihood for TITE logistic CRM models.

Build the common JAGS likelihood for TITE logistic CRM models.

## Usage

``` r
h_tite_logistic_datamodel(dose_term)
```

## Arguments

- dose_term:

  (`name` or `call`)\
  expression defining the dose transformation used in the linear
  predictor.

## Value

A function implementing the shared zero-trick JAGS likelihood.
