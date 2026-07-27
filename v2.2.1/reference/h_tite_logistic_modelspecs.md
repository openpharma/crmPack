# Build the shared JAGS data list for TITE logistic CRM models.

Build the shared JAGS data list for TITE logistic CRM models.

## Usage

``` r
h_tite_logistic_modelspecs(model, weight_method, nObs, u, Tmax, y, from_prior)
```

## Arguments

- model:

  (`GeneralModel`)\
  a logistic TITE model with `params` and `ref_dose` slots.

- weight_method:

  (`string`)\
  either `"linear"` or `"adaptive"`.

- nObs:

  (`integer`)\
  number of observations.

- u:

  (`numeric`)\
  follow-up times.

- Tmax:

  (`number`)\
  DLT assessment window.

- y:

  (`integer`)\
  DLT indicators.

- from_prior:

  (`flag`)\
  whether prior-only specifications are requested.

## Value

A named list passed to JAGS.
