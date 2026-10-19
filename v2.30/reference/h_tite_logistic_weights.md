# Calculate weights for TITE logistic CRM models.

Calculate weights for TITE logistic CRM models.

## Usage

``` r
h_tite_logistic_weights(u, Tmax, y, weight_method)
```

## Arguments

- u:

  (`numeric`)\
  follow-up times.

- Tmax:

  (`number`)\
  DLT assessment window.

- y:

  (`integer`)\
  DLT indicators.

- weight_method:

  (`string`)\
  either `"linear"` or `"adaptive"`.

## Value

A numeric vector of observation weights.
