# Target Function for Quantiles Optimization

Target Function for Quantiles Optimization

## Usage

``` r
h_quantiles_target_function(
  dosegrid,
  refDose,
  lower,
  median,
  upper,
  level,
  logNormal,
  seed
)
```

## Arguments

- dosegrid:

  (`numeric`)  
  dose grid.

- refDose:

  (`number`)  
  reference dose.

- lower:

  (`numeric`)  
  lower quantiles.

- median:

  (`numeric`)  
  median quantiles.

- upper:

  (`numeric`)  
  upper quantiles.

- level:

  (`number`)  
  credible level.

- logNormal:

  (`flag`)  
  use log-normal prior?

- seed:

  (`count`)  
  random seed.

## Value

Function that computes target value for optimization.
