# `PseudoDualSimulations`

**\[stable\]** This class conducts trial simulations for designs using
both the DLE and efficacy responses. It defines final values for
efficacy fit and DLE, estimates of Gstar, optimal dose and sigma2.

## Usage

``` r
PseudoDualSimulations(
  fit_eff,
  final_gstar_estimates,
  final_gstar_at_dose_grid,
  final_gstar_cis,
  final_gstar_ratios,
  final_optimal_dose,
  final_optimal_dose_at_dose_grid,
  sigma2_est,
  ...
)

.DefaultPseudoDualSimulations()
```

## Arguments

- fit_eff:

  (`list`)  
  see slot definition.

- final_gstar_estimates:

  (`numeric`)  
  see slot definition.

- final_gstar_at_dose_grid:

  (`numeric`)  
  see slot definition.

- final_gstar_cis:

  (`list`)  
  see slot definition.

- final_gstar_ratios:

  (`numeric`)  
  see slot definition.

- final_optimal_dose:

  (`numeric`)  
  see slot definition.

- final_optimal_dose_at_dose_grid:

  (`numeric`)  
  see slot definition.

- sigma2_est:

  (`numeric`)  
  see slot definition.

- ...:

  additional parameters from
  [`PseudoSimulations`](https://openpharma.github.io/crmPack/reference/PseudoSimulations-class.md)

## Slots

- `fit_eff`:

  (`list`)  
  final values of efficacy fit.

- `final_gstar_estimates`:

  (`numeric`)  
  final Gstar estimates.

- `final_gstar_at_dose_grid`:

  (`numeric`)  
  final Gstar estimates at dose grid.

- `final_gstar_cis`:

  (`list`)  
  list of 95% confidence interval for Gstar estimates.

- `final_gstar_ratios`:

  (`numeric`)  
  ratios of confidence intervals for Gstar estimates.

- `final_optimal_dose`:

  (`numeric`)  
  final optimal dose.

- `final_optimal_dose_at_dose_grid`:

  (`numeric`)  
  final optimal dose at dose grid.

- `sigma2_est`:

  (`numeric`)  
  final sigma2 estimates.

## Note

Do not use the `.DefaultPseudoDualSimulations()` function.
