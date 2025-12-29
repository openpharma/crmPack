# Internal Helper Functions for Validation of [`PseudoSimulations`](https://openpharma.github.io/crmPack/reference/PseudoSimulations-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`PseudoSimulations`](https://openpharma.github.io/crmPack/reference/PseudoSimulations-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_pseudo_simulations(object)

v_pseudo_dual_simulations(object)

v_pseudo_dual_flex_simulations(object)
```

## Arguments

- object:

  (`PseudoSimulations`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_pseudo_simulations()`: validates that the
  [`PseudoSimulations`](https://openpharma.github.io/crmPack/reference/PseudoSimulations-class.md)
  object contains valid `fit`, `FinalTDtargetEndOfTrialEstimates` ,
  `FinalTDtargetDuringTrialAtDoseGrid`,`FinalTDtargetEndOfTrialAtDoseGrid`
  , `FinalTDEOTCIs`, `FinalTDEOTRatios`, `FinalCIs`, `FinalRatios`,
  object and valid `stopReasons` simulations.

- `v_pseudo_dual_simulations()`: validates that the
  [`PseudoDualSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualSimulations-class.md)
  object contains valid `fit_eff`, `final_gstar_estimates` ,
  `final_gstar_at_dose_grid`, `final_gstar_cis` , `final_gstar_ratios`,
  `final_optimal_dose`, `final_optimal_dose_at_dose_grid` object and
  valid `sigma2_est` simulations.

- `v_pseudo_dual_flex_simulations()`: validates that the
  [`PseudoDualFlexiSimulations`](https://openpharma.github.io/crmPack/reference/PseudoDualFlexiSimulations-class.md)
  object contains valid `sigma2_beta_w_est` vector of the final
  posterior mean sigma2betaW estimates.`FinalGstarEstimates` ,
  `FinalGstarAtDoseGrid`,
