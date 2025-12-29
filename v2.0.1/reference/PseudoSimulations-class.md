# `PseudoSimulations`

**\[stable\]** This class captures trial simulations from designs using
pseudo model. It has additional slots `fit` and `stop_reasons` compared
to the general class
[`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md).

## Usage

``` r
PseudoSimulations(
  fit,
  final_td_target_during_trial_estimates,
  final_td_target_end_of_trial_estimates,
  final_td_target_during_trial_at_dose_grid,
  final_td_target_end_of_trial_at_dose_grid,
  final_tdeot_cis,
  final_tdeot_ratios,
  final_cis,
  final_ratios,
  stop_report,
  stop_reasons,
  ...
)

.DefaultPseudoSimulations()
```

## Arguments

- fit:

  (`list`)  
  see slot definition.

- final_td_target_during_trial_estimates:

  (`numeric`)  
  see slot definition.

- final_td_target_end_of_trial_estimates:

  (`numeric`)  
  see slot definition.

- final_td_target_during_trial_at_dose_grid:

  (`numeric`)  
  see slot definition.

- final_td_target_end_of_trial_at_dose_grid:

  (`numeric`)  
  see slot definition.

- final_tdeot_cis:

  (`list`)  
  see slot definition.

- final_tdeot_ratios:

  (`numeric`)  
  see slot definition.

- final_cis:

  (`list`)  
  see slot definition.

- final_ratios:

  (`numeric`)  
  see slot definition.

- stop_report:

  see `PseudoSimulations`

- stop_reasons:

  (`list`)  
  see slot definition.

- ...:

  additional parameters from
  [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)

## Slots

- `fit`:

  (`list`)  
  final fit values.

- `final_td_target_during_trial_estimates`:

  (`numeric`)  
  final estimates of the `td_target_during_trial`.

- `final_td_target_end_of_trial_estimates`:

  (`numeric`)  
  final estimates of the `td_target_end_of_trial`.

- `final_td_target_during_trial_at_dose_grid`:

  (`numeric`)  
  dose levels at dose grid closest below the final
  `td_target_during_trial` estimates.

- `final_td_target_end_of_trial_at_dose_grid`:

  (`numeric`)  
  dose levels at dose grid closest below the final
  `td_target_end_of_trial` estimates.

- `final_tdeot_cis`:

  (`list`)  
  95% credibility intervals of the final estimates for
  `td_target_end_of_trial`.

- `final_tdeot_ratios`:

  (`numeric`)  
  ratio of the upper to the lower 95% credibility intervals for
  `td_target_end_of_trial`.

- `final_cis`:

  (`list`)  
  final 95% credibility intervals for `td_target_end_of_trial`
  estimates.

- `final_ratios`:

  (`numeric`)  
  final ratios of the upper to the lower 95% credibility interval for
  `td_target_end_of_trial`.

- `stop_report`:

  (`matrix`)  
  outcomes of stopping rules.

- `stop_reasons`:

  (`list`)  
  reasons for stopping each simulation run.

## Note

Typically, end users will not use the `.DefaultPseudoSimulations()`
function.
