# `PseudoSimulationsSummary`

**\[stable\]**

This class captures the summary of pseudo-models simulations output.
Note that objects should not be created by users, therefore no
initialization function is provided for this class.

## Usage

``` r
.DefaultPseudoSimulationsSummary()
```

## Slots

- `target_end_of_trial`:

  (`numeric`)  
  the target probability of DLE wanted at the end of a trial

- `target_dose_end_of_trial`:

  (`numeric`)  
  the dose level corresponds to the target probability of DLE wanted at
  the end of a trial, TDEOT

- `target_dose_end_of_trial_at_dose_grid`:

  (`numeric`)  
  the dose level at dose grid corresponds to the target probability of
  DLE wanted at the end of a trial

- `target_during_trial`:

  (`numeric`)  
  the target probability of DLE wanted during a trial

- `target_dose_during_trial`:

  (`numeric`)  
  the dose level corresponds to the target probability of DLE wanted
  during the trial. TDDT

- `target_dose_during_trial_at_dose_grid`:

  (`numeric`)  
  the dose level at dose grid corresponds to the target probability of
  DLE wanted during a trial

- `tdeot_summary`:

  (`table`)  
  the six-number table summary, include the lowest, the 25th percentile
  (lower quartile), the 50th percentile (median), the mean, the 75th
  percentile and the highest values of the final dose levels obtained
  corresponds to the target probability of DLE want at the end of a
  trial across all simulations

- `tddt_summary`:

  (`table`)  
  the six-number table summary, include the lowest, the 25th percentile
  (lower quartile), the 50th percentile (median), the mean, the 75th
  percentile and the highest values of the final dose levels obtained
  corresponds to the target probability of DLE want during a trial
  across all simulations

- `final_dose_rec_summary`:

  (`table`)  
  the six-number table summary, include the lowest, the 25th percentile
  (lower quartile), the 50th percentile (median), the mean, the 75th
  percentile and the highest values of the final optimal doses, which is
  either the TDEOT when only DLE response are incorporated into the
  escalation procedure or the minimum of the TDEOT and Gstar when DLE
  and efficacy responses are incorporated, across all simulations

- `ratio_tdeot_summary`:

  (`table`)  
  the six-number summary table of the final ratios of the upper to the
  lower 95% credibility intervals of the final TDEOTs across all
  simulations

- `final_ratio_summary`:

  (`table`)  
  the six-number summary table of the final ratios of the upper to the
  lower 95% credibility intervals of the final optimal doses across all
  simulations

- `nsim`:

  (`integer`)  
  number of simulations

- `prop_dle`:

  (`numeric`)  
  proportions of DLE in the trials

- `mean_tox_risk`:

  (`numeric`)  
  mean toxicity risks for the patients

- `dose_selected`:

  (`numeric`)  
  doses selected as MTD (target_dose_end_of_trial)

- `tox_at_doses_selected`:

  (`numeric`)  
  true toxicity at doses selected

- `prop_at_target_end_of_trial`:

  (`numeric`)  
  Proportion of trials selecting at the dose_grid closest below the MTD,
  the target_dose_end_of_trial

- `prop_at_target_during_trial`:

  (`numeric`)  
  Proportion of trials selecting at the dose_grid closest below the
  target_dose_during_trial

- `dose_most_selected`:

  (`numeric`)  
  dose most often selected as MTD

- `obs_tox_rate_at_dose_most_selected`:

  (`numeric`)  
  observed toxicity rate at dose most often selected

- `n_obs`:

  (`integer`)  
  number of patients overall

- `n_above_target_end_of_trial`:

  (`integer`)  
  number of patients treated above target_dose_end_of_trial

- `n_above_target_during_trial`:

  (`integer`)  
  number of patients treated above target_dose_during_trial

- `dose_grid`:

  (`numeric`)  
  the dose grid that has been used

- `fit_at_dose_most_selected`:

  (`numeric`)  
  fitted toxicity rate at dose most often selected

- `mean_fit`:

  (`list`)  
  list with the average, lower (2.5%) and upper (97.5%) quantiles of the
  mean fitted toxicity at each dose level

- `stop_report`:

  (`matrix`)  
  matrix of stopping rule outcomes

## Note

Typically, end users will not use the
`.DefaultPseudoSimulationsSummary()` function.
