# Internal Helper Functions for Validation of [`NextBest`](https://openpharma.github.io/crmPack/reference/NextBest-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`NextBest`](https://openpharma.github.io/crmPack/reference/NextBest-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_next_best_mtd(object)

v_next_best_ncrm(object)

v_next_best_ncrm_loss(object)

v_next_best_dual_endpoint(object)

v_next_best_min_dist(object)

v_next_best_ewoc(object)

v_next_best_inf_theory(object)

v_next_best_td(object)

v_next_best_td_samples(object)

v_next_best_max_gain_samples(object)

v_next_best_prob_mtd_lte(object)

v_next_best_prob_mtd_min_dist(object)

v_next_best_ordinal(object)
```

## Arguments

- object:

  (`NextBest`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_next_best_mtd()`: validates that the
  [`NextBestMTD`](https://openpharma.github.io/crmPack/reference/NextBestMTD-class.md)
  object contains valid `target` probability and `derive` function.

- `v_next_best_ncrm()`: validates that the
  [`NextBestNCRM`](https://openpharma.github.io/crmPack/reference/NextBestNCRM-class.md)
  object contains valid `target` probability, `overdose` and
  `max_overdose_prob` probability ranges.

- `v_next_best_ncrm_loss()`: validates that the
  [`NextBestNCRMLoss`](https://openpharma.github.io/crmPack/reference/NextBestNCRMLoss-class.md)
  object contains valid objects.

- `v_next_best_dual_endpoint()`: validates that the
  [`NextBestDualEndpoint`](https://openpharma.github.io/crmPack/reference/NextBestDualEndpoint-class.md)
  object contains valid probability objects.

- `v_next_best_min_dist()`: validates that the
  [`NextBestMinDist`](https://openpharma.github.io/crmPack/reference/NextBestMinDist-class.md)
  object contains valid `target` object.

- `v_next_best_ewoc()`: validates that the
  [`NextBestEWOC`](https://openpharma.github.io/crmPack/reference/NextBestEWOC-class.md)
  object contains valid `target`, `overdose` and `max_overdose_prob`
  parameters.

- `v_next_best_inf_theory()`: validates that the
  [`NextBestInfTheory`](https://openpharma.github.io/crmPack/reference/NextBestInfTheory-class.md)
  object contains valid `target` and `asymmetry` objects.

- `v_next_best_td()`: validates that the
  [`NextBestTD`](https://openpharma.github.io/crmPack/reference/NextBestTD-class.md)
  object contains valid `prob_target_drt` and `prob_target_eot`
  probabilities.

- `v_next_best_td_samples()`: validates that the
  [`NextBestTDsamples`](https://openpharma.github.io/crmPack/reference/NextBestTDsamples-class.md)
  object contains valid `derive` function.

- `v_next_best_max_gain_samples()`: validates that the
  [`NextBestMaxGainSamples`](https://openpharma.github.io/crmPack/reference/NextBestMaxGainSamples-class.md)
  object contains valid `derive` and `mg_derive` functions.

- `v_next_best_prob_mtd_lte()`: validates that the
  [`NextBestProbMTDLTE`](https://openpharma.github.io/crmPack/reference/NextBestProbMTDLTE-class.md)
  object contains valid `target` probability and `method` string value.

- `v_next_best_prob_mtd_min_dist()`: validates that the
  [`NextBestProbMTDMinDist`](https://openpharma.github.io/crmPack/reference/NextBestProbMTDMinDist-class.md)
  object contains valid `target` probability and `method` string value.

- `v_next_best_ordinal()`: validates that the
  [`NextBestOrdinal`](https://openpharma.github.io/crmPack/reference/NextBestOrdinal-class.md)
  object contains valid `grade` and standard `NextBest` rule.
