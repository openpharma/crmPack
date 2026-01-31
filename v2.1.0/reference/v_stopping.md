# Internal Helper Functions for Validation of [`Stopping`](https://openpharma.github.io/crmPack/reference/Stopping-class.md) Objects

**\[stable\]**

These functions are only used internally to validate the format of an
input
[`Stopping`](https://openpharma.github.io/crmPack/reference/Stopping-class.md)
or inherited classes and therefore not exported.

## Usage

``` r
v_stopping_cohorts_near_dose(object)

v_stopping_patients_near_dose(object)

v_stopping_min_cohorts(object)

v_stopping_min_patients(object)

v_stopping_target_prob(object)

v_stopping_mtd_distribution(object)

v_stopping_mtd_cv(object)

v_stopping_target_biomarker(object)

v_stopping_list(object)

v_stopping_all(object)

v_stopping_tdci_ratio(object)
```

## Arguments

- object:

  (`Stopping`)  
  object to validate.

## Value

A `character` vector with the validation failure messages, or `TRUE` in
case validation passes.

## Functions

- `v_stopping_cohorts_near_dose()`: validates that the
  [`StoppingCohortsNearDose`](https://openpharma.github.io/crmPack/reference/StoppingCohortsNearDose-class.md)
  object contains valid `nCohorts` and `percentage` parameters.

- `v_stopping_patients_near_dose()`: validates that the
  [`StoppingPatientsNearDose`](https://openpharma.github.io/crmPack/reference/StoppingPatientsNearDose-class.md)
  object contains valid `nPatients` and `percentage` parameters.

- `v_stopping_min_cohorts()`: validates that the
  [`StoppingMinCohorts`](https://openpharma.github.io/crmPack/reference/StoppingMinCohorts-class.md)
  object contains valid `nCohorts` parameter.

- `v_stopping_min_patients()`: validates that the
  [`StoppingMinPatients`](https://openpharma.github.io/crmPack/reference/StoppingMinPatients-class.md)
  object contains valid `nPatients` parameter.

- `v_stopping_target_prob()`: validates that the
  [`StoppingTargetProb`](https://openpharma.github.io/crmPack/reference/StoppingTargetProb-class.md)
  object contains valid `target` and `prob` parameters.

- `v_stopping_mtd_distribution()`: validates that the
  [`StoppingMTDdistribution`](https://openpharma.github.io/crmPack/reference/StoppingMTDdistribution-class.md)
  object contains valid `target`, `thresh` and `prob` parameters.

- `v_stopping_mtd_cv()`: validates that the
  [`StoppingMTDCV`](https://openpharma.github.io/crmPack/reference/StoppingMTDCV-class.md)
  object contains valid probability target and percentage threshold.

- `v_stopping_target_biomarker()`: validates that the
  [`StoppingTargetBiomarker`](https://openpharma.github.io/crmPack/reference/StoppingTargetBiomarker-class.md)
  object contains valid `target`, `is_relative` and `prob`slots.

- `v_stopping_list()`: validates that the
  [`StoppingList`](https://openpharma.github.io/crmPack/reference/StoppingList-class.md)
  object contains valid `stop_list`, `summary` slots.

- `v_stopping_all()`: validates that the
  [`StoppingAll`](https://openpharma.github.io/crmPack/reference/StoppingAll-class.md)
  object contains valid `stop_list` slot.

- `v_stopping_tdci_ratio()`: validates that the
  [`StoppingTDCIRatio`](https://openpharma.github.io/crmPack/reference/StoppingTDCIRatio-class.md)
  object contains valid `target_ratio` and `prob_target` slots.
