# `Stopping`

**\[stable\]**

`Stopping` is a class for stopping rules.

## Slots

- `report_label`:

  (`string`)  
  a label for the stopping report. The meaning of this parameter is
  twofold. If it is equal to `NA_character_` (default), the
  `report_label` will not be used in the report at all. Otherwise, if it
  is specified as an empty character (i.e. `character(0)`) in a user
  constructor, then a default, class-specific label will be created for
  this slot. Finally, for the remaining cases, a user can provide a
  custom label.

## See also

[`StoppingList`](https://openpharma.github.io/crmPack/reference/StoppingList-class.md),
[`StoppingCohortsNearDose`](https://openpharma.github.io/crmPack/reference/StoppingCohortsNearDose-class.md),
[`StoppingPatientsNearDose`](https://openpharma.github.io/crmPack/reference/StoppingPatientsNearDose-class.md),
[`StoppingMinCohorts`](https://openpharma.github.io/crmPack/reference/StoppingMinCohorts-class.md),
[`StoppingMinPatients`](https://openpharma.github.io/crmPack/reference/StoppingMinPatients-class.md),
[`StoppingTargetProb`](https://openpharma.github.io/crmPack/reference/StoppingTargetProb-class.md),
[`StoppingMTDdistribution`](https://openpharma.github.io/crmPack/reference/StoppingMTDdistribution-class.md),
[`StoppingTargetBiomarker`](https://openpharma.github.io/crmPack/reference/StoppingTargetBiomarker-class.md),
[`StoppingHighestDose`](https://openpharma.github.io/crmPack/reference/StoppingHighestDose-class.md)
[`StoppingMTDCV`](https://openpharma.github.io/crmPack/reference/StoppingMTDCV-class.md),
[`StoppingLowestDoseHSRBeta`](https://openpharma.github.io/crmPack/reference/StoppingLowestDoseHSRBeta-class.md),
[`StoppingSpecificDose`](https://openpharma.github.io/crmPack/reference/StoppingSpecificDose-class.md).
