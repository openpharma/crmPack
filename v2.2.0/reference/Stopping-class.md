# `Stopping`

**\[stable\]**

`Stopping` is a class for stopping rules.

## Slots

- `report_label`:

  (`string`)\
  a label for the stopping report. The meaning of this parameter is
  twofold. If it is equal to `NA_character_` (default), the
  `report_label` will not be used in the report at all. Otherwise, if it
  is specified as an empty character (i.e. `character(0)`) in a user
  constructor, then a default, class-specific label will be created for
  this slot. Finally, for the remaining cases, a user can provide a
  custom label.

## See also

[`StoppingList`](https://docs.crmpack.org/reference/StoppingList-class.md),
[`StoppingCohortsNearDose`](https://docs.crmpack.org/reference/StoppingCohortsNearDose-class.md),
[`StoppingPatientsNearDose`](https://docs.crmpack.org/reference/StoppingPatientsNearDose-class.md),
[`StoppingMinCohorts`](https://docs.crmpack.org/reference/StoppingMinCohorts-class.md),
[`StoppingMinPatients`](https://docs.crmpack.org/reference/StoppingMinPatients-class.md),
[`StoppingTargetProb`](https://docs.crmpack.org/reference/StoppingTargetProb-class.md),
[`StoppingMTDdistribution`](https://docs.crmpack.org/reference/StoppingMTDdistribution-class.md),
[`StoppingTargetBiomarker`](https://docs.crmpack.org/reference/StoppingTargetBiomarker-class.md),
[`StoppingHighestDose`](https://docs.crmpack.org/reference/StoppingHighestDose-class.md)
[`StoppingMTDCV`](https://docs.crmpack.org/reference/StoppingMTDCV-class.md),
[`StoppingLowestDoseHSRBeta`](https://docs.crmpack.org/reference/StoppingLowestDoseHSRBeta-class.md),
[`StoppingSpecificDose`](https://docs.crmpack.org/reference/StoppingSpecificDose-class.md).
