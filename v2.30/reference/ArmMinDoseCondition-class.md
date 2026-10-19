# `ArmMinDoseCondition`

**\[experimental\]**

`ArmMinDoseCondition` opens an arm when the named arm has enrolled at
least one patient at the specified minimum dose or higher.

## Usage

``` r
ArmMinDoseCondition(arm_name, min_dose)

.DefaultArmMinDoseCondition()
```

## Arguments

- arm_name:

  (`string`)\
  see slot definition.

- min_dose:

  (`numeric`)\
  see slot definition.

## Slots

- `arm_name`:

  (`string`)\
  the name of the arm whose data are checked.

- `min_dose`:

  (`numeric`)\
  the minimum dose that must have been reached. For combination arms,
  this can be a vector of minimum doses for each agent.

## Note

Typically, end users will not use the `.DefaultArmMinDoseCondition()`
function.
