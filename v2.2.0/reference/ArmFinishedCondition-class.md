# `ArmFinishedCondition`

**\[experimental\]**

`ArmFinishedCondition` opens an arm when the named arm has finished dose
escalation.

## Usage

``` r
ArmFinishedCondition(arm_name)

.DefaultArmFinishedCondition()
```

## Arguments

- arm_name:

  (`string`)\
  see slot definition.

## Slots

- `arm_name`:

  (`string`)\
  the name of the arm that must have finished.

## Note

Typically, end users will not use the `.DefaultArmFinishedCondition()`
function.
