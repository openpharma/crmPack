# Open a hierarchical design arm?

**\[experimental\]**

## Usage

``` r
openArm(condition, data, ...)

# S4 method for class 'NoArmCondition'
openArm(condition, data, ...)

# S4 method for class 'ArmFinishedCondition'
openArm(condition, data, finished_arms, ...)

# S4 method for class 'ArmMinDoseCondition'
openArm(condition, data, ...)

# S4 method for class 'ArmConditionList'
openArm(condition, data, summary_fun, ...)

# S4 method for class 'ArmConditionAll'
openArm(condition, data, ...)

# S4 method for class 'ArmConditionAny'
openArm(condition, data, ...)
```

## Arguments

- condition:

  (`ArmCondition`)\
  opening condition to be applied.

- data:

  (`HierarchicalData`)\
  current hierarchical trial data.

- ...:

  further arguments passed to condition-specific methods.

- finished_arms:

  (`logical`)\
  named vector indicating which arms have finished dose escalation.

- summary_fun:

  (`function`)\
  to apply to the list of results (e.g. `all` or `any`). Only used for
  `ArmConditionList` and its subclasses.

## Value

`TRUE` if this arm can be opened, `FALSE` otherwise.

## Functions

- `openArm(NoArmCondition)`: method for `NoArmCondition` class, which
  always opens the arm.

- `openArm(ArmFinishedCondition)`: method for `ArmFinishedCondition`
  class.

- `openArm(ArmMinDoseCondition)`: method for `ArmMinDoseCondition`
  class.

- `openArm(ArmConditionList)`: method for `ArmConditionList` class.

- `openArm(ArmConditionAll)`: method for `ArmConditionAll` class.
  Returns `TRUE` if ALL arm opening criteria are satisfied.

- `openArm(ArmConditionAny)`: method for `ArmConditionAny` class.
  Returns `TRUE` if ANY arm opening criterion is satisfied.
