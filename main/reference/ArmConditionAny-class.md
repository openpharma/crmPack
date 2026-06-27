# `ArmConditionAny`

**\[experimental\]**

`ArmConditionAny` combines multiple
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
objects using OR logic. An arm opens if ANY condition in the list is
satisfied.

## Usage

``` r
ArmConditionAny(...)

.DefaultArmConditionAny()
```

## Arguments

- ...:

  (`ArmCondition`)\
  arm condition objects to combine with OR logic.

## Slots

- `condition_list`:

  (`list`)\
  a list of
  [`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
  objects to be combined with OR logic.

## Note

Typically, end users will not use the `.DefaultArmConditionAny()`
function.

## See also

[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md),
[`ArmConditionAll`](https://docs.crmpack.org/reference/ArmConditionAll-class.md),
[`ArmConditionList`](https://docs.crmpack.org/reference/ArmConditionList-class.md).
