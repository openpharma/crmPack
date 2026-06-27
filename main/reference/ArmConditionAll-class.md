# `ArmConditionAll`

**\[experimental\]**

`ArmConditionAll` combines multiple
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
objects using AND logic. An arm opens only if ALL conditions in the list
are satisfied.

## Usage

``` r
ArmConditionAll(...)

.DefaultArmConditionAll()
```

## Arguments

- ...:

  (`ArmCondition`)\
  arm condition objects to combine with AND logic.

## Slots

- `condition_list`:

  (`list`)\
  a list of
  [`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
  objects to be combined with AND logic.

## Note

Typically, end users will not use the `.DefaultArmConditionAll()`
function.

## See also

[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md),
[`ArmConditionAny`](https://docs.crmpack.org/reference/ArmConditionAny-class.md),
[`ArmConditionList`](https://docs.crmpack.org/reference/ArmConditionList-class.md).
