# `ArmConditionList`

**\[experimental\]**

`ArmConditionList` is a virtual class for combining multiple
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
objects using logical operators. It is used as a base class for
[`ArmConditionAll`](https://docs.crmpack.org/reference/ArmConditionAll-class.md)
and
[`ArmConditionAny`](https://docs.crmpack.org/reference/ArmConditionAny-class.md).

## Usage

``` r
ArmConditionList(...)

.DefaultArmConditionList()
```

## Arguments

- ...:

  (`ArmCondition`)\
  arm condition objects to combine.

## Slots

- `condition_list`:

  (`list`)\
  a list of
  [`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
  objects to be combined.

## Note

Typically, end users will not use the `.DefaultArmConditionList()`
function.

## See also

[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md),
[`ArmConditionAll`](https://docs.crmpack.org/reference/ArmConditionAll-class.md),
[`ArmConditionAny`](https://docs.crmpack.org/reference/ArmConditionAny-class.md).
