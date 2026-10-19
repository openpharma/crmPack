# Logical OR Operator for ArmCondition Objects

**\[experimental\]**

Combines two
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
objects with OR logic using the `|` operator. This creates an
[`ArmConditionAny`](https://docs.crmpack.org/reference/ArmConditionAny-class.md)
object.

## Usage

``` r
# S4 method for class 'ArmCondition,ArmCondition'
e1 | e2
```

## Arguments

- e1:

  (`ArmCondition`)\
  the first arm condition object.

- e2:

  (`ArmCondition`)\
  the second arm condition object.

## Value

An
[`ArmConditionAny`](https://docs.crmpack.org/reference/ArmConditionAny-class.md)
object combining `e1` and `e2`.
