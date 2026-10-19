# Logical AND Operator for ArmCondition Objects

**\[experimental\]**

Combines two
[`ArmCondition`](https://docs.crmpack.org/reference/ArmCondition-class.md)
objects with AND logic using the `&` operator. This creates an
[`ArmConditionAll`](https://docs.crmpack.org/reference/ArmConditionAll-class.md)
object.

## Usage

``` r
# S4 method for class 'ArmCondition,ArmCondition'
e1 & e2
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
[`ArmConditionAll`](https://docs.crmpack.org/reference/ArmConditionAll-class.md)
object combining `e1` and `e2`.
