# Updating `HierarchicalData` Objects

**\[experimental\]**

A method that updates one arm in a
[`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
object with new data.

## Usage

``` r
# S4 method for class 'HierarchicalData'
update(object, arm, ..., check = TRUE)
```

## Arguments

- object:

  (`HierarchicalData`)\
  object you want to update.

- arm:

  (`string`)\
  name of the arm to update.

- ...:

  arguments passed to the selected arm's `update` method.

- check:

  (`flag`)\
  whether the validation of the updated object should be conducted.

## Value

The new, updated
[`HierarchicalData`](https://docs.crmpack.org/reference/HierarchicalData-class.md)
object.
