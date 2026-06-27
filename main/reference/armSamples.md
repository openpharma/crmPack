# Extract One Arm's Posterior Draws from a `HierarchicalSamples` Object

**\[experimental\]**

Returns a plain
[`Samples`](https://docs.crmpack.org/reference/Samples-class.md) object
for a single hierarchical arm, with the original arm-level parameter
names restored.

## Usage

``` r
armSamples(object, arm)

# S4 method for class 'HierarchicalSamples,character'
armSamples(object, arm)
```

## Arguments

- object:

  (`HierarchicalSamples`)\
  hierarchical posterior draws.

- arm:

  (`character`)\
  name of the arm to extract.

## Value

A [`Samples`](https://docs.crmpack.org/reference/Samples-class.md)
object containing only the requested arm's parameters.
