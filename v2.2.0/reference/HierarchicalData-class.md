# `HierarchicalData`

**\[experimental\]**

`HierarchicalData` is a class for hierarchical designs, storing the data
for all arms together.

## Usage

``` r
HierarchicalData(...)

.DefaultHierarchicalData()
```

## Arguments

- ...:

  named `Data` objects, one for each arm in the trial. The argument
  names are used as the arm names.

## Slots

- `arms`:

  (`list`)\
  a named list of `Data` objects, one for each arm in the trial. The
  names of the list are the arm names.

## Note

Typically, end users will not use the `.DefaultHierarchicalData()`
function directly.

## Examples

``` r
dat_hierarchical <- HierarchicalData(
  mono = .DefaultData(),
  combo = .DefaultDataCombo()
)
```
