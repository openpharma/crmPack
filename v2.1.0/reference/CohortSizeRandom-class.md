# `CohortSizeRandom`

**\[experimental\]**

`CohortSizeRandom` is the class for random cohort sizes drawn from a
uniform distribution between `min_size` and `max_size` (inclusive).

## Usage

``` r
CohortSizeRandom(min_size, max_size)

.DefaultCohortSizeRandom()
```

## Arguments

- min_size:

  (`integer`)  
  see slot definition.

- max_size:

  (`integer`)  
  see slot definition.

## Slots

- `min_size`:

  (`integer`)  
  minimum cohort size.

- `max_size`:

  (`integer`)  
  maximum cohort size.

## Note

Typically, end users will not use the `.DefaultCohortSizeRandom()`
function.

## Examples

``` r
# Random cohort size between 1 and 5.
my_size <- CohortSizeRandom(min_size = 1, max_size = 5)
```
