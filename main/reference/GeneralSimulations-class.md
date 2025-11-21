# `GeneralSimulations` @description **\[stable\]** This class captures trial simulations. Here also the random generator state before starting the simulation is saved, in order to be able to reproduce the outcome. For this just use [`set.seed`](https://rdrr.io/r/base/Random.html) with the `seed` as argument before running [`simulate,Design-method`](https://openpharma.github.io/crmPack/reference/simulate-Design-method.md).

`GeneralSimulations` @description **\[stable\]** This class captures
trial simulations. Here also the random generator state before starting
the simulation is saved, in order to be able to reproduce the outcome.
For this just use [`set.seed`](https://rdrr.io/r/base/Random.html) with
the `seed` as argument before running
[`simulate,Design-method`](https://openpharma.github.io/crmPack/reference/simulate-Design-method.md).

## Usage

``` r
GeneralSimulations(data, doses, seed)

.DefaultGeneralSimulations()
```

## Arguments

- data:

  (`list`)  
  see slot definition.

- doses:

  (`numeric`)  
  see slot definition.

- seed:

  (`integer`)  
  see slot definition.

## Slots

- `data`:

  (`list`)  
  produced
  [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
  objects.

- `doses`:

  (`numeric`)  
  final dose recommendations.

- `seed`:

  (`integer`)  
  random generator state before starting the simulation.

## Note

Typically, end users will not use the `.DefaultGeneralSimulations()`
function.

## Examples

``` r
data <- list(
  Data(x = 1:3, y = c(0, 1, 0), doseGrid = 1:3, ID = 1L:3L, cohort = 1L:3L),
  Data(x = 4:6, y = c(0, 1, 0), doseGrid = 4:6, ID = 1L:3L, cohort = 1L:3L)
)

doses <- c(1, 2)

seed <- 123L

simulations <- GeneralSimulations(data, doses, seed)
```
