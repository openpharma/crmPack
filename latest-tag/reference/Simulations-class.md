# `Simulations`

**\[stable\]**

This class captures the trial simulations from model based designs.
Additional slots `fit`, `stop_reasons`, `stop_report`,`additional_stats`
compared to the general class
[`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md).

## Usage

``` r
Simulations(fit, stop_reasons, stop_report, additional_stats, ...)

.DefaultSimulations()
```

## Arguments

- fit:

  (`list`)  
  see slot definition.

- stop_reasons:

  (`list`)  
  see slot definition.

- stop_report:

  see `Simulations`

- additional_stats:

  (`list`)  
  see slot definition.

- ...:

  additional parameters from
  [`GeneralSimulations`](https://openpharma.github.io/crmPack/reference/GeneralSimulations-class.md)

## Slots

- `fit`:

  (`list`)  
  final fits

- `stop_reasons`:

  (`list`)  
  stopping reasons for each simulation run

- `stop_report`:

  matrix of stopping rule outcomes

- `additional_stats`:

  list of additional statistical summary

## Note

Typically, end users will not use the `.DefaultSimulations()` function.

## Examples

``` r
data <- list(
  Data(
    x = 1:2,
    y = 0:1,
    doseGrid = 1:2,
    ID = 1L:2L,
    cohort = 1L:2L
  ),
  Data(
    x = 3:4,
    y = 0:1,
    doseGrid = 3:4,
    ID = 1L:2L,
    cohort = 1L:2L
  )
)

doses <- c(1, 2)

seed <- as.integer(123)

fit <- list(
  c(0.1, 0.2),
  c(0.3, 0.4)
)

stop_report <- matrix(c(TRUE, FALSE), nrow = 2)

stop_reasons <- list("A", "B")

additional_stats <- list(a = 1, b = 1)

simulations <- Simulations(
  fit = fit,
  stop_report = stop_report,
  stop_reasons = stop_reasons,
  additional_stats = additional_stats,
  data,
  doses,
  seed
)
```
