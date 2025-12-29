# `DualResponsesDesign.R`

**\[stable\]**

This is a class of design based on DLE responses using the
[`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
model without DLE and efficacy samples. It contains all slots from the
[`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md)
and
[`TDsamplesDesign`](https://openpharma.github.io/crmPack/reference/TDsamplesDesign-class.md)
classes.

## Usage

``` r
DualResponsesDesign(eff_model, data, ...)

.DefaultDualResponsesDesign()
```

## Arguments

- eff_model:

  (`ModelEff`)  
  see slot definition.

- data:

  (`DataDual`)  
  see slot definition.

- ...:

  Arguments passed on to
  [`TDDesign`](https://openpharma.github.io/crmPack/reference/TDDesign-class.md)

  `model`

  :   (`ModelTox`)  
      see slot definition.

  `stopping`

  :   (`Stopping`)  
      see slot definition.

  `increments`

  :   (`Increments`)  
      see slot definition.

  `pl_cohort_size`

  :   (`CohortSize`)  
      see slot definition.

## Slots

- `data`:

  (`DataDual`)  
  the data set.

- `eff_model`:

  (`ModelEff`)  
  the pseudo efficacy model to be used.

## Note

Typically, end users will not use the `.DefaultDualResponsesDesign()`
function.

## Examples

``` r
empty_data <- DataDual(doseGrid = seq(25, 300, 25))

tox_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = empty_data
)

eff_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = empty_data
)

my_next_best <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

my_increments <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)

my_size <- CohortSizeConst(size = 3)
my_stopping <- StoppingMinPatients(nPatients = 36)

design <- DualResponsesDesign(
  nextBest = my_next_best,
  cohort_size = my_size,
  startingDose = 25,
  model = tox_model,
  eff_model = eff_model,
  data = empty_data,
  stopping = my_stopping,
  increments = my_increments
)
```
