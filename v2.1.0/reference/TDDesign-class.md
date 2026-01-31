# `TDDesign`

**\[stable\]**

`TDDesign` is the class of design based only on DLT responses using
[`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
class model (i.e.
[`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md))
without MCMC samples.

## Usage

``` r
TDDesign(
  model,
  stopping,
  increments,
  pl_cohort_size = CohortSizeConst(0L),
  ...
)

.DefaultTDDesign()
```

## Arguments

- model:

  (`ModelTox`)  
  see slot definition.

- stopping:

  (`Stopping`)  
  see slot definition.

- increments:

  (`Increments`)  
  see slot definition.

- pl_cohort_size:

  (`CohortSize`)  
  see slot definition.

- ...:

  Arguments passed on to
  [`RuleDesign`](https://openpharma.github.io/crmPack/reference/RuleDesign-class.md)

  `nextBest`

  :   (`NextBest`)  
      see slot definition.

  `cohort_size`

  :   (`CohortSize`)  
      see slot definition.

  `data`

  :   (`Data`)  
      see slot definition.

  `startingDose`

  :   (`number`)  
      see slot definition.

## Slots

- `model`:

  (`ModelTox`)  
  the pseudo DLT model to be used.

- `stopping`:

  (`Stopping`)  
  stopping rule(s) for the trial.

- `increments`:

  (`Increments`)  
  how to control increments between dose levels.

- `pl_cohort_size`:

  (`CohortSize`)  
  rules for the cohort sizes for placebo, if any planned (defaults to
  constant 0 placebo patients).

## Note

Typically, end users will not use the `.DefaultTDDesign()` function.

## Examples

``` r
empty_data <- Data(doseGrid = seq(25, 300, 25))

my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = empty_data
)

# The escalation rule.
my_next_best <- NextBestTD(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

my_size <- CohortSizeConst(size = 3)

# The increments for the dose-escalation process:
#  the maximum increase of 200% for doses up to the maximum dose in grid,
#  the maximum increase of 200% for dose above the maximum dose in grid.
my_increments <- IncrementsRelative(
  intervals = range(empty_data@doseGrid),
  increments = c(2, 2)
)

# Stop when the maximum sample size of 36 patients is reached.
my_stopping <- StoppingMinPatients(nPatients = 36) | StoppingMissingDose()

# The design with all the above information and starting with a dose of 25.
# This design incorporates only DLT responses and no DLT samples are involved
# during the simulation.
design <- TDDesign(
  model = my_model,
  stopping = my_stopping,
  increments = my_increments,
  nextBest = my_next_best,
  cohort_size = my_size,
  data = empty_data,
  startingDose = 25
)
```
