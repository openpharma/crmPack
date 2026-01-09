# `DADesign`

**\[stable\]**

This class has special requirements for the `model` and `data` slots in
comparison to the parent class
[`Design`](https://openpharma.github.io/crmPack/reference/Design-class.md):

## Usage

``` r
DADesign(model, data, safetyWindow, ...)

.DefaultDADesign()
```

## Arguments

- model:

  (`GeneralModel`)  
  see slot definition.

- data:

  (`DataDA`)  
  see slot definition.

- safetyWindow:

  (`SafetyWindow`)  
  see slot definition.

- ...:

  Arguments passed on to
  [`Design`](https://openpharma.github.io/crmPack/reference/Design-class.md)

  `stopping`

  :   (`Stopping`)  
      see slot definition.

  `increments`

  :   (`Increments`)  
      see slot definition.

  `pl_cohort_size`

  :   (`CohortSize`)  
      see slot definition.

## Details

The `safetyWindow` slot should be an instance of the `SafetyWindow`
class. It can be customized to specify the duration of the safety window
for your trial. The safety window represents the time period required to
observe toxicity data from the ongoing cohort before opening the next
cohort. Note that even after opening the next cohort, further toxicity
data will be collected and analyzed to make dose escalation decisions.

To specify a constant safety window, use the `SafetyWindowConst`
constructor. For example:

`mysafetywindow <- SafetyWindowConst(c(6, 2), 10, 20)`

## Slots

- `model`:

  (`GeneralModel`)  
  the model to use, see in particular
  [`DALogisticLogNormal`](https://openpharma.github.io/crmPack/reference/DALogisticLogNormal-class.md)
  and
  [`TITELogisticLogNormal`](https://openpharma.github.io/crmPack/reference/TITELogisticLogNormal-class.md)
  which make use of the time-to-DLT data.

- `data`:

  (`DataDA`)  
  what is the dose grid, any previous data, etc.

- `safetyWindow`:

  (`SafetyWindow`)  
  the safety window to apply between cohorts.

## Note

Typically, end users will not use the `.DefaultDADesign()` function.

## See also

[`SafetyWindowConst`](https://openpharma.github.io/crmPack/reference/SafetyWindowConst-class.md)
for creating a constant safety window.

## Examples

``` r
empty_data <- DataDA(
  doseGrid = c(
    0.1,
    0.5,
    1,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  ),
  Tmax = 60
)

npiece <- 10
t_max <- 60

lambda_prior <- function(k) {
  npiece / (t_max * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece,
  l = as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece), 2, lambda_prior))),
  c_par = 2
)

my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)

my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

my_size <- maxSize(my_size1, my_size2)

my_stopping1 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)

my_stopping2 <- StoppingMinPatients(nPatients = 50)

my_stopping <- (my_stopping1 | my_stopping2) | StoppingMissingDose()

my_safety_window <- SafetyWindowConst(c(6, 2), 7, 7)

design <- DADesign(
  model = model,
  increments = my_increments,
  nextBest = my_next_best,
  stopping = my_stopping,
  cohort_size = my_size,
  data = empty_data,
  safetyWindow = my_safety_window,
  startingDose = 3
)
```
