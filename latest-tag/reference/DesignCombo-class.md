# `DesignCombo`

**\[experimental\]**

`DesignCombo` is the class for two-drug combination dose-escalation
designs.

## Usage

``` r
DesignCombo(
  model,
  nextBest,
  stopping,
  increments,
  cohort_size,
  data,
  startingDose,
  backfill = Backfill(opening = OpeningNone())
)

.DefaultDesignCombo()
```

## Arguments

- model:

  (`TwoDrugsCombo`) see slot definition.

- nextBest:

  (`NextBestNCRM`) see slot definition.

- stopping:

  (`Stopping`) see slot definition.

- increments:

  (`Increments`) see slot definition.

- cohort_size:

  (`CohortSize`) see slot definition.

- data:

  (`DataCombo`) see slot definition.

- startingDose:

  (`numeric`) see slot definition.

- backfill:

  (`Backfill`) see slot definition.

## Details

This class stores the same core design components as
[`Design`](https://docs.crmpack.org/reference/Design-class.md), but uses
[`DataCombo`](https://docs.crmpack.org/reference/DataCombo-class.md) and
[`TwoDrugsCombo`](https://docs.crmpack.org/reference/TwoDrugsCombo-class.md)
to represent two-drug combinations.

## Slots

- `model`:

  (`TwoDrugsCombo`) the model to be used.

- `nextBest`:

  (`NextBestNCRM`) the rule to select the next dose combination.

- `stopping`:

  (`Stopping`) stopping rule(s) for the trial.

- `increments`:

  (`Increments`) how to control increments between dose combinations.

- `cohort_size`:

  (`CohortSize`) rules for the cohort sizes.

- `data`:

  (`DataCombo`) specifies dose grids and any previously observed data.

- `startingDose`:

  (`numeric`) starting dose combination as a numeric vector of length 2.

- `backfill`:

  (`Backfill`) rules for backfilling patients in the trial.

## Note

Typically, end users will not use the `.DefaultDesignCombo()` function.

## Examples

``` r
empty_data <- DataCombo(
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

my_model <- TwoDrugsCombo(
  single_models = list(
    drug1 = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    drug2 = LogisticLogNormal(
      mean = c(-0.7, 0.8),
      cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
      ref_dose = 20
    )
  ),
  gamma = 0,
  tau = 1
)

my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

my_increments <- IncrementsMin(
  increments_list = list(
    IncrementsComboOneDrugOnly(),
    IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    )
  )
)

my_stopping <- StoppingMinPatients(nPatients = 20)

design_combo <- DesignCombo(
  model = my_model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = CohortSizeConst(3L),
  data = empty_data,
  startingDose = c(drug1 = 10, drug2 = 20)
)
```
