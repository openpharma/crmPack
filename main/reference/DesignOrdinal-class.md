# `DesignOrdinal`

**\[experimental\]**

`DesignOrdinal` is the class for rule-based ordinal designs. The
difference between this class and its parent
[`RuleDesignOrdinal`](https://openpharma.github.io/crmPack/reference/RuleDesignOrdinal-class.md)
class is that the `DesignOrdinal` class contains additional `model`,
`stopping`, `increments` and `pl_cohort_size` slots.

## Usage

``` r
DesignOrdinal(
  model,
  stopping,
  increments,
  pl_cohort_size = CohortSizeOrdinal(1L, CohortSizeConst(0L)),
  ...
)

.DefaultDesignOrdinal()
```

## Arguments

- model:

  (`LogisticLogNormalOrdinal`)  
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
  [`RuleDesignOrdinal`](https://openpharma.github.io/crmPack/reference/RuleDesignOrdinal-class.md)

  `next_best`

  :   (`NextBestOrdinal`)  
      see slot definition.

  `cohort_size`

  :   (`CohortSize`)  
      see slot definition.

  `data`

  :   (`DataOrdinal`)  
      see slot definition.

  `starting_dose`

  :   (`number`)  
      see slot definition.

## Details

Please note that stopping, increments or cohort size rules need to be
wrapped into the corresponding
[StoppingOrdinal](https://openpharma.github.io/crmPack/reference/StoppingOrdinal-class.md),
[IncrementsOrdinal](https://openpharma.github.io/crmPack/reference/IncrementsOrdinal-class.md)
or
[CohortSizeOrdinal](https://openpharma.github.io/crmPack/reference/CohortSizeOrdinal-class.md)
classes, before a successful evaluation of the corresponding methods can
take place. Note also that these wrappers cannot be nested, i.e., you
cannot have an
[IncrementsOrdinal](https://openpharma.github.io/crmPack/reference/IncrementsOrdinal-class.md)
inside another
[IncrementsOrdinal](https://openpharma.github.io/crmPack/reference/IncrementsOrdinal-class.md)
(which also would not make sense) because it would not be clear which
event grade to use for the methods calculation. However, multiple rules
can be combined using the operators defined for these classes, e.g.,
`StoppingOrdinal(1L, rule1 & rule2) | StoppingOrdinal(2L, rule3)`.

## Slots

- `model`:

  (`LogisticLogNormalOrdinal`)  
  the model to be used.

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

Typically, end users will not use the `.DefaultDesignOrdinal()`
function.

## Examples

``` r
my_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
my_size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)
my_size <- CohortSizeOrdinal(1L, maxSize(my_size1, my_size2))

my_stopping1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- StoppingOrdinal(
  1L,
  (my_stopping1 & my_stopping2) | my_stopping3 | StoppingMissingDose()
)

# Initialize the design.
design <- DesignOrdinal(
  model = LogisticLogNormalOrdinal(
    mean = c(-3, -4, 1),
    cov = diag(c(3, 4, 1)),
    ref_dose = 50
  ),
  next_best = NextBestOrdinal(
    1L,
    NextBestNCRM(
      target = c(0.2, 0.35),
      overdose = c(0.35, 1),
      max_overdose_prob = 0.25
    )
  ),
  stopping = my_stopping,
  increments = IncrementsOrdinal(
    1L,
    IncrementsRelative(
      intervals = c(0, 20),
      increments = c(1, 0.33)
    )
  ),
  cohort_size = my_size,
  data = DataOrdinal(
    doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
    yCategories = c("No tox" = 0L, "Sub-tox AE" = 1L, "DLT" = 2L)
  ),
  starting_dose = 3
)
```
