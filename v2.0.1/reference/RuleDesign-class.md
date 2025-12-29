# `RuleDesign`

**\[stable\]**

`RuleDesign` is the class for rule-based designs. The difference between
this class and the
[`Design`](https://openpharma.github.io/crmPack/reference/Design-class.md)
class is that `RuleDesign` does not contain `model`, `stopping` and
`increments` slots.

## Usage

``` r
RuleDesign(nextBest, cohort_size, data, startingDose)

.DefaultRuleDesign()

ThreePlusThreeDesign(doseGrid)
```

## Arguments

- nextBest:

  (`NextBest`)  
  see slot definition.

- cohort_size:

  (`CohortSize`)  
  see slot definition.

- data:

  (`Data`)  
  see slot definition.

- startingDose:

  (`number`)  
  see slot definition.

- doseGrid:

  (`numeric`)  
  the dose grid to be used (sorted).

## Functions

- `ThreePlusThreeDesign()`: creates a new 3+3 design object from a dose
  grid.

## Slots

- `nextBest`:

  (`NextBest`)  
  how to find the next best dose.

- `cohort_size`:

  (`CohortSize`)  
  rules for the cohort sizes.

- `data`:

  (`Data`)  
  specifies dose grid, any previous data, etc.

- `startingDose`:

  (`number`)  
  the starting dose, it must lie on the dose grid in `data`.

## Note

Typically, end users will not use the `.DefaultRuleDesign()` function.

## Examples

``` r
# Specify the design to run simulations. The design comprises a model,
# the escalation rule, starting data, a cohort size and a starting dose.

# Initialing a 3+3 design with constant cohort size of 3 and starting dose equal 5.
my_design <- RuleDesign(
  nextBest = NextBestThreePlusThree(),
  cohort_size = CohortSizeConst(size = 3L),
  data = Data(doseGrid = c(5, 10, 15, 25, 35, 50, 80)),
  startingDose = 5
)
# Initialing a 3+3 design with constant cohort size of 3 and starting dose equal 8.
my_design <- ThreePlusThreeDesign(doseGrid = c(8, 10, 15, 25, 35, 50, 80))
```
