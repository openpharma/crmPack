# Determine the Maximum Possible Next Dose

**\[stable\]**

This function determines the upper limit of the next dose based on the
`increments`and the `data`.

## Usage

``` r
maxDose(increments, data, ...)

# S4 method for class 'IncrementsComboOneDrugOnly,DataCombo'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsComboCartesian,DataCombo'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsRelative,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsRelativeDLT,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsRelativeDLTCurrent,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsRelativeParts,DataParts'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsDoseLevels,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsHSRBeta,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsMin,Data'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsMin,DataCombo'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsMin,DataOrdinal'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsOrdinal,DataOrdinal'
maxDose(increments, data, ...)

# S4 method for class 'IncrementsMaxToxProb,DataOrdinal'
maxDose(increments, data, model, samples, ...)

# S4 method for class 'IncrementsMaxToxProb,Data'
maxDose(increments, data, model, samples, ...)
```

## Arguments

- increments:

  (`Increments`)\
  the rule for the next best dose.

- data:

  (`Data`)\
  input data.

- ...:

  additional arguments without method dispatch.

- model:

  (`GeneralModel`)\
  The model on which probabilities will be based

- samples:

  (`Samples`)\
  The MCMC samples to which `model` will be applied

## Value

A `number`, the maximum possible next dose.

## Functions

- `maxDose(increments = IncrementsComboOneDrugOnly, data = DataCombo)`:
  determine the maximum possible next dose levels for a two drug
  combination, based on the rule that only one drug can be escalated at
  a time.

- `maxDose(increments = IncrementsComboCartesian, data = DataCombo)`:
  determine the maximum possible next dose levels for a two drug
  combination, based on the drug specific increment rules which are
  applied independently.

- `maxDose(increments = IncrementsRelative, data = Data)`: determine the
  maximum possible next dose based on relative increments.

- `maxDose(increments = IncrementsRelativeDLT, data = Data)`: determine
  the maximum possible next dose based on relative increments determined
  by DLTs so far.

- `maxDose(increments = IncrementsRelativeDLTCurrent, data = Data)`:
  determine the maximum possible next dose based on relative increments
  determined by DLTs in the current cohort.

- `maxDose(increments = IncrementsRelativeParts, data = DataParts)`:
  determine the maximum possible next dose based on relative increments
  as well as part 1 and beginning of part 2.

- `maxDose(increments = IncrementsDoseLevels, data = Data)`: determine
  the maximum possible next dose based on the number of dose grid
  levels. That is, the max dose is determined as the one which level is
  equal to: base dose level + level increment. The base dose level is
  the level of the last dose in grid or the level of the maximum dose
  applied, which is defined in `increments` object. Find out more in
  [`IncrementsDoseLevels`](https://docs.crmpack.org/reference/IncrementsDoseLevels-class.md).

- `maxDose(increments = IncrementsHSRBeta, data = Data)`: determine the
  maximum possible next dose for escalation.

- `maxDose(increments = IncrementsMin, data = Data)`: determine the
  maximum possible next dose based on multiple increment rules, taking
  the minimum across individual increments.

- `maxDose(increments = IncrementsMin, data = DataCombo)`: determine the
  maximum possible next dose based on multiple increment rules, taking
  the minimum across individual increments.

- `maxDose(increments = IncrementsMin, data = DataOrdinal)`: determine
  the maximum possible next dose based on multiple increment rules,
  taking the minimum across individual increments.

- `maxDose(increments = IncrementsOrdinal, data = DataOrdinal)`:
  determine the maximum possible next dose in an ordinal CRM trial

- `maxDose(increments = IncrementsMaxToxProb, data = DataOrdinal)`:
  determine the maximum possible next dose based on the probability of
  toxicity

- `maxDose(increments = IncrementsMaxToxProb, data = Data)`: determine
  the maximum possible next dose based on the probability of toxicity

## Examples

``` r
# Example of usage for `IncrementsComboOneDrugOnly` maxDose method.

# Create two-drug combination data where the last cohort received
# drug1 = 10, drug2 = 20.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10),
    drug2 = c(20, 20, 20)
  ),
  y = c(0L, 0L, 0L),
  ID = 1L:3L,
  cohort = c(1L, 1L, 1L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

# Define the one-drug-only escalation rule.
my_increments <- IncrementsComboOneDrugOnly()

# Determine the maximum allowed dose combination.
# For drug1 = 10 (not escalated): drug2 may go up to Inf (unrestricted here).
# For drug1 = 20 or 30 (escalated): drug2 is capped at the last drug2 dose (20).
maxDose(my_increments, my_data)
#>      dose_grid_one dose_for_second_drug
#> [1,]            10                  Inf
#> [2,]            20                   20
#> [3,]            30                   20
# Example of usage for `IncrementsComboCartesian` maxDose method.

# Create two-drug combination data where the last cohort received
# drug1 = 10, drug2 = 20.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10),
    drug2 = c(20, 20, 20)
  ),
  y = c(0L, 0L, 0L),
  ID = 1L:3L,
  cohort = c(1L, 1L, 1L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

# Define independent increment rules for each drug.
my_increments <- IncrementsComboCartesian(
  drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
  drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
)

# Determine the maximum allowed next dose levels.
# Here, the drug1 rule allows escalation up to 20 and the drug2 rule allows
# escalation up to 40. For drug1 values above 20, the combination is not
# admissible and is represented with NA for drug2.
maxDose(my_increments, my_data)
#>      dose_grid_one dose_for_second_drug
#> [1,]            10                   40
#> [2,]            20                   40
#> [3,]            30                   NA
# Example of usage for `IncrementsRelative` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:40)
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if the last dose was below 20,
#  - increasing the dose by 33% of the last dose, only if the last dose was
#    above or equal to 20.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Based on the rule above, the maximum dose allowed is:
max_dose <- maxDose(my_increments, data = my_data)
# Example of usage for `IncrementsRelativeDLT` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if no DLTs were yet observed,
#  - increasing the dose by 33% if 1 or 2 DLTs were already observed,
#  - increasing the dose by 20% if at least 3 DLTs were already observed.
my_increments <- IncrementsRelativeDLT(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# Based on the rule above, the maximum dose allowed is:
max_dose <- maxDose(my_increments, data = my_data)
# Example of usage for `IncrementsRelativeDLTCurrent` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Define a rule for dose increments which allows for:
#  - doubling the dose if no DLTs were observed in current (i.e. last) cohort,
#  - only increasing the dose by 33% if 1 or 2 DLTs were observed in current cohort,
#  - only increasing the dose by 20% if at least 3 DLTs were observed in current cohort.
my_increments <- IncrementsRelativeDLTCurrent(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# Based on the rule above, the maximum dose allowed is:
max_dose <- maxDose(my_increments, data = my_data)
# Example of usage for `IncrementsRelativeParts` maxDose class.

# Create an object of class `DataParts`.
my_data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  ID = 1:3,
  cohort = 1:3,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 10),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
)

my_increments <- IncrementsRelativeParts(
  dlt_start = 0,
  clean_start = 1
)

max_dose <- maxDose(my_increments, data = my_data)
# Example of usage for `IncrementsDoseLevels` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 12, 12, 12, 16, 16, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1),
  ID = 1:14,
  cohort = c(1, 2, 3, 4, 5, 6, 6, 7, 7, 7, 8, 8, 9, 9),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:30)
)

# In this first example we define a rule for dose increments which allows for
# maximum skip one dose level, that is 2 dose levels higher than the last dose
# given.
my_increments_1 <- IncrementsDoseLevels(levels = 2, basis_level = "last")

# Based on the rule above, the maximum dose allowed is:
max_dose_1 <- maxDose(my_increments_1, data = my_data)

# In this second example we define a rule for dose increments which allows for
# maximum skip one dose level, that is 2 dose levels higher than the max dose
# given.
my_increments_2 <- IncrementsDoseLevels(levels = 2, basis_level = "max")

# Based on the rule above, the maximum dose allowed is:
max_dose_2 <- maxDose(my_increments_2, data = my_data)
# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8, 6, 6, 6),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# In this example we define a rule for dose increments that limits the further
# dose escalation to doses below 6, because dose 6 is above the probability
# toxicity threshold.
my_increments <- IncrementsHSRBeta(target = 0.3, prob = 0.95)

# Based on the rule above, we then calculate the maximum dose allowed.
my_next_max_dose <- maxDose(my_increments, data = my_data)
# Example of usage for `IncrementsRelativeDLTCurrent` maxDose class.

# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 8, 8, 8),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, 8, 10:80)
)

# Here, we combine two different increment rules.

# The first rule allows for:
#  - doubling the dose if no DLTs were observed at the current dose,
#  - increasing the dose by 33% if 1 or 2 DLTs were observed at the current dose,
#  - increasing the dose by 22% if 3 or more DLTs were observed.
my_increments_1 <- IncrementsRelativeDLT(
  intervals = c(0, 1, 3),
  increments = c(1, 0.33, 0.2)
)

# The second rule allows for:
#  - doubling the dose if the current dose is <20,
#  - increasing the dose by 33% if the current dose is >=20.
my_increments_2 <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Finally, the maximum dose allowed is computed by taking the minimum dose from
# the maximum doses computed by the two rules.
my_increments <- IncrementsMin(
  increments_list = list(my_increments_1, my_increments_2)
)
max_dose <- maxDose(my_increments, my_data)
# Example of usage for `IncrementsMin` maxDose with DataCombo.

# Create two-drug combination data where the last cohort received
# drug1 = 10, drug2 = 20.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10),
    drug2 = c(20, 20, 20)
  ),
  y = c(0L, 0L, 0L),
  ID = 1L:3L,
  cohort = c(1L, 1L, 1L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

# Rule 1: only one drug can be escalated at a time.
rule_one <- IncrementsComboOneDrugOnly()

# Rule 2: independent Cartesian increments for each drug.
rule_two <- IncrementsComboCartesian(
  drug1 = IncrementsRelative(intervals = c(0), increments = c(2)),
  drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
)

# Combine both rules and take the most conservative allowed dose per row.
my_increments <- IncrementsMin(increments_list = list(rule_one, rule_two))
maxDose(my_increments, my_data)
#>      first_column min_second_column
#> [1,]           10                40
#> [2,]           20                20
#> [3,]           30                20
maxDose(
  increments = IncrementsOrdinal(2L, .DefaultIncrementsRelative()),
  data = .DefaultDataOrdinal()
)
#> [1] 79.8
model <- LogisticLogNormalOrdinal(
  mean = c(0.25, 0.15, 0.5),
  cov = matrix(c(1.5, 0, 0, 0, 2, 0, 0, 0, 1), nrow = 3),
  ref_dose = 30
)

emptyData <- DataOrdinal(
  doseGrid = c(1, 3, 9, 25, 50, 75, 100),
  yCategories = c("No tox" = 0L, "DLAE" = 1L, "CRS" = 2L)
)

# For warning regarding tox, see issue #748 https://github.com/openpharma/crmPack/issues/748
suppressWarnings({
  samples <- mcmc(emptyData, model, .DefaultMcmcOptions())
})
toxIncrements <- IncrementsMaxToxProb(prob = c("DLAE" = 0.2, "CRS" = 0.05))
maxDose(toxIncrements, emptyData, model, samples)
#> [1] 1
model <- LogisticLogNormalOrdinal(
  mean = c(0.25, 0.15, 0.5),
  cov = matrix(c(1.5, 0, 0, 0, 2, 0, 0, 0, 1), nrow = 3),
  ref_dose = 30
)

emptyData <- DataOrdinal(
  doseGrid = c(1, 3, 9, 25, 50, 75, 100),
  yCategories = c("No tox" = 0L, "DLAE" = 1L, "CRS" = 2L)
)

# For warning regarding tox, see issue #748 https://github.com/openpharma/crmPack/issues/748
suppressWarnings({
  samples <- mcmc(emptyData, model, .DefaultMcmcOptions())
})
toxIncrements <- IncrementsMaxToxProb(prob = c("DLAE" = 0.2, "CRS" = 0.05))
maxDose(toxIncrements, emptyData, model, samples)
#> [1] 1
```
