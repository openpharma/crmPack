# `IncrementsComboCartesian`

**\[experimental\]**

`IncrementsComboCartesian` is the class for increments control in
combination trials where separate increment rules are defined for each
drug and applied independently.

## Usage

``` r
IncrementsComboCartesian(drug1, drug2)

.DefaultIncrementsComboCartesian()
```

## Arguments

- drug1:

  (`Increments`) see slot definition.

- drug2:

  (`Increments`) see slot definition.

## Details

Under this rule, the maximum admissible dose for each drug is first
computed from the corresponding one-dimensional increment rule. The
resulting two-dimensional admissible region is then represented in a
matrix form compatible with combination `maxDose` methods.

## Slots

- `drug1`:

  (`Increments`) increment rule for the first drug.

- `drug2`:

  (`Increments`) increment rule for the second drug.

## Note

Typically, end users will not use the
`.DefaultIncrementsComboCartesian()` function.

## Examples

``` r
# Create independent increment rules for each drug in a two-drug combination
# trial.
drug1_rule <- IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))
drug2_rule <- IncrementsRelative(intervals = c(0, 20), increments = c(1, 0.33))

# Combine the one-dimensional rules into a Cartesian combination rule.
my_increments <- IncrementsComboCartesian(
  drug1 = drug1_rule,
  drug2 = drug2_rule
)
```
