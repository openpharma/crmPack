# `IncrementsComboOneDrugOnly`

**\[stable\]**

`IncrementsComboOneDrugOnly` is the class for increments control in
combination trials when only one drug is allowed to be escalated.

## Usage

``` r
IncrementsComboOneDrugOnly()

.DefaultIncrementsComboOneDrugOnly()
```

## Details

Under this rule, when escalating drug 1 to a dose above the last
administered drug 1 dose, drug 2 must remain at or below its last
administered dose, and vice versa. In other words, at most one drug may
be escalated in any given cohort. No parameters are required; the rule
is fully defined by the "one drug only" principle.

## Note

Typically, end users will not use the
`.DefaultIncrementsComboOneDrugOnly()` function.

## Examples

``` r
# Create a rule that allows escalation of only one drug at a time in a
# two-drug combination trial.
my_increments <- IncrementsComboOneDrugOnly()
```
