# Group Together Mono and Combo Data

This is only used in the simulation method for `DesignGrouped` to
combine the separately generated data sets from mono and combo arms and
to fit the combined logistic regression model. Hence the ID and cohort
information is not relevant and will be arbitrarily assigned to avoid
problems with the
[`DataGrouped`](https://openpharma.github.io/crmPack/reference/DataGrouped-class.md)
validation.

## Usage

``` r
h_group_data(mono_data, combo_data)
```

## Arguments

- mono_data:

  (`Data`)  
  mono data.

- combo_data:

  (`Data`)  
  combo data.

## Value

A
[`DataGrouped`](https://openpharma.github.io/crmPack/reference/DataGrouped-class.md)
object containing both `mono_data` and `combo_data`, but with arbitrary
ID and cohort slots.
