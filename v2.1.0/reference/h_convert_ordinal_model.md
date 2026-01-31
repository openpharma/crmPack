# Convert an ordinal CRM model to the Equivalent Binary CRM Model for a Specific Grade

**\[experimental\]**

A simple helper function that takes a
[`LogisticLogNormalOrdinal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormalOrdinal-class.md)
and an integer grade and converts them to the equivalent
`LogisticLogNormal` model.

## Usage

``` r
h_convert_ordinal_model(x, grade)
```

## Arguments

- x:

  (`LogisticLogNormalOrdinal`)  
  the `LogisticLogNormalOrdinal` model to covert

- grade:

  (`integer`)  
  the toxicity grade for which the equivalent model is required.

## Value

A
[`LogisticLogNormal`](https://openpharma.github.io/crmPack/reference/LogisticLogNormal-class.md)
model.
