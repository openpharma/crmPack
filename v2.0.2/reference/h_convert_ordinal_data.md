# Convert a Ordinal Data to the Equivalent Binary Data for a Specific Grade

**\[experimental\]**

A simple helper function that takes a
[`DataOrdinal`](https://openpharma.github.io/crmPack/reference/DataOrdinal-class.md)
object and an integer grade and converts them to the equivalent `Data`
object.

## Usage

``` r
h_convert_ordinal_data(data_ord, grade)
```

## Arguments

- data_ord:

  (`DataOrdinal`)  
  the `DataOrdinal` object to covert

- grade:

  (`integer`)  
  the toxicity grade for which the equivalent data is required.

## Value

A [`Data`](https://openpharma.github.io/crmPack/reference/Data-class.md)
object.
