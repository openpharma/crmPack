# Convert a Samples Object from an ordinal Model to the Equivalent Samples Object from a Binary Model

**\[experimental\]**

A simple helper function that converts a
[`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
object from the fit of an ordinal CRM model to that which would have
been obtained from fitting a binary CRM model for toxicities of a
specified grade to the same observed data.

## Usage

``` r
h_convert_ordinal_samples(obj, grade)
```

## Arguments

- obj:

  (`Samples`)  
  the `Samples` object to covert

- grade:

  (`integer`)  
  the toxicity grade for which the equivalent data is required.

## Value

A
[`Samples`](https://openpharma.github.io/crmPack/reference/Samples-class.md)
object.
