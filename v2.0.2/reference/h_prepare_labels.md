# Check That Labels Are Valid and Useful

A vector of labels is valid and useful if it is of length 2, of type
character and its values are distinct.

## Usage

``` r
h_prepare_labels(x)
```

## Arguments

- x:

  (`character`)  
  The vector to be checked

## Value

a character vector of length 2 whose values are distinct

## Details

If `x` is a scalar, a second element is added, whose value is the value
of the scalar with "s" appended. If `x` is `"toxicity"`, the plural is
handled appropriately.
