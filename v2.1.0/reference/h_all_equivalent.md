# Comparison with Numerical Tolerance and Without Name Comparison

**\[experimental\]**

This helper function ensures a default tolerance level equal to `1e-10`,
and ignores names and other attributes. In contrast to
[`all.equal()`](https://rdrr.io/r/base/all.equal.html), it always
returns a logical type object.

## Usage

``` r
h_all_equivalent(target, current, tolerance = 1e-10)
```

## Arguments

- target:

  (`numeric`)  
  target values.

- current:

  (`numeric`)  
  current values.

- tolerance:

  (`number`) relative differences smaller than this are not reported.

## Value

`TRUE` when `target` and `current` do not differ up to desired tolerance
and without looking at names or other attributes, `FALSE` otherwise.
